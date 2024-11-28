## -----------------------------------------------------------------------------
##
## Runner to setup projections
##
## -----------------------------------------------------------------------------

## Directory of model to project
base_dir <- 'benchmarks/WKBDEEP/gadget3'
vers <- 'models/04-baseline_Linf0_K0_t01_penrec1_sigma0.4_agedata0_bmtS0_llnS1_recl1'

outpath <- file.path(base_dir, vers, 'PROJ')
if (!dir.exists(outpath)) dir.create(outpath)

## Load the fit and bootstrap
load(file = file.path(base_dir, vers, 'WGTS', 'fit.Rdata'))
load(file = file.path(base_dir, vers, 'BOOTSTRAP_SHORT', 'boot_fit.Rdata'))
## Load the model and parameters
load(file = file.path(base_dir, vers, 'tmb_model.Rdata'))
params_final <- fit$params

## -----------------------------------------------------------------------------
## Stock and fleets
## -----------------------------------------------------------------------------

year_range <- c(min(fit$res.by.year$year), max(fit$res.by.year$year)-1)
end_year <- max(year_range) 
start_year <- end_year + 1
num_steps <- length(unique(fit$stock.full$step))

rec_years <- 2001:2022
harvest_rates <- 0.1#seq(0.00, 0.8, by = 0.01)
hr_trials <- 1

## -----------------------------------------------------------------------------

## Blim
fit$res.by.year %>% 
  filter(stock == 'bli_mat',
         year > 2000) %>% 
  dplyr::select(year, ssb = total.biomass) %>% 
  left_join(fit$res.by.year %>% 
              filter(stock == 'bli_imm') %>% 
              dplyr::select(year, recruitment), by = 'year') %>% 
  arrange(year) %>%
  mutate(r1 = lead(recruitment, n = 5)) %>% 
  #filter(year > 1999) %>% 
  ggplot(aes(ssb/1e6, r1/1e6)) +  
  expand_limits(y = 0, x = 0) + 
  labs(x='SSB (in kt)',y='Recruitment (millions)') +
  #geom_vline(xintercept = local(refPoints$blim)/1e3, lty = 'dashed', col = 'red') + 
  geom_text(aes(label = substring(year, 3, 4))) 



## Median SSB for 2000:2005 (see TR)
blim <- round(fit$res.by.year %>% filter(stock == 'bli_mat', year == 2001) %>% pull(total.biomass)/1e3)
bpa  <- blim * exp(1.645 * 0.2)
btrigger <- bpa

## -----------------------------------------------------------------------------

schedule <- 
  expand.grid(year = (end_year - recruitment_age + 1):(end_year + num_project_years),
              step = recstep)

## Gather all the parameter templates
param_list <- c(list(base = fit$params), 
                boot_fit %>% 
                  discard(~class(.)[1] == 'try-error') %>% 
                  map('params')) 

## Gather all the recruitment time-series
rec_list <- c(list(base = fit$stock.recruitment), 
              boot_fit %>% 
                discard(~class(.)[1] == 'try-error') %>% 
                map('stock.recruitment')) %>% 
  map(filter, recruitment > 0, 
      year %in% rec_years)

## Fleet proportions

fleet_props <- 
  fit$fleet.info %>% 
  filter(fleet != 'foreign') %>% 
  group_by(year,fleet) %>% 
  summarise(harv.rate = mean(harv.rate,na.rm=TRUE)) %>% 
  filter(!is.infinite(harv.rate)) %>% 
  group_by(year) %>% 
  mutate(m = harv.rate/sum(harv.rate)) %>% 
  filter(year > 2016,year < 2024) %>% 
  group_by(fleet) %>% 
  summarise(m=mean(m,na.rm=TRUE)) %>% 
  {set_names(as.list(.$m),.$fleet)}



## -----------------------------------------------------------------------------
## Initial parameters and adfun
## Update values
tmp <- params_final %>% filter(switch != 'report_detail')
par.proj <- base.par.proj
par.proj$value[tmp$switch] <- tmp$value
par.proj$value$project_years <- defaults$project_years
par.proj$value$blim <- blim*1e3
par.proj$value$lln_prop <- fleet_props$lln
par.proj$value$bmt_prop <- fleet_props$bmt

## Should need oly one function
par.proj <- 
  par.proj %>% 
  g3p_project_rec(recruitment = fit$stock.recruitment %>% filter(year >= rec_start_year), method = 'bootstrap') %>%
  g3p_project_advice_error(hr_target = min(harvest_rates), advice_cv = 0) %>% 
  g3_init_guess('btrigger', 1) 
  
fun_fun <- g3_tmb_adfun(tmb_proj, par.proj, type = 'Fun')

## Set up the harvest rates and trials-per-harvest-rate
hr_list <- 
  expand.grid(hr = harvest_rates, 
              boot = 1:length(param_list), 
              trial = 1:hr_trials) %>% 
  #expand.grid(hr = 0.1, trial = 1) %>% 
  unite(id, remove = FALSE) %>%
  mutate(id = paste0('h', id)) %>% 
  split(f = as.factor(.$id))

# ## -----------------------------------------------------------------------------
# ## Precautionary reference points (flim): no assessment error and no Btrigger
# ## -----------------------------------------------------------------------------

projpar_pre <- lapply(setNames(names(hr_list), names(hr_list)), function(x){

  par.proj <- base.par.proj
  par.proj$value[param_list[[hr_list[[x]]$boot]]$switch] <- param_list[[hr_list[[x]]$boot]]$value
  par.proj$value$project_years <- num_project_years
  par.proj$value$blim <- blim*1e3
  par.proj$value$lln_prop <- fleet_props$lln
  par.proj$value$bmt_prop <- fleet_props$bmt

  out <-
    par.proj %>%
    g3p_project_rec(recruitment = rec_list[[hr_list[[x]]$boot]], method = 'bootstrap') %>%
    #g3p_project_rec(recruitment = fit$stock.recruitment %>% filter(year >= rec_start_year) %>% summarise(recruitment = mean(recruitment)), method = 'constant') %>%
    g3p_project_advice_error(hr_target = hr_list[[x]]$hr, advice_rho = 0.423, advice_cv = 0) %>%
    g3_init_guess('btrigger', 1)

  return(out)

})



## Project
results_pre <-
  do.call('rbind',
          parallel::mclapply(setNames(names(projpar_pre), names(projpar_pre)), function(x){
            print(x)
            out <- run_proj(fun_fun, projpar_pre[[x]])
            out <- 
              out %>% 
              group_by(year) %>% 
              summarise(catch = sum(catch), ssb = ssb[step == 1], fbar = mean(fbar), rec = max(rec))
            out$hr_target <- as.numeric(gsub('h', '', gsub('(.+)_(.+)_(.+)', '\\1', x)))
            out$boot <- as.numeric(gsub('(.+)_(.+)_(.+)', '\\2', x))
            out$trial <- as.numeric(gsub('(.+)_(.+)_(.+)', '\\3', x))
            return(out)
            }, mc.cores = 50)#parallel::detectCores(logical = TRUE))
          )

save(results_pre, file = file.path(outpath, 'results_pre.Rdata'))
save(projpar_pre, file = file.path(outpath, 'projpar_pre.Rdata'))

## -----------------------------------------------------------------------------
## MSY reference points (fmsy, fp0.5): assessment error and no btrigger
## -----------------------------------------------------------------------------

projpar_msy_nobtrigger <- lapply(setNames(names(hr_list), names(hr_list)), function(x){

  par.proj <- base.par.proj
  par.proj$value[param_list[[hr_list[[x]]$boot]]$switch] <- param_list[[hr_list[[x]]$boot]]$value
  par.proj$value$project_years <- num_project_years
  par.proj$value$blim <- blim*1e3
  par.proj$value$lln_prop <- fleet_props$lln
  par.proj$value$bmt_prop <- fleet_props$bmt

  out <-
    par.proj %>%
    g3p_project_rec(recruitment = rec_list[[hr_list[[x]]$boot]], method = 'bootstrap') %>%
    #g3p_project_rec(recruitment = fit$stock.recruitment %>% filter(year >= rec_start_year) %>% summarise(recruitment = mean(recruitment)), method = 'constant') %>%
    g3p_project_advice_error(hr_target = hr_list[[x]]$hr, advice_rho = 0.423, advice_cv = 0.212) %>%
    g3_init_guess('btrigger', 1)

  return(out)

})


results_msy_nobtrigger <-
  do.call('rbind',
          parallel::mclapply(setNames(names(projpar_msy_nobtrigger), names(projpar_msy_nobtrigger)), function(x){
            print(x)
            out <- run_proj(fun_fun, projpar_msy_nobtrigger[[x]])
            out <- 
              out %>% 
              group_by(year) %>% 
              summarise(catch = sum(catch), ssb = ssb[step == 1], fbar = mean(fbar), rec = max(rec))
            out$hr_target <- as.numeric(gsub('h', '', gsub('(.+)_(.+)_(.+)', '\\1', x)))
            out$boot <- as.numeric(gsub('(.+)_(.+)_(.+)', '\\2', x))
            out$trial <- as.numeric(gsub('(.+)_(.+)_(.+)', '\\3', x))
            return(out)
            }, mc.cores = 50)#parallel::detectCores(logical = TRUE))
          )

save(results_msy_nobtrigger, file = file.path(outpath, 'results_msy_nobtrigger.Rdata'))
save(projpar_msy_nobtrigger, file = file.path(outpath, 'projpar_msy_nobtrigger.Rdata'))

## -----------------------------------------------------------------------------
## MSY reference points (fmsy, fp0.5): assessment error and btrigger
## -----------------------------------------------------------------------------

projpar_msy <- lapply(setNames(names(hr_list), names(hr_list)), function(x){
  
  par.proj <- base.par.proj
  par.proj$value[param_list[[hr_list[[x]]$boot]]$switch] <- param_list[[hr_list[[x]]$boot]]$value
  par.proj$value$project_years <- num_project_years
  par.proj$value$blim <- blim*1e3
  par.proj$value$lln_prop <- fleet_props$lln
  par.proj$value$bmt_prop <- fleet_props$bmt
  
  out <- 
    par.proj %>% 
    g3p_project_rec(recruitment = rec_list[[hr_list[[x]]$boot]], method = 'bootstrap') %>% 
    g3p_project_advice_error(hr_target = hr_list[[x]]$hr, advice_rho = 0.423, advice_cv = 0.212)  %>% 
    g3_init_guess('btrigger', btrigger*1e3) 
  
  return(out)
  
})


results_msy <- 
  do.call('rbind', 
          parallel::mclapply(setNames(names(projpar_msy), names(projpar_msy)), function(x){
            print(x)
            out <- run_proj(fun_fun, projpar_msy[[x]])
            out <- 
              out %>% 
              group_by(year) %>% 
              summarise(catch = sum(catch), ssb = ssb[step == 1], fbar = mean(fbar), rec = max(rec))
            out$hr_target <- as.numeric(gsub('h', '', gsub('(.+)_(.+)_(.+)', '\\1', x)))
            out$boot <- as.numeric(gsub('(.+)_(.+)_(.+)', '\\2', x))
            out$trial <- as.numeric(gsub('(.+)_(.+)_(.+)', '\\3', x))
            return(out)
          }, mc.cores = 50)#parallel::detectCores(logical = TRUE))
  )

save(results_msy, file = file.path(outpath, 'results_msy.Rdata'))
save(projpar_msy, file = file.path(outpath, 'projpar_msy.Rdata'))



## -----------------------------------------------------------------------------

tmp <- results_pre %>% filter(year > 2125, trial == 5, hr_target == 0.51)

results_pre %>%
  filter(year > 2125) %>%
  group_by(year, hr_target, trial) %>%
  summarise(c = sum(catch), ssb = mean(ssb), fbar = mean(fbar)) %>%
  group_by(hr_target) %>%
  summarise(yield = median(c), ssb = median(ssb), fbar = median(fbar)) %>%
  ggplot(aes(fbar, yield)) + geom_point()
# 
results_msy %>%
  filter(year > 2125) %>%
  group_by(year, hr_target, boot, trial) %>%
  summarise(c = sum(catch), ssb = mean(ssb), fbar = median(fbar)) %>%
  group_by(hr_target) %>%
  summarise(yield = median(c), ssb = median(ssb), fbar = median(fbar)) %>%
  ggplot(aes(fbar, yield)) + geom_point()
# 
# results_msy_nobtrigger %>% 
#   filter(year > 2170) %>% 
#   group_by(year, hr_target, boot, trial) %>% 
#   summarise(c = sum(catch), ssb = mean(ssb[step == 1]), fbar = median(fbar[step == 1])) %>% 
#   group_by(hr_target) %>% 
#   summarise(yield = median(c), ssb = median(ssb), fbar = median(fbar)) %>% 
#   ggplot(aes(fbar, yield)) + geom_point()
# 


