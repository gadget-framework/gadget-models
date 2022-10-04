## -----------------------------------------------------------------------------
##
## Runner to build a Gadget3 model for Ling 
##
## -----------------------------------------------------------------------------

library(mfdb)
library(tidyverse)
library(gadget3)
library(Rgadget)
library(parallel)

base_dir <- '06-ling/gadget3'
vers <- 'baseline_randomRec'

source(file.path(base_dir, 'src/stock_param_functions.R'))
source(file.path(base_dir, 'src/step-utils_random.R'))
source(file.path(base_dir, 'src/g3_iterative.R'))
source(file.path(base_dir, 'src/g3_init_guess.R'))
source(file.path(base_dir, 'src/g3_jitter.R'))

## -----------------------------------------------------------------------------
## OPTIONS 
## -----------------------------------------------------------------------------

## Whether or not to call the setup-data scripts
read_data <- FALSE

## Whether or not to run iterative reweighting
run_iterative <- FALSE


## Optimisation mode (param_opt_mode), options: 
# (1) parameters are bounded internally (ie using the bounded function) works with 'BFGS' optim method
# (2) parameters are bounded externally so the optim function must use a box-constrained optimisation method
# (3) global search, all parameters unbounded, unconstrained optimisation can be used (eg 'BFGS')

setup_options <- list(param_opt_mode = 1,
                      initial_abund_mode = 2)

## Whether or not to bound parameters internally
setup_options$bound_params <- ifelse(setup_options$param_opt_mode == 1, TRUE, FALSE)

## -----------------------------------------------------------------------------
## PARAMETERS 
## -----------------------------------------------------------------------------

## Some model parameters...
year_range <- 1982:2021 

## Stock info.
species_name <- "ling"
species_code <- "LIN"

## -----------------------------------------------------------------------------

reitmapping <-
  read.table(
    system.file("demo-data", "reitmapping.tsv", package="mfdb"),
    header=TRUE,
    as.is=TRUE)

defaults <- list(
  area = mfdb::mfdb_group("1" = unique(reitmapping$SUBDIVISION)),
  timestep = mfdb::mfdb_timestep_quarterly,
  year = year_range,
  species = species_code)

# Map area names to integer area numbers (in this case only "1" ==> 1, but could be anything)
areas <- structure(
  seq_along(defaults$area),
  names = names(defaults$area))

# Timekeeping for the model, i.e. how long we run for
time_actions <- list(
  g3a_time(start_year = min(defaults$year),
           end_year = max(defaults$year),
           project_years = 0L,
           defaults$timestep),
  list())


## Data and model folders
fs::dir_create(file.path(base_dir, c('data', 'model')))

## ------------------------------------------------------------------------------------

source(file.path(base_dir, '00-setup_random', 'setup-model.R'))  # Generates mat_stock_actions / imm_stock_actions

if(read_data){
  mdb <- mfdb('Iceland', db_params = list(host = 'mfdb.hafro.is'))
  #  mdb <- mfdb("../../mfdb/copy/iceland.duckdb")
  source(file.path(base_dir, '00-setup_random', 'setup-fleet-data.R'))
  source(file.path(base_dir, '00-setup_random', 'setup-catchdistribution.R'))
  source(file.path(base_dir, '00-setup_random', 'setup-indices.R'))
  source(file.path(base_dir, '00-setup_random', 'setup-initial_parameters.R'))
  
}else{
  fs::dir_ls(file.path(base_dir, 'data')) %>%
    stringr::str_subset('.Rdata') %>%
    lapply(load,.GlobalEnv)
}

##### Configure model actions #################################################

source(file.path(base_dir, '00-setup_random', 'setup-fleets.R'))  # Generates fleet_actions
#if (!run_iterative){ 
#  source(file.path(base_dir, '00-setup_random', 'setup-likelihood_alphabeta.R')) # Generates likelihood_actions (alpha and beta optimized)
#}else{ 
source(file.path(base_dir, '00-setup_random', 'setup-likelihood.R')) # Generates likelihood_actions (alpha and beta estimated by lm)
#}

## RANDOM EFFECTS
source(file.path(base_dir, '00-setup_random', 'setup-randomeffects.R')) 

## Collate actions
actions <- c(
  mat_actions,
  imm_actions,
  fleet_actions,
  likelihood_actions,
  time_actions,
  list(custom_action))

##### Compile the r- and tmb-based models ######################################

# Turn actions into an R function
model <- g3_to_r(actions)

# You can edit the model code with:
#model <- edit(model)

# Turn actions into C++ objective function code
tmb_model <- g3_to_tmb(actions)

# Get the parameter template 
tmb_param <- attr(tmb_model, 'parameter_template')

# Copy initial guesses from R model (just the weights now)
#tmb_param$value <- I(param[rownames(tmb_param)])

# Fill in the parameter template
# NOTE: g3_init_param initialises values at 0 if bounded (for now)
# was running into problems with initial values

tmb_param <- 
  tmb_param %>% 
  g3_init_guess('\\.rec', log(50), log(0.001), log(1000), 1) %>% 
  g3_init_guess('\\.rec.sigma', 0.2, -1, 1, 1)  %>% 
  g3_init_guess('\\.init', 1, 0.001, 1000, 1) %>% 
  g3_init_guess('recl', 12, 1, 20, 1) %>% 
  g3_init_guess('rec.sd', 5, 4, 20, 1) %>% 
  g3_init_guess('rec.scalar', 400, 1, 500, 1) %>% 
  g3_init_guess('init.scalar', 200, 1, 300, 1) %>% 
  g3_init_guess('Linf', 160, 100, 200, 1) %>% 
  g3_init_guess('\\.K', 90, 40, 100, 1) %>% 
  g3_init_guess('bbin', 6, 1e-08, 100, 1) %>% 
  g3_init_guess('\\.alpha', 0.5, 0.01, 3, 1) %>% 
  g3_init_guess('l50', 50, 10, 100, 1) %>% 
  g3_init_guess('init.F', 0.4, 0.1, 1, 1) %>% 
  g3_init_guess('\\.M', 0.15, 0.001, 1, 0) %>% 
  g3_init_guess('prop_mat0', 0.5, 0.1, 0.9, 0) %>%
  g3_init_guess('B0', 100, 1, 5000, 1) %>%
  g3_init_guess('mat1', log(70), log(10), log(200), 1) %>% # g3_init_guess('mat1', 0, 10, 200, 1) %>% 
  g3_init_guess('mat2', mat.l50$l50, 0.75*mat.l50$l50, 1.25*mat.l50$l50, 1) %>% 
  #  g3_init_guess('sigma_alpha', init.sigma.coef[['alpha']], -1, 1, 0) %>%
  #  g3_init_guess('sigma_beta', init.sigma.coef[['beta']], 0, 2, 0) %>%
  #  g3_init_guess('sigma_gamma', init.sigma.coef[['gamma']], 0, 1, 0) %>%
  g3_init_guess('walpha', lw.constants$estimate[1], 1e-10, 1, 0) %>% 
  g3_init_guess('wbeta', lw.constants$estimate[2], 2, 4, 0) 

## Initial sd's
if (any(grepl('\\.init\\.sd', tmb_param$switch))){
  
  tmb_param[grepl('mat\\.init\\.sd', tmb_param$switch), 'value'] <-
    init.sigma %>% filter(age %in% 
                            gadget3:::stock_definition(mat_stock, 'minage'): 
                            gadget3:::stock_definition(mat_stock, 'maxage')) %>% .$ms
  
  tmb_param[grepl('imm\\.init\\.sd', tmb_param$switch), 'value'] <-
    init.sigma %>% filter(age %in% 
                            gadget3:::stock_definition(imm_stock, 'minage'): 
                            gadget3:::stock_definition(imm_stock, 'maxage')) %>% .$ms
  ## Turn off optimisation
  tmb_param <-
    tmb_param %>% 
    mutate(optimise = case_when(grepl('init.sd', switch) ~ FALSE,
                                grepl('.M.[\\.[0-9]', switch) ~ FALSE,
                                TRUE~optimise))
    
}

## Old weights from gadget2
if (!run_iterative){
  
  tmb_param$value['cdist_sumofsquares_ldist_lln_weight'] <- 3200.2609
  tmb_param$value['cdist_sumofsquares_aldist_lln_weight'] <- 2200.6472
  tmb_param$value['cdist_sumofsquares_ldist_bmt_weight'] <- 1319.4544
  tmb_param$value['cdist_sumofsquares_aldist_bmt_weight'] <- 1576.5504
  tmb_param$value['cdist_sumofsquares_ldist_gil_weight'] <- 271.4932
  tmb_param$value['cdist_sumofsquares_aldist_gil_weight'] <- 634.7807
  tmb_param$value['cdist_sumofsquares_ldist_igfs_weight'] <- 7112.2648
  tmb_param$value['cdist_sumofsquares_aldist_igfs_weight'] <- 10853.7620
  tmb_param$value['cdist_sumofsquares_matp_igfs_weight'] <- 9.8200
  tmb_param$value['adist_surveyindices_log_si_igfs_si1_weight'] <- 28.8200
  tmb_param$value['adist_surveyindices_log_si_igfs_si2a_weight'] <- 10.2
  tmb_param$value['adist_surveyindices_log_si_igfs_si2b_weight'] <- 47.24
  tmb_param$value['adist_surveyindices_log_si_igfs_si3a_weight'] <- 48.08
  tmb_param$value['adist_surveyindices_log_si_igfs_si3b_weight'] <- 25.33
  tmb_param$value['adist_surveyindices_log_si_igfs_si3c_weight'] <- 11.69
  tmb_param$value['adist_surveyindices_log_si_igfs_si3d_weight'] <- 13.42
  tmb_param[grepl('si_alpha|si_beta', tmb_param$switch), 'value'] <- 1
  
}

## -----------------------------------------------------------------------------

## Run the R-model
result <- model(tmb_param$value)
result[[1]]

# List all available reports
print(names(attributes(result)))

## -----------------------------------------------------------------------------

if (!run_iterative){
  
  options(warn=2)
  options(error=recover) 
  
  # Compile and generate TMB ADFun (see ?TMB::MakeADFun)
  obj.fun <- g3_tmb_adfun(tmb_model,tmb_param)
  # writeLines(TMB::gdbsource(g3_tmb_adfun(tmb_model, tmb_param, compile_flags = "-g", output_script = TRUE)))
  
  # Run model once, using g3_tmb_par to reshape tmb_param into param vector.
  # Will return nll
  
  obj.fun$fn(g3_tmb_par(tmb_param))
  
  # Run model once, returning model report
  obj.fun$report(g3_tmb_par(tmb_param))
  
  # fit.opt <- nlminb(g3_tmb_par(tmb_param),
  #                   obj.fun$fn, 
  #                   obj.fun$gr,
  #                   upper = g3_tmb_upper(tmb_param),
  #                   lower = g3_tmb_lower(tmb_param),
  #                   control = list(trace = 2, iter.max=1000, rel.tol = .Machine$double.eps))
  
  # Run model through R optimiser, using bounds set in tmb_param
   fit.opt <- optim(g3_tmb_par(tmb_param),
                    obj.fun$fn,
                    obj.fun$gr,
                    method = 'BFGS',
                    control = list(trace = 2,maxit = 1000, reltol = .Machine$double.eps^2))
  
  fit <- gadget3:::g3_fit(model, g3_tmb_relist(tmb_param, fit.opt$par))
  
  fit$stock.std %>% filter(year == 1982, step == 1) %>% group_by(age) %>% 
    ggplot(aes(x = age, y = number, fill = stock)) + geom_bar(stat = "identity", position = "dodge")
  
}else{
  # Compile and generate TMB ADFun (see ?TMB::MakeADFun)
  obj.fun <- g3_tmb_adfun(tmb_model,tmb_param)
  
  ## Run iterative re-weighting
  tmp <- g3_iterative(file.path(base_dir, 'models', vers),
                      model,
                      tmb_model,
                      tmb_param,
                      grouping = list(si1 = c('log_si_igfs_si1',
                                              'log_si_igfs_si2a',
                                              'log_si_igfs_si2b'),
                                      si2 = c('log_si_igfs_si3a',
                                              'log_si_igfs_si3b',
                                              'log_si_igfs_si3c',
                                              'log_si_igfs_si3d'),
                                      comm= c('ldist_gil','ldist_bmt',
                                              'aldist_gil','aldist_bmt')))
  
  ## Get the model fit
  fit <- gadget3:::g3_fit(model, tmp$value)
  ## Save the model
  save(model, tmb_model, tmb_param, file = file.path(base_dir, "models", vers, "model.Rdata"))
  
  
}
    
  # 
  # ## Iterative re-weighting
  # res1 <-  g3_lik_out(model, tmb_param) 
  # #res2 <-  g3_iterative_setup(res1)
   # res2 <-  g3_iterative_setup(res1, grouping = list(si1 = c('log_si_igfs_si1',
   #                                                           'log_si_igfs_si2a',
   #                                                           'log_si_igfs_si2b'),
   #                                                   si2 = c('log_si_igfs_si3a',
   #                                                           'log_si_igfs_si3b',
   #                                                           'log_si_igfs_si3c',
   #                                                           'log_si_igfs_si3d'),
   #                                                   comm= c('ldist_gil','ldist_bmt',
   #                                                          'aldist_gil','aldist_bmt'))
   #                             )
   
  # res3 <-  parallel::mclapply(res2, function(x) g3_iterative_run(x, tmb_model), mc.cores = parallel::detectCores())
  # res4 <-  parallel::mclapply(res3, function(x) g3_lik_out(model,x), mc.cores = parallel::detectCores())
  # res5 <-  g3_iterative_final(res4)
  # res6 <-  parallel::mclapply(res5, function(x) g3_iterative_run(x, tmb_model), mc.cores = parallel::detectCores())
  # res7 <-  parallel::mclapply(res6, function(x) g3_lik_out(model,x), mc.cores = parallel::detectCores())
  # #res8 <-  g3_iterative_final(res7)
  # 
  # names(res2) %>%
  #   set_names(.,.) %>%
  #   map(function(x) {
  #     #if (class(res7[[x]]) == "try-error") return(NULL)
  #     res7[[x]] %>%
  #       left_join(res6[[x]] %>%
  #                   select(comp = switch,weight=value) %>%
  #                   mutate(comp = gsub('_weight','',comp),
  #                          weight = unlist(weight)))
  #   })-> out
  # 
  # names(res2) %>%
  #   set_names(.,.) %>%
  #   map(function(x) {
  #     res4[[x]] %>%
  #       left_join(res3[[x]] %>%
  #                   select(comp = switch,weight=value) %>%
  #                   mutate(comp = gsub('_weight','',comp),
  #                          weight = unlist(weight)))
  #   })-> out2
  # 
  # 
  # out %>% bind_rows(.id='group') %>% group_by(group) %>% summarise(s=sum(value*weight))
  # out2 %>% bind_rows(.id='group') %>% group_by(group) %>% summarise(s=sum(value*weight))
  # 
  # #fit <- gadget3:::g3_fit(model, res6$ldist_lln$value)
  
  


