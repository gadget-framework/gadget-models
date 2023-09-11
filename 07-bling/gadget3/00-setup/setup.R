## -----------------------------------------------------------------------------
##
## Runner to build a Gadget3 model for Blue Ling 
##
## -----------------------------------------------------------------------------

library(mfdb)
library(gadget3)
library(gadgetutils)
library(gadgetplots)
library(tidyverse)
library(g3experiments)

## Model directory
base_dir <- '07-bling/gadget3'
## Model version
vers <- 'models/01-baseline'

## -----------------------------------------------------------------------------
## OPTIONS 
## -----------------------------------------------------------------------------

## Whether or not to call the setup-data scripts
read_data <- FALSE

## Setup options -
## Parameters:
include_bound_penalty <- TRUE

## Stock options options:
single_stock_model <- TRUE
maxlengthgroupgrowth <- 5  # Maximum length group growth
lencv <- 0.1               # CV for initial conditions standard deviations
timevarying_K <- FALSE
exponentiate_bbin <- FALSE  # Whether or not to exponentiate the beta-binomial parameter

## Fleet options:
single_fleet <- FALSE            # Single commercial fleet?
dome_comm <- TRUE             # Only applies if single_fleet == TRUE
dome_bmt <- FALSE             # Only applies if single_fleet == FALSE
dome_lln <- FALSE             # Only applies if single_fleet == FALSE

## Different options for running g3 and its diagnostics
run_iterative <- FALSE
run_jitter <- FALSE
run_retro <- FALSE
run_leaveout <- FALSE

# Controls for g3_optim, see ?optim
optim_controls <- list(maxit = 3, reltol = 1e-10)#.Machine$double.eps^2)

## Iterative re-weighting options:
cv_floor <- 0
iter_group_SI <- FALSE         # whether or not to group all SI's together for optimisation
iter_group_aut <- FALSE        # group together autumn likelihood components

## Retrospective analysis options:
peel <- 0
npeels <- 6

## Jitter options:
njits <- 3    #100

## -----------------------------------------------------------------------------
tyr <- lubridate::year(Sys.Date())
year_range <- 1982:(tyr - 1)
species_name <- 'bli' 

## -----------------------------------------------------------------------------

reitmapping <- 
  read.table(
    system.file("demo-data", "reitmapping.tsv", package="mfdb"),
    header=TRUE,
    as.is=TRUE)

defaults <- list(
  area = mfdb_group("1" = unique(reitmapping$SUBDIVISION)),
  timestep = mfdb_timestep_quarterly,
  year = year_range,
  species = toupper(species_name))

# Map area names to integer area numbers (in this case only "1" ==> 1, but could be anything)
areas <- structure(
  seq_along(defaults$area),
  names = names(defaults$area))

# Timekeeping for the model, i.e. how long we run for
time_actions <- list(
  g3a_time(start_year = min(defaults$year), 
           end_year = max(defaults$year),
           defaults$timestep),
  list())

## Data and model folders
fs::dir_create(file.path(base_dir, c('data')))
fs::dir_create(file.path(base_dir, vers))
fs::dir_create(file.path(base_dir, vers, c('RETRO', 'WGTS', 'JITTER', 'LEAVEOUT')))

## ------------------------------------------------------------------------------------

source(file.path(base_dir, '00-setup', 'setup-stocks.R'))  # Generates stock objects

if (single_stock_model){
  source(file.path(base_dir, '00-setup', 'setup-model-single-stock.R'))  # Generates mat_stock_actions / imm_stock_actions  
}else{
  source(file.path(base_dir, '00-setup', 'setup-model.R'))  # Generates mat_stock_actions / imm_stock_actions  
}


## Load data objects ----------------------------------------------------------
if(read_data){
  mdb <- mfdb('Iceland', db_params = list(host = 'mfdb.hafro.is'))
  #mdb <- mfdb("../../mfdb/copy/iceland.duckdb")
  source(file.path(base_dir, '00-setup', 'setup-fleet-data.R'))
  source(file.path(base_dir, '00-setup', 'setup-catchdistribution.R'))
  source(file.path(base_dir, '00-setup', 'setup-indices.R'))
  source(file.path(base_dir, '00-setup', 'setup-initial_parameters.R'))
} else {
  fs::dir_ls(file.path(base_dir, 'data')) %>%
    stringr::str_subset('.Rdata') %>%
    lapply(load,.GlobalEnv)
}

## Configure model actions ------------------------------------------------------------


source(file.path(base_dir, '00-setup', 'setup-fleets.R'))  # Generates fleet_actions
source(file.path(base_dir, '00-setup', 'setup-likelihood.R'))  # Generates likelihood_actions
#source(file.path(base_dir, '00-setup', 'setup-randomeffects.R'))  # Generates random actions

##### Compile the r- and tmb-based models ######################################


## Collate actions
actions <- c(
  stock_actions,
  fleet_actions,
  likelihood_actions,
  #  if (random_recruitment || random_initial) random_actions else NULL,
  time_actions,
  NULL
)

# Turn actions into an R function
model <- g3_to_r(actions)#, strict = TRUE, trace = TRUE)

# Turn actions into C++ objective function code
tmb_model <- g3_to_tmb(actions)

## Fill in the parameter template
tmb_param <- 
  attr(tmb_model, 'parameter_template') %>% 
  g3_init_guess('\\.rec', 100, 0.001, 200, 1) %>% # ifelse(random_recruitment, penalise_recruitment, 1)) %>% 
  g3_init_guess('\\.init.[0-9]', 100, 0.001, 200, 1) %>% #ifelse(random_initial, penalise_initial, 1)) %>% 
  g3_init_guess('recl', 30, 10, 50, 1) %>% 
  g3_init_guess('reclcv', lencv, lencv*0.9, lencv*1.1, 0) %>% 
  g3_init_guess('lencv', 0.2, 0.1, 0.3, 0) %>% 
  g3_init_guess('rec.sd', 5, 4, 20, 1) %>% 
  g3_init_guess('init.scalar', 50, 1, 100, 1) %>% 
  g3_init_guess('rec.scalar', 5, 1, 10, 1) %>% 
  g3_init_guess('init.rec.scalar', 50, 1, 100, 1) %>% 
  #g3_init_guess('\\.rec.sigma', 0.2, -1, 1, 0) %>% 
  g3_init_guess('Linf', 140, 100, 200, 1) %>% 
  g3_init_guess('\\.K', 0.09, 0.04, 0.2, 1) %>%
  g3_init_guess('\\.t0', 1, -1, 5, 0) %>%
  g3_init_guess('bbin', 100, 1e-05, 1000, 1) %>% 
  g3_init_guess('\\.alpha', 0.5, 0.01, 3, 1) %>% 
  g3_init_guess('\\.l50', 50, 10, 100, 1) %>% 
  g3_init_guess('init.F', 0, 0.1, 1, 0) %>% 
  g3_init_guess('\\.M$', 0.15, 0.001, 1, 0) %>% 
  g3_init_guess('^M$', 0.15, 0.001, 1, 0) %>%  
  g3_init_guess('mat_initial_alpha', 1, 0.5, 2, 1) %>% 
  g3_init_guess('mat_initial_a50', 7, 3, 15, 0) %>% 
  g3_init_guess('mat.alpha', 0.07, 0.01, 0.2, 1) %>%
  g3_init_guess('mat.l50', mat.l50$l50, 0.75*mat.l50$l50, 1.25*mat.l50$l50, 1) %>%
  g3_init_guess('sigma_alpha', init.sigma.coef[['alpha']]) %>%
  g3_init_guess('sigma_beta', init.sigma.coef[['beta']]) %>%
  g3_init_guess('sigma_gamma', init.sigma.coef[['gamma']]) %>% 
  
  ## Fleet selection parameters
  g3_init_guess('andersen.p0$', 0, NA, NA, 0) %>%
  g3_init_guess('andersen.p2$', 1, NA, NA, 0) %>%
  g3_init_guess('andersen.L$', max(unlist(lapply(stocks, gadget3::g3_stock_def, 'minlen'))), NA, NA, 0) %>%
  g3_init_guess('\\.p1$', log(2), 0, 3, 1) %>%
  g3_init_guess('\\.p3$', 0.1, 0, 10, 1) %>%
  g3_init_guess('\\.p4$', 0.02, 0, 1e3, 1) %>%
  
  ## Random effects/penalities
  # g3_init_guess('recruitment_sigma', 0.2, 0.01, 10, ifelse(penalise_recruitment == 0, 1, 0)) %>% 
  # g3_init_guess('initial_sigma', 0.2, 0.01, 10, ifelse(penalise_initial == 0, 1, 0)) %>% 
  # g3_init_guess('rnd_recruitment_weight', 1, 0.01, 100, 0) %>% 
  # g3_init_guess('rnd_initial_weight', 1, 0.01, 100, 0) %>% 
  # g3_init_guess('zero', 0, -1, 1, 0) %>% 
  
  ## Weight-length
  g3_init_guess('walpha', lw.constants$estimate[1]) %>% 
  g3_init_guess('wbeta', lw.constants$estimate[2])


## --------------------------------------------------------------------------

if (include_bound_penalty){
  actions <- c(actions, list(g3l_bounds_penalty(tmb_param %>% filter(!grepl('_sigma$|_sigma_exp$',switch)))))
  model <- g3_to_r(actions)
  tmb_model <- g3_to_tmb(actions)
}

## Run the R-model
result <- model(tmb_param$value)
print(result[[1]])

# List all available reports
print(names(attributes(result)))

## Write out parameters and both models
save(tmb_param, file = file.path(base_dir, vers, 'tmb_param.Rdata'))
save(model, file = file.path(base_dir, vers, 'r_model.Rdata'))
save(tmb_model, file = file.path(base_dir, vers, 'tmb_model.Rdata'))

## -----------------------------------------------------------------------------

## Setup grouping for iterative re-weighting 
if (iter_group_SI){
  
  grouping <- list(sind = c('log_si_aut_1',
                            'log_si_aut_2a',
                            'log_si_aut_2b',
                            'log_si_aut_3a',
                            'log_si_aut_3b',
                            'log_si_aut_3c',
                            'log_si_aut_3d'))
  if (!single_fleet){
    grouping <- c(grouping,
                  list(aut = c('ldist_aut', if (iter_group_aut) 'matp_aut' else NULL)))
  }else{
    grouping <- c(grouping, list(comm = c('ldist_comm')))
  }
}else{
  
  grouping <- list(sind1 = c('log_si_aut_1',
                             'log_si_aut_2a',
                             'log_si_aut_2b'),
                   sind2 = c('log_si_aut_3a',
                             'log_si_aut_3b',
                             'log_si_aut_3c',
                             'log_si_aut_3d'))
  if (!single_fleet){
    grouping <- c(grouping,
                  list(aut = c('ldist_aut', if (iter_group_aut) 'matp_aut' else NULL)))
  }else{
    grouping <- c(grouping, list(comm = c('ldist_comm')))
  } 
}

## Iterative re-weighting
if (run_iterative){
  
  # Compile and generate TMB ADFun (see ?TMB::MakeADFun)
  obj.fun <- gadget3::g3_tmb_adfun(tmb_model, tmb_param)
  
  ## Run iterative re-weighting
  params.out <- gadgetutils::g3_iterative(file.path(base_dir, vers),
                                          wgts = 'WGTS',
                                          model = tmb_model,
                                          params.in = tmb_param,
                                          grouping = grouping,
                                          method = 'BFGS',
                                          control = optim_controls,
                                          use_parscale = TRUE,
                                          shortcut = FALSE,
                                          cv_floor = cv_floor,
                                          resume_final = FALSE)
  
  ## Get the model fit
  fit <- gadgetutils::g3_fit(model, params.out)
  save(fit, file = file.path(base_dir, vers, 'fit.Rdata'))
  gadgetplots::gadget_plots(fit, file.path(base_dir, vers, 'figs'), file_type = 'html')
  
  sdout <- TMB::sdreport(obj.fun, g3_tmb_par(params.out))
  save(sdout, file = file.path(base_dir, vers, 'sdout.Rdata'))
  
}else{
  load(file = file.path(base_dir, vers, 'fit.Rdata'))
}

## Run the retro
if (run_retro){
  
  load(file = file.path(base_dir, vers, 'WGTS/params_final.Rdata'))
  
  tmp <- params_final$value[grepl('_weight$', params_final$switch)]
  retro_pars <- tmb_param
  retro_pars$value[names(tmp)] <- tmp
  
  retro_model <- list()
  retro_params <- list()
  
  for(peel in 1:npeels){
    
    source(file.path(base_dir, '00-setup', 'setup-likelihood.R'))  # Generates likelihood_actions
    
    retro_actions <- 
      c(stock_actions,
        fleet_actions,
        likelihood_actions,
        if (penalise_recruitment) random_actions else NULL,
        time_actions,
        list(g3l_bounds_penalty(tmb_param %>% 
                                  filter(!grepl('_sigma$|_sigma_exp$',switch, optimise))))# filter(optimise, !grepl('\\.init\\.[0-9]|\\.rec\\.[0-9]', switch)))))filter(optimise, !grepl('\\.init\\.[0-9]|\\.rec\\.[0-9]', switch))))
      )
    retro_model[[peel]] <- g3_to_tmb(retro_actions)
    retro_params[[peel]] <- params_final# retro_pars #gadgetutils::jitter_params(params_final)
    retro_params[[peel]]$value$retro_years <- peel
  } 
  
  peel <- 0
  
  retro <- 
    parallel::mclapply(1:npeels,function(x){
      g3_optim(retro_model[[x]],
               retro_params[[x]],
               control = optim_controls)
    }, 
    mc.cores = parallel::detectCores())
  ## Collate fit
  retro_fit <- 
    1:npeels %>%
    set_names(paste0('r',1:npeels)) %>% 
    purrr::map(function(x) g3_fit(model = retro_model[[x]], params = retro[[x]])) 
  
  save(retro_fit, file = file.path(base_dir, vers,'RETRO', 'retro_fit.Rdata'))
  
}

## Jitter analysis
if (run_jitter){
  
  jitter_out <- gadgetutils::g3_jitter(file.path(base_dir, vers), 
                                       model = tmb_model, 
                                       params = tmb_param,
                                       njits = njits,
                                       control = optim_controls)
  
  ## Collate the fits
  jitter_fit <- 1:length(jitter_out) %>% 
    purrr::map(function(x) try(gadgetutils::g3_fit(model = tmb_model, params = jitter_out[[x]])) )
  
  gadgetplots::plot_jitter(jitter_fit)
  save(jitter_fit, file = file.path(base_dir, vers, 'jitter_fit.Rdata'))
  
}

## Leaveout analysis
if (run_leaveout){
  
  leaveout_out <- gadgetutils::g3_leaveout(file.path(base_dir, vers), 
                                           model = tmb_model, 
                                           params = tmb_param,
                                           grouping = grouping,
                                           control = optim_controls)
  
  ## Collate the fits
  leaveout_fit <- setNames(names(leaveout_out), names(leaveout_out)) %>% 
    purrr::map(function(x) try(gadgetutils::g3_fit(model = tmb_model, params = leaveout_out[[x]])) )
  
  gadgetplots::plot_leaveout(leaveout_fit)
  save(leaveout_fit, file = file.path(base_dir, vers, 'leaveout_fit.Rdata'))
  
}