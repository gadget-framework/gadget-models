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

## -----------------------------------------------------------------------------
## OPTIONS 
## -----------------------------------------------------------------------------

## Model directory and version, output will be stored in file.path(base_dir, vers)
base_dir <- '07-bling/gadget3'
vers <- 'models/01-baseline'

## Model year range and species code
year_range <- 1982:2020
species_name <- 'bli' 

## Whether or not to call the setup-data scripts, if FALSE, the datasets will be loaded from file.path(base_dir, "data")
read_data <- FALSE

## Stock options options:
single_stock_model <- FALSE
maxlengthgroupgrowth <- 5      # Maximum length group growth
lencv <- 0.1                   # CV for initial conditions standard deviations
timevarying_K <- TRUE         # Have the growth parameter K vary by years
exponentiate_bbin <- TRUE     # Whether or not to exponentiate the beta-binomial parameter
init_abund_by_age <- FALSE      # Will have a parameter per age group
init_abund_by_stock <- FALSE    # Will have a parameter per age group per stock (irrelevant for single stock model)

## Fleet options:
single_fleet <- FALSE            # Single commercial fleet?
dome_comm <- TRUE                # Only applies if single_fleet == TRUE
dome_bmt <- FALSE                # Only applies if single_fleet == FALSE
dome_lln <- TRUE                # Only applies if single_fleet == FALSE
dome_aut <- FALSE

## Likelihood options:
si_length_intervals <- c(20,52,60,72,80,92,100,140)     # Length intervals for slicing the SIs

## Which diagnostics would you like to run
run_iterative <- FALSE
run_jitter <- FALSE
# run_retro <- FALSE      Not implemented yet
# run_bootstrap <- FALSE  Not implemented yet

## Iterative settings
iterative_folder <- 'WGTS'    # Folder within file.path(base_dir, vers) which will store the output of iterative re-weighting
cv_floor <- 0                 # Option can be used to prevent the overfitting of survey indices, see ?g3_iterative
iter_group_SI <- FALSE        # whether or not to group all SI's together for optimisation
iter_group_aut <- FALSE       # group together autumn likelihood components

## Jitter settings
jitter_folder <- 'JITTER'               # Folder within file.path(base_dir, vers) which will store the output of the jitter analysis
number_of_jitters <- 100                # How many jitters are required
jitter_optimised_pars <- FALSE          

## Retrospective analysis options:
peel <- 0
npeels <- 6

## Optimisation settings
include_bound_penalty <- TRUE   # Whether or not to include a penalty for when parameters go outside their bounds
optim_controls <- list(maxit = 3, reltol = 1e-10)#.Machine$double.eps^2)  # controls for g3_optim, see ?optim

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
  species = toupper(species_name))

# Map area names to integer area numbers (in this case only "1" ==> 1, but could be anything)
areas <- structure(
  seq_along(defaults$area),
  names = names(defaults$area))

# Timekeeping for the model, i.e. how long we run for
time_actions <- list(
  gadget3::g3a_time(start_year = min(defaults$year), 
                    end_year = max(defaults$year),
                    defaults$timestep),
  list())

## Data and model folders
fs::dir_create(file.path(base_dir, c('data')))
fs::dir_create(file.path(base_dir, vers, c(iterative_folder, jitter_folder)))

## ------------------------------------------------------------------------------------

source(file.path(base_dir, '00-setup', 'setup-stocks.R'))  # Generates stock objects

if (single_stock_model){
  source(file.path(base_dir, '00-setup', 'setup-model-single-stock.R'))  # Generates stock actions for single_stock
}else{
  source(file.path(base_dir, '00-setup', 'setup-model.R'))  # Generates mat_stock actions and imm_stock actions  
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
model <- gadget3::g3_to_r(actions, strict = FALSE, trace = FALSE)

# Turn actions into C++ objective function code
tmb_model <- gadget3::g3_to_tmb(actions)

tmb_param <- 
  attr(tmb_model, 'parameter_template') %>% 
  
  ## Recruitment and initial conditions
  gadget3::g3_init_val('*.rec.#', 100, lower = 0.001, upper = 200) %>% # ifelse(random_recruitment, penalise_recruitment, 1)) %>% 
  gadget3::g3_init_val('*.init.#', 100, lower = 0.001, upper = 200) %>% #ifelse(random_initial, penalise_initial, 1)) %>% 
  gadget3::g3_init_val('*.rec.scalar', 50, spread = 0.99) %>% 
  gadget3::g3_init_val('*.init.scalar', 5000, spread = 0.99) %>%
  gadget3::g3_init_val('init.F', 
                       ifelse((!init_abund_by_age && !init_abund_by_stock), 0.2, 0), 
                       spread = {if (!init_abund_by_age && !init_abund_by_stock) 0.75 else NULL}) %>% 
  gadget3::g3_init_val('*.lencv', lencv) %>%   # Used to calculate length sd (in initial conditions)
  gadget3::g3_init_val('*.rec.sd', 15, spread = 0.6) %>%     # To use lencv here, use g3a_renewal_normalcv instead of g3a_renewal_normalparam

  ## Growth
  gadget3::g3_init_val('*.Linf', 140, spread = 0.3) %>% 
  gadget3::g3_init_val(ifelse(timevarying_K, '*.K.#', '*.K'), 0.1, spread = 0.5) %>%
  gadget3::g3_init_val('*.t0', 1, lower = -1, upper = 5) %>%
  # gadget3::g3_init_val('*.recl', 30, spread = 0.5) %>%       Using t0 (g3a_renewal_vonb_t0) instead of recl (g3a_renewal_vonb_recl)
  gadget3::g3_init_val('*.bbin', 100, lower = 1e-05, upper = 1000) %>% 
  gadget3::g3_init_val('*.M.#', 0.15) %>% 
  
  ## Maturity ogives
  #  For initial conditions if init_abund_mode = 1
  gadget3::g3_init_val('*.mat_initial_alpha', 1, spread = 0.5) %>% 
  gadget3::g3_init_val('*.mat_initial_a50', 7) %>% 
  #  For maturation
  gadget3::g3_init_val('*.mat_alpha', 0.08, spread = 0.75) %>%
  gadget3::g3_init_val('*.mat_l50', mat.l50$l50, spread = 0.25) %>% 
  
  ## Fleet selection parameters
  #  S-shaped
  gadget3::g3_init_val('*.alpha', 1, spread = 0.99) %>% 
  gadget3::g3_init_val('*.l50', mean(sapply(stocks, gadget3::g3_stock_def, 'midlen')), spread = 0.5) %>% 
  #  Dome-shaped
  gadget3::g3_init_val('*.p1', log(2), lower = 0, upper = 3) %>%
  gadget3::g3_init_val('*.p3', 0.1, lower = 0.001, upper = 10) %>%
  gadget3::g3_init_val('*.p4', 0.02, lower = 0.001, upper = 1e3) %>%
  
  ## Random effects/penalities
  # gadget3::g3_init_val('recruitment_sigma', 0.2, 0.01, 10, ifelse(penalise_recruitment == 0, 1, 0)) %>% 
  # gadget3::g3_init_val('initial_sigma', 0.2, 0.01, 10, ifelse(penalise_initial == 0, 1, 0)) %>% 
  # gadget3::g3_init_val('rnd_recruitment_weight', 1, 0.01, 100, 0) %>% 
  # gadget3::g3_init_val('rnd_initial_weight', 1, 0.01, 100, 0) %>% 
  # gadget3::g3_init_val('zero', 0, -1, 1, 0) %>% 
  
  ## Weight-length
  gadget3::g3_init_val('*.walpha', lw.constants$estimate[1]) %>% 
  gadget3::g3_init_val('*.wbeta', lw.constants$estimate[2]) %>% 
  gadget3::g3_init_val('recage', min(sapply(stocks, gadget3::g3_stock_def, 'minage')))


## --------------------------------------------------------------------------

if (include_bound_penalty){
  actions <- c(actions, list(gadget3::g3l_bounds_penalty(tmb_param %>% filter(!grepl('_sigma$|_sigma_exp$',switch)))))
  model <- gadget3::g3_to_r(actions)
  tmb_model <- gadget3::g3_to_tmb(actions)
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
  
  grouping <- list(sind = c('si_aut_1',
                            'si_aut_2a',
                            'si_aut_2b',
                            'si_aut_3a',
                            'si_aut_3b',
                            'si_aut_3c',
                            'si_aut_3d'))
  if (!single_fleet){
    grouping <- c(grouping,
                  list(aut = c('ldist_aut', if (iter_group_aut) 'matp_aut' else NULL)))
  }else{
    grouping <- c(grouping, list(comm = c('ldist_comm')))
  }
}else{
  
  grouping <- list(sind1 = c('si_aut_1',
                             'si_aut_2a',
                             'si_aut_2b'),
                   sind2 = c('si_aut_3a',
                             'si_aut_3b',
                             'si_aut_3c',
                             'si_aut_3d'))
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