## -----------------------------------------------------------------------------
##
## Runner to build a Gadget3 model for Blue Ling 
##
## -----------------------------------------------------------------------------

library(mfdb)
library(gadget3)
library(gadgetutils)
library(gadgetplots)
library(mar)
#library(g3experiments)

## Model directory
base_dir <- '07-bling/gadget3 - WKBDEEP'

## Model version
vers <- 'models/22-variant'
#vers <- 'models/07-baseline'
## For the baseline model we will fix Linf, estimate k and t0
## Some easy options for alternative models
faroe_growth <- FALSE
faroe_M <- FALSE
estimate_linf <- TRUE
estimate_k <- TRUE
estimate_t0 <- FALSE
estimate_phi <- FALSE
estimate_reccv <- FALSE


#fixed_growth_pars <- list(linf = 125, k = 0.152, t0 = 1.552)
#fixed_growth_pars <- list(linf = 125, k = 0.152, t0 = 1.552)

## Combining data from Faroes, with commercial and survey samples from IS
if (faroe_growth){ 
  #fixed_growth_pars <- list(linf = 125, k = 0.15, t0 = 1.552)  ## Faroes - OM
  fixed_growth_pars <- list(linf = 129, k = 0.11, t0 = -1.16)  ## Faroes - data
  #fixed_growth_pars <- list(linf = 132.8939, k = 0.1042, t0 = 1.552)     
}else{
  fixed_growth_pars <- list(linf = 134.4084, k = 0.1016, t0 = 0)  ## Iceland and Faroes
}

## Penalising recruitment or not?
recruitment_free_pars <- TRUE
penalise_recruitment <- 1             # Only relevant if recruitment_free_pars = TRUE
exponentiate_recruitment <- FALSE     # Only relevant if recruitment_free_pars = TRUE (done automatically if FALSE)
recruitment_sigma <- 0.4            # Only relevant if penalise recruitment = 1
recruitment_std <- 0.2                # Only relevant if penalise recruitment = 0
arima_phi <- 0
naturalmortality <- ifelse(faroe_M, 0.11, 0.15)

# Composition data - 
bmt_age <- TRUE             # whether to include the commercial trawl age readings
bmt_age_stratified <- FALSE       # If including commercial age readings, should they be stratified in the likelihood
iter_group_BMTAGE <- FALSE

stratified_indices <- TRUE

## -----------------------------------------------------------------------------
## OPTIONS 
## -----------------------------------------------------------------------------

## Whether or not to call the setup-data scripts
read_data <- FALSE
read_bootstrap_data <- FALSE

## Which model diagnostics to run
run_iterative <- TRUE
run_retro <- FALSE
run_bootstrap <- FALSE
run_mprofile <- FALSE
run_growthprofile <- FALSE
run_jitter <- FALSE
run_leaveout <- FALSE

## Model dimensions and stock:
year_range <- 1975:2023 #(lubridate::year(Sys.Date())-1)
project_years <- 100
species_name <- 'bli' 

## Stock options:
single_stock_model <- FALSE
init_rec_scalar <- FALSE              # single scalar for initial conditions and recruitment?
timevarying_K <- FALSE               # time-varying K (vonb growth parameter)
exponentiate_bbin <- TRUE            # exponentiate the beta-binomial parameter
maxlengthgroupgrowth <- 4             # Maximum length group growth
lencv <- 0.1                          # CV for initial conditions standard deviations
mean_len_recl <- FALSE

## Initial abundance mode
# 4 options:
# (1) initabun_by_age = T initabun_by_stock = T - one parameter per age class per stock
# (2) initabun_by_age = T initabun_by_stock = F - one parameter per age class, number per age split between stocks with an ogive
# (3) initabun_by_age = F initabun_by_stock = T - one parameter per stock, number per age determined by decay with mortality (F and M)
# (4) initabun_by_age = F initabun_by_stock = F - one parameter, number per age determined by decay with mortality (F and M), split between stocks with an ogive
initabun_by_age <- TRUE               
initabun_by_stock <- FALSE

## Fleet options:
single_fleet <- FALSE            # Single commercial fleet?
dome_comm <- FALSE              # Only applies if single_fleet == TRUE
dome_bmt <- FALSE                # Only applies if single_fleet == FALSE
dome_lln <- FALSE                # Only applies if single_fleet == FALSE
timevarying_bmt <- FALSE         # Only applies to S-shaped at the moment
timevarying_lln <- FALSE        # Only applies to S-shaped at the moment 
 
## Likelihood options:
include_bound_penalty <- TRUE

# Survey indices -
selection_si_lik <- FALSE
#si_len_breaks <- c(20,52,60,72,80,92,100,140)
si_len_breaks <- c(20,44,56,68,80,92,104,140)
si_len_slopes <- NULL#c(20,44)                           # NULL to have slope = 1 for each component
si_cv <- 0.2

## DIAGNOSTICS
## Iterative re-weighting:
wgts_folder <- 'WGTS'
cv_floor <- 0
iter_group_SI <- TRUE         # whether or not to group all SI's together for optimisation
iter_group_AUT <- FALSE        # group together autumn likelihood components

## Jitter options:
jitter_folder <- 'JITTER'
number_of_jitters <- 100
jitter_fraction <- 0.1
jitter_optimised_pars <- FALSE   # If TRUE, the optimised pars from 'wgts_folder' will be jittered. If FALSE, initial parameters will be jittered with weights read from 'wgts_folder'. 
jitter_folder <- paste(jitter_folder, as.numeric(jitter_optimised_pars), sep = '_')

## Leaveout options:
leaveout_folder <- 'LEAVEOUT'

## M profile options:
profile_folder <- 'M_PROFILE'
profile_short <- TRUE
profile_reps <- 1
M_values <- seq(0.08, 0.2, by = 0.01)

## Growth profile options:
gw_profile_folder <- 'K_LINF_PROFILE_v2'
gw_profile_short <- FALSE
gw_profile_reps <- 1
gw_values <- 
  expand.grid(K = seq(0.06, 0.20, by = 0.02),
              Linf = seq(90, 150, by = 10))

## Retro options:
retro_folder <- 'RETRO'
number_of_peels <- 5

## Bootstrap options:
boot_folder <- 'BOOTSTRAP'                # Bootstrap results will be written to file.path(base_dir, vers, boot_folder)
nboots <- 100                           # Number of bootstrap replicates
boot_short <- TRUE                      # If FALSE, iterative re-weighting will be performed. If TRUE, weights will be read in from 'wgts_folder'
bootstrap_cores <- 35

## Optimisation control:
# Control list for stats::optim
optim_control <- list(maxit = 2000, reltol = 1e-10) 

if (FALSE){
  ## RANDOM EFFECTS:
  random_recruitment <- FALSE
  random_initial <- FALSE
  penalise_initial <- 0
  bound_random_cv <- FALSE
  #bound_random_effects <- TRUE   
  
  # Random effects options
  usenlminb <- FALSE            # If TRUE, optimisations will be performed with stats::nlminb (only using for random effects atm). If FALSE, stats::optim is used
  nlminb_control <- list(trace=1, eval.max=2000, iter.max=1000, rel.tol=1e-10)
  random_control <- optim_control  
  newton_control <- list(maxit = 50, tol = 1e-10, tol10 = 0)    # for TMB::newton inner optimisations
}

## ---------------------------------------------------Hér er viðhengið sem fylgdi.--------------------------

## Parameters that will should not change
boot_repl <- 1
peel <- 0

## Modify setting based on the above:
#if (!bmt_age) year_range <- year_range[year_range >= 1978]
M_values <- rep(M_values, each = profile_reps)
## Create suffix for model
vers <- paste0(vers, 
               {if (faroe_growth) '_Fgr' else '_IFgr'},
               {if (faroe_M) '_FarM' else NULL},
               '_Linf', as.numeric(estimate_linf),
               '_K', as.numeric(estimate_k),
               '_t0', as.numeric(estimate_t0),
               '_mlgg', maxlengthgroupgrowth,
               {if (!estimate_reccv) '_reccv0' else NULL},
               {if (recruitment_free_pars) paste0('_penrec', penalise_recruitment) else NULL},
               {if (recruitment_free_pars && penalise_recruitment == 1) paste0('_sd', recruitment_sigma) else NULL},
               {if (!recruitment_free_pars) paste0('_sd', recruitment_std) else NULL},
               {if (!recruitment_free_pars) paste0('_ar', arima_phi) else NULL},
               '_agedata', as.numeric(bmt_age),
               {if (dome_bmt) '_bmtAnd' else NULL},
               {if (timevarying_bmt) '_bmtTV' else NULL},
               {if (dome_lln) '_llnAnd' else NULL},
               {if (timevarying_lln) '_llnTV' else NULL},
               {if (!mean_len_recl) '_t0' else NULL},
               {if (!is.null(si_len_slopes)) '_sislope' else NULL},
               {if (!iter_group_SI) '_aut2gp' else NULL})

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
  project_years = project_years,
  species = toupper(species_name),
  save_bootstrap_data = FALSE)               ## Adding this so we can stop overwriting bootstrap and standard datasets

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
fs::dir_create(file.path(base_dir, c('data', vers)))

## ------------------------------------------------------------------------------------

source(file.path(base_dir, '00-setup', 'setup-stocks.R'))  # Generates stock objects

if (single_stock_model){
  source(file.path(base_dir, '00-setup', 'setup-model-single-stock.R'))  # Generates mat_stock_actions / imm_stock_actions  
}else{
  source(file.path(base_dir, '00-setup', 'setup-model.R'))  # Generates mat_stock_actions / imm_stock_actions  
}

## -----------------------------------------------------------------------------
## Data ------------------------------------------------------------------------
## -----------------------------------------------------------------------------

if(read_data){
  mdb <- mfdb('Iceland', db_params = list(host = 'mfdb.hafro.is'))
  source(file.path(base_dir, '00-setup', 'setup-fleet-data.R'))
  source(file.path(base_dir, '00-setup', 'setup-catchdistribution.R'))
  source(file.path(base_dir, '00-setup', 'setup-indices.R'))
  source(file.path(base_dir, '00-setup', 'setup-initial_parameters.R'))
  mfdb::mfdb_disconnect(mdb)
} else {
  fs::dir_ls(file.path(base_dir, 'data')) |>
    stringr::str_subset('.Rdata') |>
    stringr::str_subset('bootstrap',negate = TRUE) |> 
    lapply(load,.GlobalEnv)
}

## Configure model actions ------------------------------------------------------------

source(file.path(base_dir, '00-setup', 'setup-fleets.R'))  # Generates fleet_actions
source(file.path(base_dir, '00-setup', 'setup-likelihood.R'))  # Generates likelihood_actions
source(file.path(base_dir, '00-setup', 'setup-randomeffects.R'))  # Generates random actions

##### Compile the r- and tmb-based models ######################################

## Collate actions
actions <- c(
  stock_actions,
  fleet_actions,
  likelihood_actions,
  if (recruitment_free_pars && penalise_recruitment == 1) random_actions else NULL,
  #if (random_recruitment || random_initial) random_actions else NULL,
  time_actions,
  NULL
  )

# Turn actions into C++ objective function code
tmb_model <- g3_to_tmb(c(actions,
                         list(g3a_report_detail(actions)),
                         list(g3l_bounds_penalty(actions)),
                         #list(g3a_trace_nan(actions, browse_on_error = TRUE)),
                         NULL
                         )
                       )

# Turn actions into an R function
model <- g3_to_r(attr(tmb_model, 'actions'))#, strict = TRUE, trace = TRUE)

## Fill in the parameter template
tmb_param <- 
  attr(tmb_model, 'parameter_template') |> 
  
  ## Recruitment and initial conditions
  g3_init_val('*.rec.#', 50, lower = 0.001, upper = 100) |> # ifelse(random_recruitment, penalise_recruitment, 1)) |> 
  g3_init_val('*.init.#', 50, lower = 0.001, upper = 100) |> #ifelse(random_initial, penalise_initial, 1)) |> 
  g3_init_val('*.rec.scalar', 25, spread = 0.99) |> 
  g3_init_val('*.init.scalar', 25, spread = 0.99) |>
  g3_init_val('*.init.F', 
                       ifelse((!initabun_by_age && !initabun_by_stock), 0.2, 0), 
                       spread = {if (!initabun_by_age && !initabun_by_stock) 0.75 else NULL}) |> 
  g3_init_val('*.lencv.#', init.cv |> filter(age %in% 2:20) |> arrange(age) |> pull(cv), optimise = FALSE) |>   # Used to calculate length sd (in initial conditions)
  #g3_init_val(paste0('*.lencv.', g3_stock_def(imm_stock, 'minage')), lencv, lower = 0.01, upper = 0.3) |>   # Used to calculate length sd (in initial conditions)
  g3_init_val('*.rec.sd', 5, lower = 0.1, upper = 10) |>     # To use lencv here, use g3a_renewal_normalcv instead of g3a_renewal_normalparam
  
  ## Growth
  g3_init_val('*.Linf', fixed_growth_pars$linf, lower = 100, upper = 160, optimise = estimate_linf) |> 
  g3_init_val(ifelse(timevarying_K, '*.K.#', '*.K'), fixed_growth_pars$k, lower = 0.05, upper = 0.2, optimise = estimate_k) |>
  g3_init_val('*.t0', ifelse(estimate_t0, -1, fixed_growth_pars$t0), lower = -2, upper = 0, optimise = estimate_t0) |>
  g3_init_val('*.recl', 15, lower = 10, upper = 40) |>  #     Using t0 (g3a_renewal_vonb_t0) instead of recl (g3a_renewal_vonb_recl)
  g3_init_val('*.recl.cv', init.cv[init.cv$age == g3_stock_def(imm_stock, 'minage'), 'cv'][[1]], lower = 0.05, upper = 0.5, optimise = estimate_reccv) |>  #     Using t0 (g3a_renewal_vonb_t0) instead of recl (g3a_renewal_vonb_recl)
  g3_init_val('*.bbin', 500, lower = 1e-03, upper = 1000) |> 
  g3_init_val('*.M.#', naturalmortality) |> 
  
  ## Fleet selection parameters
  #  S-shaped
  g3_init_val('*.alpha', 0.1, lower = 0.01, upper = 1.5) |> 
  g3_init_val('*.l50', mean(do.call('c', sapply(stocks, g3_stock_def, 'midlen'))), spread = 0.5) |> 
  g3_init_val('*.alpha.#', 0.1, lower = 0.01, upper = 1.5) |> 
  g3_init_val('*.l50.#', mean(do.call('c', sapply(stocks, g3_stock_def, 'midlen'))), spread = 0.5) |> 
  #  Dome-shaped
  g3_init_val('*.p1', log(2), lower = 0, upper = 3) |>
  g3_init_val('*.p3', 0.1, lower = 0.001, upper = 10) |>
  g3_init_val('*.p4', 0.02, lower = 0.001, upper = 1e3) |>
  ## Quota
  g3_init_val('*.quota.prop', 1, optimise = FALSE) |> 
  g3_init_val('*.cons.step.#', 0.25, optimise = FALSE) |> 
  g3_init_val("*.quota.step.#", 0.25, optimise = FALSE) |> 
  g3_init_val("*.stddev", 0, optimise = FALSE) |>
  
  ## Maturity ogives
  #  For initial conditions if init_abund_mode = 1
  g3_init_val('*.mat_initial_alpha', 0.5, lower = 0.01, upper = 1.5) |> 
  g3_init_val('*.mat_initial_a50', 10) |> 
  #  For maturation
  g3_init_val('*.mat.alpha', 0.07, lower = 0.01, upper = 0.2) |>
  g3_init_val('*.mat.l50', mat.l50$l50, spread = 0.5) |> 
  
  ## Random effects/penalities
  g3_init_val('bling_recruitment_sigma', recruitment_sigma) |> 
  # g3_init_val('initial_sigma', 0.2, 0.01, 10, ifelse(penalise_initial == 0, 1, 0)) |> 
  g3_init_val('rnd_recruitment_weight', 1) |> 
  # g3_init_val('rnd_initial_weight', 1, 0.01, 100, 0) |> 
  g3_init_val('zero', 0) |> 
  
  ## Weight-length
  g3_init_val('*.walpha', lw.constants$estimate[1]) |> 
  g3_init_val('*.wbeta', lw.constants$estimate[2]) |> 
  
  ## Recruitment/Spawning
  g3_init_val('bling_rec_scalar', value = 20, spread = 0.99) |> 
  g3_init_val('bling_rec_std', value = recruitment_std, optimise = FALSE) |>
  g3_init_val('bling_rec.*', value = log(50), lower = log(0.01), upper = log(100)) |> 
  ## ARIMA
  g3_init_val('*.rec.proj.logar1.logphi', value = arima_phi, lower = -1, upper = 1, optimise = estimate_phi) |> 
  g3_init_val('*.rec.proj.logar1.lstddev', value = log(recruitment_std), optimise = FALSE) 



## Create narrower bounds for time-varying parameters
if (timevarying_K) tmb_param <- g3_init_guess(tmb_param, '\\.K', 0.12, 0.1, 0.14, 1)

## --------------------------------------------------------------------------

## Run the R-model
result <- model(tmb_param$value)
result[[1]]

#gadgetplots::gadget_plots(g3_fit(model, tmb_param), file_type = 'html', path = 'tmp2', template = 'iceland_bling')

# List all available reports
print(names(attributes(result)))

## Write out parameters and both models
save(tmb_param, file = file.path(base_dir, vers, 'tmb_param.Rdata'))
save(model, file = file.path(base_dir, vers, 'r_model.Rdata'))
save(tmb_model, file = file.path(base_dir, vers, 'tmb_model.Rdata'))

## -----------------------------------------------------------------------------

if (FALSE){
  
  tmp <- params_final[grepl('weight$', params_final$switch), 'switch']
  tmp <- tmp[tmp %in% tmb_param$switch]
  tmb_param$value[tmp] <- params_final$value[tmp]
  rm(tmp)
  
  # Compile and generate TMB ADFun (see ?TMB::MakeADFun)
  obj.fun <- g3_tmb_adfun(tmb_model, 
                          jitter_params(tmb_param), 
                          inner.control = newton_control)
  
  obj.fun$env$tracepar <- TRUE
  
  if (usenlminb){
    out <- nlminb(start = obj.fun$par, 
                  objective = obj.fun$fn, 
                  gradient = obj.fun$gr, 
                  control = nlminb_control)
  }else{
    out <- optim(par = obj.fun$par, 
                 fn = obj.fun$fn, 
                 gr = obj.fun$gr,
                 method = 'BFGS',
                 control = c(random_control, 
                             list(parscale = g3_tmb_parscale(tmb_param)))
    )  
  }
  
  sdout <- TMB::sdreport(obj.fun, out$par)
  
  save(tmb_model, tmb_param, 
       obj.fun,
       out, sdout, file = file.path(base_dir, vers, 'randomout.Rdata'))
  
  ## Merge estimated parameters into template
  pars <- summary(sdout, 'all')[,1]
  tmbpars <- g3_tmb_relist(tmb_param, pars[match(names(g3_tmb_par(tmb_param)), names(pars))])
  modcpp <- g3_to_tmb(c(attr(tmb_model, 'actions'), list(g3a_report_detail(attr(tmb_model, 'actions')))))
  newpars <- attr(modcpp, 'parameter_template')
  newpars$value[names(tmbpars)] <- tmbpars
  newpars$value$report_detail <- 1L
  
  ## Fit
  fit <- g3_fit(g3_to_r(attr(modcpp, 'actions')), newpars)
  save(fit, file = file.path(base_dir, vers, 'fit.Rdata'))
  gadget_plots(fit, file.path(base_dir, vers, 'figs'), file_type = 'html', template = 'iceland_bling')
  
}
  
## -----------------------------------------------------------------------------

## Setup grouping for iterative re-weighting 
if (iter_group_SI){
  
  grouping <- list(sind = c('si_len020_AUT',
                            'si_len044_AUT',
                            'si_len056_AUT',
                            'si_len068_AUT',
                            'si_len080_AUT',
                            'si_len092_AUT',
                            'si_len104_AUT'))
  if (!single_fleet){
    grouping <- c(grouping,
                  list(bmt = c('ldist_bmt', if (bmt_age) 'aldist_bmt' else NULL),
                       aut = c('ldist_aut', if (iter_group_AUT) 'matp_aut' else NULL)))
  }else{
    grouping <- c(grouping, list(comm = c('ldist_comm')))
  }
}else{
  
  grouping <- list(sind1 = c('si_len020_AUT',
                             'si_len044_AUT',
                             'si_len056_AUT'),
                   sind2 = c('si_len068_AUT',
                             'si_len080_AUT',
                             'si_len092_AUT',
                             'si_len104_AUT'))
  if (!single_fleet){
    grouping <- c(grouping,
                  list(bmt = c('ldist_bmt', if (bmt_age & iter_group_BMTAGE) 'aldist_bmt' else NULL),
                       aut = c('ldist_aut', if (iter_group_AUT) 'matp_aut' else NULL)))
  }else{
    grouping <- c(grouping, list(comm = c('ldist_comm')))
  } 
}

## Iterative re-weighting
if (run_iterative){
 
  # Compile and generate TMB ADFun (see ?TMB::MakeADFun)
  obj.fun <- g3_tmb_adfun(tmb_model, tmb_param)
   
  ## Run iterative re-weighting
  params.out <- g3_iterative(file.path(base_dir, vers),
                             wgts = wgts_folder,
                             model = tmb_model,
                             params.in = tmb_param,
                             grouping = grouping,
                             method = 'BFGS',
                             control = optim_control,
                             use_parscale = TRUE,
                             shortcut = FALSE,
                             cv_floor = cv_floor,
                             resume_final = FALSE)
  
  
  ## Get the model fit
  fit <- g3_fit(model, params.out |> g3_init_guess('project_years', 1))
  save(fit, file = file.path(base_dir, vers, 'WGTS/fit.Rdata'))
  gadget_plots(fit, file.path(base_dir, vers, 'figs'), file_type = 'html', template = 'iceland_bling')
  
  sdout <- TMB::sdreport(obj.fun, g3_tmb_par(params.out))
  save(sdout, file = file.path(base_dir, vers, 'WGTS/sdout.Rdata'))
  
}

if (run_retro){
  cat("\n\n------------- RETROSPECTIVE ANALYSIS -------------\n\n")
  cat("------------- Setting up retro -------------\n")  
  if (!dir.exists(file.path(base_dir, vers, retro_folder))){
    dir.create(file.path(base_dir, vers, retro_folder))
  }
  
  ## Load output parameters from iterative re-weighting
  load(file = file.path(base_dir, vers, 'WGTS/params_final.Rdata'))
  
  ## Take the weights and insert into initial params
  tmp <- params_final$value[grepl('_weight$', params_final$switch)]
  retro_pars <- tmb_param
  retro_pars$value[names(tmp)] <- tmp
  
  retro_model <- list()
  retro_params <- list()
  
  for(peel in 1:number_of_peels){
    
    source(file.path(base_dir, '00-setup', 'setup-likelihood.R'))  # Generates likelihood_actions
    
    retro_actions <- 
      c(stock_actions,
        fleet_actions,
        likelihood_actions,
        if (recruitment_free_pars && penalise_recruitment == 1) random_actions else NULL,
        time_actions)
    
    retro_model[[peel]] <- g3_to_tmb(c(retro_actions,
                                       list(g3a_report_detail(retro_actions)),
                                       list(gadget3::g3l_bounds_penalty(retro_actions))))
    
    retro_params[[peel]] <- params_final# retro_pars #gadgetutils::jitter_params(params_final)
    retro_params[[peel]]$value$retro_years <- peel
  } 
  
  peel <- 0
  cat("------------- Running retro -------------\n")  
  retro <- 
    parallel::mclapply(1:number_of_peels,function(x){
      g3_optim(retro_model[[x]],
               retro_params[[x]],
               control = optim_control)
    }, 
    mc.cores = parallel::detectCores())
  
  cat("------------- Collating retro results -------------\n")  
  retro_fit <- 
    1:number_of_peels |>
    set_names(paste0('r',1:number_of_peels)) |> 
    purrr::map(function(x) g3_fit(model = retro_model[[x]], params = retro[[x]] |> g3_init_guess('project_years',1))) 
  
  save(retro_fit, file = file.path(base_dir, vers,'RETRO', 'retro_fit.Rdata'))
  
  # retro <- g3_retro(file.path(base_dir, vers),
  #                   tmb_model,
  #                   params.out,
  #                   num.years = 10)
  
  gadget_plots(fit, file.path(base_dir, vers, 'figs'), file_type = 'html', retro_fit = retro_fit, template = 'iceland_bling')
  
  cat("------------- Retro complete -------------\n\n")
}

## -----------------------------------------------------------------------------
## Jitter
## -----------------------------------------------------------------------------

if (run_jitter){
  
  cat("\n\n------------- JITTER ANALYSIS -------------\n\n")
  cat("------------- Setting up jitter -------------\n")  
  
  ## Directory
  fs::dir_create(file.path(base_dir, vers, jitter_folder))
  
  ## Load the parameters that resulted from iterative re-weighting
  load(file.path(base_dir, vers, wgts_folder, 'params_final.Rdata'))
  pars.in <- params_final
  
  ## Re-adjust parameters to initial values
  if (!jitter_optimised_pars){
    pars.in$value[params_final$switch[(!grepl('_weight$', params_final$switch))]] <- 
      tmb_param$value[params_final$switch[(!grepl('_weight$', params_final$switch))]]
  } 
  
  cat("------------- Running jitter -----------------------\n")  
  jitter_out <- gadgetutils::g3_jitter(file.path(base_dir, vers),
                                       outdir = jitter_folder,
                                       model = tmb_model,
                                       params = pars.in,
                                       njits = number_of_jitters,
                                       jitter_fraction = jitter_fraction,
                                       control = optim_control,
                                       mc.cores = bootstrap_cores)
  
  cat("------------- Collating jitter results -------------\n") 
  
  jitter_fit <- 1:number_of_jitters |> purrr::map(function(x) try(g3_fit(model = tmb_model, 
                                                                          params = jitter_out[[x]] |> g3_init_guess('project_years',1))))
  save(jitter_fit, file = file.path(base_dir, vers, jitter_folder, 'jitter_fit.Rdata'))
  
  cat("------------- Jitter complete ----------------------\n")  
}

## -----------------------------------------------------------------------------
## Leave-out analysis
## -----------------------------------------------------------------------------

if (run_leaveout){
  
  ## Directory
  fs::dir_create(file.path(base_dir, vers, leaveout_folder))
  
  ## Load the parameters that resulted from iterative re-weighting
  load(file.path(base_dir, vers, wgts_folder, 'params_final.Rdata'))
  pars.in <- params_final
  
  ## Re-adjust parameters to initial values
  if (!leaveout_optimised_pars){
    pars.in$value[params_final$switch[(!grepl('_weight$', params_final$switch))]] <- 
      tmb_param$value[params_final$switch[(!grepl('_weight$', params_final$switch))]]
  } 
  
  leaveout_out <- gadgetutils::g3_leaveout(file.path(base_dir, vers),
                                           outdir = leaveout_folder,
                                           model = tmb_model,
                                           param = pars.in,
                                           grouping = groupings,
                                           control = optim_control)
  
  leaveout_fit <- 1:length(leaveout_out) |> purrr::map(function(x) try(g3_fit(model = tmb_model, params = leaveout_out[[x]])))
  save(leaveout_fit, file = file.path(base_dir, vers, leaveout_folder, 'leaveout_fit.Rdata'))
  
}

## -----------------------------------------------------------------------------
## profile M
## -----------------------------------------------------------------------------

if (run_mprofile){
  
  ## Directory
  profile_folder <- ifelse(profile_short, paste0(profile_folder, '_SHORT'), profile_folder)
  fs::dir_create(file.path(base_dir, vers, profile_folder))
  
  ## Take weights from params_final
  load(file.path(base_dir, vers, wgts_folder, 'params_final.Rdata'))
  pars.tmp <- params_final
  
  if (profile_short){
    pars.tmp$value[params_final$switch[(!grepl('_weight$', params_final$switch))]] <- 
      tmb_param$value[params_final$switch[(!grepl('_weight$', params_final$switch))]]
  }
  
  ## Create input parameters for each m
  pars.in <- list()
  for (i in seq_along(M_values)){
    pars.in[[i]] <- pars.tmp |> g3_init_val('*.M.#', M_values[i])
  }
  names(pars.in) <- paste0('M', M_values)
  
  profile_run <- 
    parallel::mclapply(setNames(names(pars.in), names(pars.in)),function(x) {
      if(profile_short){
        try(g3_optim(tmb_model, pars.in[[x]], control = optim_control))
                     #gadgetutils::jitter_params(pars.in[[x]]), control = optim_control))
      } else {
        try(g3_iterative(file.path(base_dir, vers),
                         wgts = file.path(profile_folder, paste0('WGTS_', x)),
                         tmb_model,
                         pars.in[[x]],
                         grouping = groupings,
                         cv_floor = cv_floor,
                         #resume_final = TRUE,
                         control = optim_control,
                         use_parscale = TRUE,
                         mc.cores = 1))
      }
    }, mc.cores = bootstrap_cores)
  
  write.g3.file(bind_rows(lapply(profile_run, function(x) attr(x, 'summary')), .id = "id"), base_dir, file.path(vers, profile_folder, 'summary.txt'))
  
  save(profile_run, file = file.path(base_dir, vers, profile_folder, 'profile_run.Rdata')) 
  
  profile_fit <- 
    1:length(profile_run) |>  
    set_names(paste0('m',1:length(profile_run))) |> 
    purrr::map(function(x) try(g3_fit(model = model, params = profile_run[[x]])))
  
  save(profile_fit, file = file.path(base_dir, vers, profile_folder, 'profile_fit.Rdata'))  
  
}


if (run_growthprofile){
  
  # Compile and generate TMB ADFun (see ?TMB::MakeADFun)
  obj.fun <- g3_tmb_adfun(tmb_model, tmb_param)
  
  ## Directory
  fs::dir_create(file.path(base_dir, vers, gw_profile_folder))
  
  ## Create input parameters for each growth combination
  pars.in <- list()
  for (i in 1:nrow(gw_values)){
    pars.in[[i]] <- 
      tmb_param |> 
      g3_init_val('*.K', gw_values[i,'K'], optimise = FALSE) |> 
      g3_init_val('*.Linf', gw_values[i,'Linf'], optimise = FALSE) 
  }
  names(pars.in) <- paste0('K', gw_values$K, '_Linf', gw_values$Linf)
 
  gw_profile_run <- 
    parallel::mclapply(setNames(names(pars.in), names(pars.in)),function(x) {
      try(g3_iterative(file.path(base_dir, vers),
                       wgts = file.path(gw_profile_folder, paste0('WGTS_', x)),
                       tmb_model,
                       pars.in[[x]],
                       grouping = grouping,
                       cv_floor = cv_floor,
                       #resume_final = TRUE,
                       control = optim_control,
                       use_parscale = TRUE,
                       mc.cores = 1))
      }, mc.cores = bootstrap_cores)
  
  write.g3.file(bind_rows(lapply(gw_profile_run, function(x) attr(x, 'summary')), .id = "id"), base_dir, file.path(vers, gw_profile_folder, 'summary.txt'))
  
  save(gw_profile_run, file = file.path(base_dir, vers, gw_profile_folder, 'gw_profile_run.Rdata')) 
  
  gw_profile_fit <- 
    1:length(gw_profile_run) |>  
    set_names(paste0('k_linf_',1:length(gw_profile_run))) |> 
    purrr::map(function(x) try(g3_fit(model = model, params = gw_profile_run[[x]])))
  
  save(gw_profile_fit, file = file.path(base_dir, vers, gw_profile_folder, 'gw_profile_fit.Rdata'))  
  
}


################################################################################
##### Setup book strap #########################################################
################################################################################

if(read_bootstrap_data){
  
  load(file = 'benchmarks/WKBDEEP/gadget3/data/nodata_divs.Rdata')
  
  ## Get new divisions
  newreits <- 
    reitmapping |> 
    filter(!(SUBDIVISION %in% nodata_divs))
  
  ## First - setup defaults
  defaults <- 
    within(defaults,
           {area = mfdb_bootstrap_group(nboots,
                                        mfdb_group('1' = unique(newreits$SUBDIVISION)),
                                        seed = 2298)})
  
  defaults$save_bootstrap_data <- TRUE
  mdb <- mfdb('Iceland', db_params = list(host = 'mfdb.hafro.is'))
  source(file.path(base_dir, '00-setup', 'setup-catchdistribution.R'))
  source(file.path(base_dir, '00-setup', 'setup-indices.R'))
  mfdb::mfdb_disconnect(mdb)
  
}

if (run_bootstrap){
  
  ## Directory for output
  boot_folder <- ifelse(boot_short, paste0(boot_folder, '_SHORT'), boot_folder)
  fs::dir_create(file.path(base_dir, vers, boot_folder))
  
  load(file = file.path(base_dir, vers, 'WGTS/params_final.Rdata'))
  
  ## Read in the bootstrap data
  fs::dir_ls(file.path(base_dir, 'data')) |>
    stringr::str_subset('.Rdata') |>
    stringr::str_subset('bootstrap_') |> 
    lapply(load,.GlobalEnv)
  
  ## Create a list of models nboots long
  boot_model <- list()
  for(boot_repl in 1:nboots){
    source(file.path(base_dir, '00-setup', 'setup-likelihood.R'))  # Generates likelihood_actionsleave out analysis
    boot_actions <- 
      c(stock_actions,
        fleet_actions,
        likelihood_actions,
        if (recruitment_free_pars && penalise_recruitment == 1) random_actions else NULL,
        time_actions)
    
    boot_actions <- c(boot_actions,
                      list(g3a_report_detail(boot_actions)),
                      list(g3l_bounds_penalty(boot_actions)))
    
    boot_model[[boot_repl]] <- g3_to_tmb(c(boot_actions))
    
  } 
  
  ## Save the models
  save(boot_model, file=file.path(base_dir, vers, boot_folder, 'boot_model.Rdata'))
  
  boot_run <- 
      parallel::mclapply(1:nboots,function(x) {
        
        if(boot_short){
          try(g3_optim(boot_model[[x]], params_final, control = optim_control)) 
        } else {
          try(g3_iterative(file.path(base_dir, vers),
                           wgts = paste('BOOTSTRAP/WGTS',x,sep='_'),
                           boot_model[[x]],
                           params_final,
                           grouping = grouping,
                           cv_floor = cv_floor,
                           #resume_final = TRUE,
                           control = control_list,
                           use_parscale = TRUE,
                           mc.cores = 1))
        }
      },
      mc.cores = bootstrap_cores)
  
  save(boot_run, file = file.path(base_dir, vers, boot_folder, 'boot_run.Rdata')) 
  save(boot_model, file = file.path(base_dir, vers, boot_folder, 'boot_model.Rdata')) 
  
  
  boot_fit <- 
    1:nboots |>  
    set_names(paste0('bs',1:nboots)) |> 
    purrr::map(function(x) try(g3_fit(model = boot_model[[x]] ,
                                      params = boot_run[[x]] |> g3_init_guess('project_years',1))))
  
  save(boot_fit, file = file.path(base_dir, vers, boot_folder, 'boot_fit.Rdata'))     
  gadget_plots(fit, 
               file.path(base_dir, vers, 'figs2'), 
               file_type = 'html', 
               retro_fit = retro_fit, 
               boot_fit = boot_fit,
               template = 'iceland_bling')
  
  
}
