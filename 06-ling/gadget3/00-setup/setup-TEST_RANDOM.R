## -----------------------------------------------------------------------------
##
## Runner to build a Gadget3 model for Ling 
##
## -----------------------------------------------------------------------------

library(mfdb)
library(tidyverse)
library(gadget3)
library(gadgetplots)
library(gadgetutils)

base_dir <- '06-ling/gadget3'

## -----------------------------------------------------------------------------
## OPTIONS 
## -----------------------------------------------------------------------------

## Whether or not to call the setup-data scripts
read_data <- FALSE

## Whether or not to run iterative reweighting
run_iterative <- FALSE
run_retro <- FALSE
bootstrap <- FALSE

## -----------------------------------------------------------------------------
## PARAMETERS 
## -----------------------------------------------------------------------------

## Some model parameters...
year_range <- 1982:lubridate::year(Sys.Date())

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
#           project_years = 0L,
           defaults$timestep),
  list())


## Data and model folders
fs::dir_create(file.path(base_dir, c('data', 'model')))

## ------------------------------------------------------------------------------------

source(file.path(base_dir, '00-setup', 'setup-model.R'))  # Generates mat_stock_actions / imm_stock_actions
source(file.path(base_dir, '00-setup', 'setup-model_components.R'))  # Generates mat_stock_actions / imm_stock_actions

if(read_data){
  mdb <- mfdb('Iceland', db_params = list(host = 'mfdb.hafro.is'))
  #  mdb <- mfdb("../../mfdb/copy/iceland.duckdb")
  source(file.path(base_dir, '00-setup', 'setup-fleet-data.R'))
  source(file.path(base_dir, '00-setup', 'setup-catchdistribution.R'))
  source(file.path(base_dir, '00-setup', 'setup-indices.R'))
  source(file.path(base_dir, '00-setup', 'setup-initial_parameters.R'))
} else {
  fs::dir_ls(file.path(base_dir, 'data')) %>%
    stringr::str_subset('.Rdata') %>%
    lapply(load,.GlobalEnv)
}

##### Configure model actions #################################################

source(file.path(base_dir, '00-setup', 'setup-fleets.R'))  # Generates fleet_actions
#source(file.path(base_dir, '00-setup', 'setup-fleets_prognosis.R'))  # Generates fleet_actions
#source(file.path(base_dir, '00-setup', 'setup-tagging.R'))  # Generates fleet_actions
source(file.path(base_dir, '00-setup', 'setup-likelihood.R')) # Generates likelihood_actions (alpha and beta estimated by lm)
source(file.path(base_dir, '00-setup', 'setup-randomeffects.R')) # Generates likelihood_actions (alpha and beta estimated by lm)


## Stock actions
stock_actions <- c(initial_conditions_imm,
                   natural_mortality_imm,
                   ageing_imm,
                   renewal_imm,
                   growmature_imm,
                   list(),
                   initial_conditions_mat,
                   natural_mortality_mat,
                   ageing_mat,
                   growmature_mat,
                   random_actions3,
                   #spawning,
                   list())

## Collate actions
actions <- c(
  stock_actions,
  fleet_actions,
 # tagging_actions,
  likelihood_actions,
  time_actions)

actions <- c(actions, list(
  g3a_report_history(actions, '^ling_(imm|mat)__(num|wgt|igfs|lln|bmt|gil|foreign|suit_igfs|renewalnum|renewalwgt)$')))

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
tmb_param <- 
  tmb_param %>% 
  g3_init_guess('\\.rec', 50, 0.001, 120, 1) %>% 
  g3_init_guess('\\.init', 50, 0.001, 120, 1) %>% 
  g3_init_guess('recl', 4.5, 4, 10, 1) %>% 
  g3_init_guess('rec.sd', 5, 4, 20, 1) %>% 
  g3_init_guess('rec.scalar', 250, 1, 500, 1) %>% 
  g3_init_guess('init.scalar', 150, 1, 300, 1) %>% 
  g3_init_guess('Linf', 110, 100, 120, 0) %>% 
  g3_init_guess('\\.K', 70, 60, 80, 1) %>% 
  g3_init_guess('bbin', 6, 1e-08, 100, 1) %>% 
  g3_init_guess('\\.alpha', 0.5, 0.01, 1, 1) %>% 
  g3_init_guess('l50', 50, 10, 100, 1) %>% 
  g3_init_guess('init.F', 0.4, 0.1, 1, 1) %>% 
  g3_init_guess('\\.M', 0.15, 0.001, 1, 0) %>% 
  #    g3_init_guess('mat_initial_alpha', 1, 0.5, 2, 1) %>% 
  #    g3_init_guess('mat_initial_a50', mat.a50$a50, 1, 18, 0) %>% 
  g3_init_guess('prop_mat0', 0.5, 0.1, 0.9, 0) %>%
  g3_init_guess('B0', 100, 1, 5000, 1) %>%
  g3_init_guess('mat_alpha', 70, 10, 200, 1) %>% 
  g3_init_guess('mat_l50', mat.l50$l50, 0.75*mat.l50$l50, 1.25*mat.l50$l50, 1) %>% 
  g3_init_guess('sigma_alpha', init.sigma.coef[['alpha']], -1, 1, 0) %>%
  g3_init_guess('sigma_beta', init.sigma.coef[['beta']], 0, 2, 0) %>%
  g3_init_guess('sigma_gamma', init.sigma.coef[['gamma']], 0, 1, 0) %>%
  g3_init_guess('walpha', lw.constants$estimate[1], 1e-10, 1, 0) %>% 
  g3_init_guess('wbeta', lw.constants$estimate[2], 2, 4, 0) 

## Initial sd's
if (any(grepl('\\.init\\.sd', tmb_param$switch))){
  
  tmb_param[grepl('mat\\.init\\.sd', tmb_param$switch), 'value'] <-
    init.sigma %>% filter(age %in% 
                            gadget3:::stock_definition(mat_stock, 'stock__minage'): 
                            gadget3:::stock_definition(mat_stock, 'stock__maxage')) %>% .$ms
  
  tmb_param[grepl('imm\\.init\\.sd', tmb_param$switch), 'value'] <-
    init.sigma %>% filter(age %in% 
                            gadget3:::stock_definition(imm_stock, 'stock__minage'): 
                            gadget3:::stock_definition(imm_stock, 'stock__maxage')) %>% .$ms
  ## Turn off optimisation
  tmb_param <-
    tmb_param %>% 
    mutate(optimise = case_when(grepl('init.sd', switch) ~ FALSE,
                                grepl('.M.[\\.[0-9]', switch) ~ FALSE,
                                TRUE~optimise))
  
}


## Run the R-model
result <- model(tmb_param$value)
result[[1]]

# List all available reports
print(names(attributes(result)))

# Compile and generate TMB ADFun (see ?TMB::MakeADFun)
obj.fun <- g3_tmb_adfun(tmb_model,tmb_param)

# Run model once, using g3_tmb_par to reshape tmb_param into param vector.
# Will return nll
obj.fun$fn(g3_tmb_par(tmb_param))
  
# Run model once, returning model report
obj.fun$report(g3_tmb_par(tmb_param))
  
# Run model through R optimiser, using bounds set in tmb_param
fit.opt <- optim(obj.fun$par,
                 obj.fun$fn,
                 obj.fun$gr,
                 #lower = gadget3:::g3_tmb_bound(tmb_param, 'lower', include_random=T),
                 #upper = gadget3:::g3_tmb_bound(tmb_param, 'upper', include_random=T),
                 method = 'BFGS',
                 control = list(trace = 2,
                                maxit = 10, 
                                reltol = .Machine$double.eps^2))
  
fit <- gadget3:::g3_fit(model, g3_tmb_relist(tmb_param, fit.opt$par))
  
 
