library(tidyverse)
library(gadget3)
library(g3experiments)

source("benchmarks/WKBDEEP/gadget3/00-setup/projections-functions.R")
source("benchmarks/WKBDEEP/gadget3/src/gadget_functions.R")
load("~/DAG/07-bling/benchmarks/WKBDEEP/gadget3/data/sexratio.Rdata")

## -----------------------------------
## Directory of model to project
## -----------------------------------

base_dir <- 'benchmarks/WKBDEEP/gadget3'
vers <- 'models/26-baseline_IFgr_Linf0_K0_t00_mlgg4_reccv0_penrec1_sd0.4_agedata0_t0_ff'
outpath <- file.path(base_dir, vers, 'PROJ')
if (!dir.exists(outpath)) dir.create(outpath)
## Load the fit and bootstrap
load(file = file.path(base_dir, vers, 'WGTS', 'fit.Rdata'))
#fit <- calc_ssb(bli_fit_update(fit), sexratio) 
fit <- bli_fit_update(fit) 

## ----------------------------------
## Options for running simulations
## ----------------------------------

rec_years <- 2001:2022           # years for re-sampling recruitment
harvest_rates <- seq(0, 0.8, by = 0.01)             # seq(0.00, 0.8, by = 0.01)
hr_trials <- 1                   # how many trials per harvest rate
ncores <- 50
project_years <- 100
f_age_range <- 10:20
rec_age <- 5
block_size <- 5

## Estimate Blim


## Run the simulations
load(file = file.path(base_dir, vers, 'BOOTSTRAP_SHORT', 'boot_fit.Rdata'))
boot_fit <- boot_fit |> purrr::discard(~class(.)[1] == 'try-error')             # Remove errored boots
bootfit <- map(boot_fit, .f = bli_fit_update)
## Load the model 
load(file = file.path(base_dir, vers, 'tmb_model.Rdata'))

source(file.path(base_dir, '00-setup/projections-sim.R'))
gc()
source(file.path(base_dir, '00-setup/projections-refpoints.R'))

################################################################################