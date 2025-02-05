library(tidyverse)
library(gadget3)
library(g3experiments)

source("benchmarks/WKBDEEP/gadget3/00-setup/projections-functions.R")

## -----------------------------------
## Directory of model to project
## -----------------------------------

base_dir <- 'benchmarks/WKBDEEP/gadget3'
vers <- 'models/23-baseline_IFgr_Linf1_K1_t00_mlgg4_reccv0_penrec1_sd0.4_agedata1_t0'
outpath <- file.path(base_dir, vers, 'PROJ')
if (!dir.exists(outpath)) dir.create(outpath)
## Load the fit and bootstrap
load(file = file.path(base_dir, vers, 'WGTS', 'fit.Rdata'))

## ----------------------------------
## Options for running simulations
## ----------------------------------

rec_years <- 2001:2022           # years for re-sampling recruitment
harvest_rates <- seq(0, 0.8, by = 0.01)             # seq(0.00, 0.8, by = 0.01)
hr_trials <- 10                   # how many trials per harvest rate
ncores <- 35
project_years <- 100
f_age_range <- 10:20
rec_age <- 5

## Estimate Blim


## Run the simulations
load(file = file.path(base_dir, vers, 'BOOTSTRAP_SHORT', 'boot_fit.Rdata'))
boot_fit <- boot_fit |> purrr::discard(~class(.)[1] == 'try-error')             # Remove errored boots
## Load the model 
load(file = file.path(base_dir, vers, 'tmb_model.Rdata'))

source(file.path(base_dir, '00-setup/projections-sim.R'))
gc()
source(file.path(base_dir, '00-setup/projections-refpoints.R'))

################################################################################