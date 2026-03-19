## -----------------------------------------------------------------------------
##
## Runner to calculate the annual advice for blue ling in 5a and 14
##
## -----------------------------------------------------------------------------

library(mfdb)
library(gadget3)
library(tidyverse)
library(gadgetutils)
library(gadgetplots)
library(g3experiments)

## Functions and data to update fit
source('R/model_functions.R')
load('assessment_model/data/sexratio.Rdata')

## Read in reference points
ref_points <- read_csv('advice/tables/ref_points.csv')

################################################################################
## THIS NEEDS TO BE UPDATED EACH YEAR
TAC_lastyear <- 307*1e3

## -----------------------------------------------------------------------------

## Some settings
tyr <- lubridate::year(Sys.Date())
num_project_years <- 10
hr_target_range <- seq(0.00,0.5,by=0.01)        ## Gadget works with harvest rates, so we need to find the harvest rate that produces F_msy
age_range <- 10:20                              ## F age range

## Read in assessment model and fit
base_dir <- 'assessment_model'
vers <- sprintf('model/nwwg_%s',tyr)
load(file = file.path(base_dir, vers, 'WGTS/params_final.Rdata'))
load(file = file.path(base_dir, vers, 'r_model.Rdata'))
load(file = file.path(base_dir, vers, 'WGTS/fit.Rdata'))
load(file = file.path(base_dir, vers, 'defaults.Rdata'))

## Update the fit
fit <- calc_ssb(bli_fit_update(fit), sexratio)
## Fbar for last year - used for a catch scenario
fbar_tyr <- fit$res.by.year |> filter(year == (tyr-1)) |> pull(F) |> max()

## STOCKS
single_stock_model <- FALSE
species_name <- 'bli'
source('assessment_model/00-setup/setup-stocks.R')

## FLEETS AND CATCHES
## Interim catches
source('assessment_model/00-setup/setup-interim_catch.R')

## Fleet proportions for projected years based on mean proportions from last 7 years
fleet_props <- 
  fit$fleet.info |> 
  filter(fleet != 'foreign', fleet != 'aut') |> 
  group_by(year, fleet) |> 
  summarise(harv.rate = mean(harv.rate, na.rm=TRUE)) |> 
  drop_na(harv.rate) |> 
  filter(!is.infinite(harv.rate)) |> 
  group_by(year) |> 
  mutate(prop = harv.rate/sum(harv.rate)) |> 
  filter(year > (tyr-8), year < (tyr)) |> 
  group_by(fleet) |> 
  summarise(prop = mean(prop, na.rm=TRUE))

## Merge fleet props with int_catch
int_catch <- 
  int_catch_tmp |> 
  cross_join(fleet_props) |> 
  mutate(total_weight = total*prop) |> 
  select(year, step, area, fleet, total_weight)
  
## SETUP INTERIM FLEETS AND THEIR ACTIONS

inter_bmt <- 
  g3_fleet(c('bmt', 'inter')) |> 
  g3s_livesonareas(areas[c('1')])

inter_lln <- 
  g3_fleet(c('lln', 'inter')) %>%
  g3s_livesonareas(areas[c('1')])

## Create actions for the fleets
inter_fleet_actions <- 
  list(inter_bmt %>%  
         g3a_predate(
           stocks,
           suitabilities = g3_suitability_exponentiall50(g3_parameterized('bli.bmt.alpha'),
                                                         g3_parameterized('bli.bmt.l50')),
           catchability_f = g3a_predate_catchability_totalfleet(
             E = g3_timeareadata('int_bmt_landings', 
                                 int_catch |> 
                                   filter(fleet == 'bmt') |> 
                                   select(year, step, area, total_weight))
           ),
           run_f = g3_formula(cur_year == tyr, tyr = local(tyr))),
       
       inter_lln %>%  
         g3a_predate(
           stocks,
           suitabilities = g3_suitability_exponentiall50(g3_parameterized('bli.lln.alpha'),
                                                         g3_parameterized('bli.lln.l50')),
           catchability_f = g3a_predate_catchability_totalfleet(
             E = g3_timeareadata('int_gil_landings', 
                                 int_catch |> 
                                   filter(fleet == 'lln') |> 
                                   select(year, step, area, total_weight))
           ),
           run_f = g3_formula(cur_year == tyr, tyr = local(tyr)))
       )
       
## COMPILE MODEL
proj_actions <- c(attr(model, 'actions'), inter_fleet_actions)
tmb_proj <- g3_to_tmb(c(proj_actions, list(g3a_report_detail(proj_actions))))
r_proj <- g3_to_r(attr(tmb_proj, 'actions'))

## Setup a list of input parameters (1 for each harvest rate tested)
input_pars <- g3p_setup_pars(r_proj,
                             fit$params |> filter(switch != 'quota_leftover'),
                             blim = ref_points$B_lim*1e3,
                             btrigger = ref_points$MSY_btrigger*1e3,
                             harvest_rates = hr_target_range,
                             harvest_rate_trials = 1,
                             fleet_proportions = fleet_props,
                             rec_list = list(base = fit$stock.recruitment |> filter(stock == 'bli_imm')),
                             rec_years = (tyr-3):(tyr-1),
                             rec_method = 'constant',
                             project_years = num_project_years,
                             assess_err = FALSE,
                             hr_pattern = 'project_hr.[0-9]')

proj_runs <- 
  parallel::mclapply(input_pars, function(x){
    out <- g3_fit(r_proj, x)
    return(out)
    }, mc.cores = parallel::detectCores(logical = TRUE)) |> 
  discard(~class(.)[[1]]=='try-error') %>% 
  discard(~length(.)==0)
  
## Collate the runs
prognosis <- 
  ## Total biomass
  gadgetutils::bind_fit_components(proj_runs, 'res.by.year') |> 
  filter(stock %in% c('bli_mat', 'bli_imm')) |> 
  group_by(year, id) |> 
  summarise(tb = sum(total.biomass), 
            catch = sum(catch), .groups = 'drop') |> 
  ## SSB and F from mature stock
  left_join(
    proj_runs %>% 
      discard(~class(.)[[1]]=='try-error') %>% 
      discard(~length(.)==0) |>
      map(bli_fit_update) %>% 
      map(calc_ssb, sexratio) |> 
      map('res.by.year') |> 
      bind_rows(.id = 'id') |> 
      filter(stock == 'bli_mat') |> 
      select(year, id, ssb = total.biomass, fbar = F) 
  ) |>  
  ## Recruitment
  left_join(
    proj_runs %>% 
      discard(~class(.)[[1]]=='try-error') %>% 
      discard(~length(.)==0) |>
      map(bli_fit_update) %>% 
      map(calc_ssb, sexratio) |> 
      map('res.by.year') |> 
      bind_rows(.id = 'id') |> 
      filter(stock == 'bli_imm') |> 
      select(year, id, rec = recruitment) 
  )  |> 
  ## Advice
  left_join(
    proj_runs %>% 
      map(add_advice) %>% 
      map('advice') |> 
      bind_rows(.id = 'id') |> 
      mutate(year = substring(timabil, 1, 4) |> as.numeric()) |> 
      rename(timabil_fbar = F,
             timabil_catch = catch)
  ) |> 
  mutate(id = as.numeric(gsub('h-(.+)-(.+)-(+.)', '\\1', id))) |> 
  select(year, harvest_rate = id, tb, ssb, catch, fbar, rec,
         timabil, timabil_catch, timabil_fbar)

## Collate results:
## Scenario 1: F = 0
prognosis0 <-
  prognosis |> 
  filter(harvest_rate == 0, year %in% (tyr-3):(tyr+3)) |> 
  mutate(fbar = round(fbar, 2),
         timabil_fbar = round(timabil_fbar, 2),
         harvest_rate = case_when(year < local(tyr) ~ NA, .default = harvest_rate))

## Scenario 2: F = Fmsy
## Find appropriate HR
prognosis %>% 
  filter(year == (tyr + 1)) |> 
  mutate(fbar_diff = abs(local(ref_points$F_msy) - fbar)) |> 
  arrange(fbar_diff) |> 
  select(year, fbar, fbar_diff, harvest_rate)

prognosis_msy <-
  prognosis |> 
  filter(harvest_rate == 0.11, year %in% (tyr-3):(tyr+3)) |> 
  mutate(fbar = round(fbar, 2),
         timabil_fbar = round(timabil_fbar, 2),
         harvest_rate = case_when(year < local(tyr) ~ NA, .default = harvest_rate))

## Scenario 3: F = Fy-1
## Find appropriate HR
prognosis %>% 
  filter(year == (tyr + 1)) |> 
  mutate(fbar_diff = abs(local(fbar_tyr) - fbar)) |> 
  arrange(fbar_diff) |> 
  select(year, fbar, fbar_diff, harvest_rate)

prognosis_sq <-
  prognosis |> 
  filter(harvest_rate == 0.03, year %in% (tyr-3):(tyr+3)) |> 
  mutate(fbar = round(fbar, 2),
         timabil_fbar = round(timabil_fbar, 2),
         harvest_rate = case_when(year < local(tyr) ~ NA, .default = harvest_rate))

## TAC
TAC <- prognosis_msy %>% filter(year == tyr+1) %>% .$timabil_catch/1e3 %>% round()

save(proj_runs, TAC, prognosis0, prognosis_msy, prognosis_sq,
     file = file.path(base_dir,vers,'prognosis.Rdata'))

prognosis_msy %>% 
  mutate(approach = 'Fmsy') %>% 
  bind_rows(
    prognosis0 %>% 
      mutate(approach = 'Zero catch'),
    prognosis_sq %>% 
      mutate(approach = 'SQ')
  ) %>% 
  write_csv2(file.path('assessment_model','model',paste0('nwwg_',tyr),'catch_options.csv'))

