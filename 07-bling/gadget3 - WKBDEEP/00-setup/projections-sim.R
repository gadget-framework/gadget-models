## -----------------------------------------------------------------------------
## Run the simulations
## -----------------------------------------------------------------------------


## Blim, Bpa, Btrigger
blim <- round(fit$res.by.year |> filter(stock == 'bli_mat', year == 2001) |> pull(total.biomass)/1e3)
bpa  <- blim * exp(1.645 * 0.2)
btrigger <- bpa

## -----------------------------------------------------------------------------

## Fleet proportions
fleet_props <- 
  fit$fleet.info |> 
  filter(fleet != 'foreign', fleet != 'aut') |> 
  group_by(year, fleet) |> 
  summarise(harv.rate = mean(harv.rate, na.rm=TRUE)) |> 
  drop_na(harv.rate) |> 
  filter(!is.infinite(harv.rate)) |> 
  group_by(year) |> 
  mutate(prop = harv.rate/sum(harv.rate)) |> 
  filter(year > 2016, year < 2024) |> 
  group_by(fleet) |> 
  summarise(prop = mean(prop, na.rm=TRUE))

#fleet_props <- data.frame(fleet = c('bmt', 'lln'), prop = c(0.5, 0.5))

## Gather the fit parameters and bootstrap parameters into a single list
param_list <- c(list(base = fit$params), boot_fit |> map('params'))

## Gather all the recruitment time-series
rec_list <- 
  c(list(base = fit$stock.recruitment), boot_fit |> 
      map('stock.recruitment')) |>
  map(filter, recruitment > 0)

## -----------------------------------------------------------------------------
## Precautionary reference points (flim): no assessment error and no Btrigger
## -----------------------------------------------------------------------------

projpar_pre <- g3p_setup_pars(tmb_model, param_list, 
                              blim*1e3, btrigger = 1,
                              harvest_rates, hr_trials,
                              fleet_props,
                              rec_list, rec_years = rec_years)

# projpar_pre[[1]] <- g3p_project_rec(projpar_pre[[1]],
#                                     fit$stock.recruitment |>
#                                       filter(stock == 'bli_imm') |>
#                                       drop_na(recruitment),
#                                     method = 'constant')

## Objective function
of <- g3_tmb_adfun(tmb_model, projpar_pre[[1]])

# model <- g3_to_r(attr(tmb_model, 'actions'))
# reports <- of$report(g3_tmb_par(projpar_pre[[1]]))
# reports2 <- of$report(g3_tmb_par(projpar_pre2[[1]]))
# reports3 <- of$report(g3_tmb_par(projpar_pre3[[1]]))
# model <- g3_to_r(attr(tmb_model, 'actions'))
# 
# (reports$dstart_bli_mat__num*reports$dstart_bli_mat__wgt) |> 
#   as.data.frame.table() |> 
#   gadgetutils::extract_year_step() |> 
#   group_by(year) |> 
#   summarise(b = sum(Freq)) |> 
#   mutate(b = b/1e6) |> 
#   ggplot(aes(year, b)) + geom_line()
# 
# (reports$dstart_bli_dummy__num) |> 
#   as.data.frame.table() |> 
#   gadgetutils::extract_year_step() |> 
#   group_by(year) |> 
#   summarise(b = sum(Freq)) |> 
#   mutate(b = b) |> 
#   ggplot(aes(year, b)) + geom_line()

print('Running precautionary rp projections')

results_pre <- g3p_run(obj_fun = of, params = projpar_pre, 
                       mat_stock = 'bli_mat', imm_stock = 'bli_imm',
                       f_ages = f_age_range, rec_age = rec_age,
                       ncores = ncores)

save(results_pre, file = file.path(outpath, 'results_pre.Rdata'))
save(projpar_pre, file = file.path(outpath, 'projpar_pre.Rdata'))

print('Finished running precautionary rp projections')

## -----------------------------------------------------------------------------
## MSY reference points (fmsy, fp0.5): assessment error and no btrigger
## -----------------------------------------------------------------------------

projpar_msy_nobtrigger <- g3p_setup_pars(tmb_model, param_list, 
                                         blim*1e3, btrigger = 1,
                                         harvest_rates, hr_trials,
                                         fleet_props,
                                         rec_list, rec_years = rec_years,
                                         assess_err = TRUE)

print('Running msy rp projections')

results_msy_nobtrigger <- g3p_run(obj_fun = of, params = projpar_msy_nobtrigger, 
                                  mat_stock = 'bli_mat', imm_stock = 'bli_imm',
                                  f_ages = f_age_range, rec_age = rec_age,
                                  ncores = ncores)

save(results_msy_nobtrigger, file = file.path(outpath, 'results_msy_nobtrigger.Rdata'))
save(projpar_msy_nobtrigger, file = file.path(outpath, 'projpar_msy_nobtrigger.Rdata'))

print('Finished running msy rp projections')

## -----------------------------------------------------------------------------
## MSY reference points (fmsy, fp0.5): assessment error and btrigger
## -----------------------------------------------------------------------------

projpar_msy <- g3p_setup_pars(tmb_model, param_list, 
                              blim*1e3, btrigger = btrigger*1e3,
                              harvest_rates, hr_trials,
                              fleet_props,
                              rec_list, rec_years = rec_years,
                              assess_err = TRUE)

print('Running msy rp with btrigger projections')

results_msy <- g3p_run(obj_fun = of, params = projpar_msy, 
                       mat_stock = 'bli_mat', imm_stock = 'bli_imm',
                       f_ages = f_age_range, rec_age = rec_age,
                       ncores = ncores)

save(results_msy, file = file.path(outpath, 'results_msy.Rdata'))
save(projpar_msy, file = file.path(outpath, 'projpar_msy.Rdata'))

print('Finished running msy rp with btrigger projections')

## -----------------------------------------------------------------------------

# tmp <- results_pre |> filter(year > 2125, trial == 5, hr_target == 0.51)
# 
# res |>
#   filter(year > 2025) |>
#   group_by(year, hr_target, trial) |>
#   summarise(c = sum(catch), ssb = mean(ssb), fbar = mean(fbar)) |>
#   group_by(hr_target) |>
#   summarise(yield = median(c), ssb = median(ssb), fbar = median(fbar)) |>
#   ggplot(aes(fbar, yield)) + geom_point()
# # 
# results_msy |>
#   filter(year > 2125) |>
#   group_by(year, hr_target, boot, trial) |>
#   summarise(c = sum(catch), ssb = mean(ssb), fbar = median(fbar)) |>
#   group_by(hr_target) |>
#   summarise(yield = median(c), ssb = median(ssb), fbar = median(fbar)) |>
#   ggplot(aes(fbar, yield)) + geom_point()
# # 
# # results_msy_nobtrigger |> 
# #   filter(year > 2170) |> 
# #   group_by(year, hr_target, boot, trial) |> 
# #   summarise(c = sum(catch), ssb = mean(ssb[step == 1]), fbar = median(fbar[step == 1])) |> 
# #   group_by(hr_target) |> 
# #   summarise(yield = median(c), ssb = median(ssb), fbar = median(fbar)) |> 
# #   ggplot(aes(fbar, yield)) + geom_point()
# # 
# 
# 
# #renew <- 
# res$detail_bli_dummy__spawnednum |> 
#   as.data.frame.table() |> 
#   extract_year_step() |> 
#   group_by(year, step) |> 
#   summarise(n = sum(Freq)) |> 
#   filter(step == 1) |> 
#   mutate(fh = case_when(year < 2024 ~ 'fit', .default = 'forecast')) |> 
#   ggplot(aes(n)) + 
#   geom_histogram() + 
#   facet_wrap(~fh)
# 
# res$detail_bli_dummy__spawnednum |> 
#   as.data.frame.table() |> 
#   extract_year_step() |> 
#   group_by(year, step) |> 
#   summarise(n = sum(Freq)) |> 
#   filter(step == 1, year < 2100) |> 
#   mutate(fh = case_when(year < 2024 ~ 'fit', .default = 'forecast')) |> 
#   ggplot(aes(year, n)) + 
#   geom_line(aes(col = fh))
# 
# test <- g3_fit(model, pars)
# 
# 
