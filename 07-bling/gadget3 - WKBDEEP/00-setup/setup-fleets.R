## -----------------------------------------------------------------------------
##
## Configure fleets and associated actions
##
## -----------------------------------------------------------------------------

## FLEET STOCKS
## Longline
lln <- 
  g3_fleet('lln') |> 
  g3s_livesonareas(areas[c('1')])

## Bottom trawl
bmt <- 
  g3_fleet('bmt') |> 
  g3s_livesonareas(areas[c('1')])

## Foreign
foreign <- 
  g3_fleet('foreign') |> 
  g3s_livesonareas(areas[c('1')])

## Autumn survey
aut <- 
  g3_fleet('aut') |>
  g3s_livesonareas(areas[c('1')])

## Older surveys with age data
deep_surveys <- 
  g3_fleet('deep') |> 
  g3s_livesonareas(areas[c('1')])

## For single fleet option
comm <- 
  g3_fleet('comm') |> 
  g3s_livesonareas(areas[c('1')])

if (single_fleet) fleets <- list(comm, foreign) else fleets <- list(bmt, lln, foreign)

## ------------------------------------------------------------------------------

quota <-
  gadget3::g3_quota(
    gadget3::g3_quota_hockeyfleet(
      predstocks = fleets,
      preystocks = list(mat_stock),
      btrigger = g3_parameterized('hf.btrigger', value = 1, optimise = FALSE, by_stock = mat_stock),
      harvest_rate = g3_parameterized('hf.harvest_rate', value = 0, optimise = FALSE, by_stock = fleets)
      ),
    year_length = 1L,
    start_step = 3L,
    init_val = 0,
    run_revstep = -2,
    run_f = quote(cur_year >= end_year - 1),
    )


## Fleet actions
fleet_actions <- 
  list(
    if (single_fleet){
      comm |> 
        g3a_predate_fleet(stocks,
                          suitabilities = {
                            if (dome_comm)
                              gadget3::g3_suitability_andersenfleet(
                                p5 = max(sapply(stocks, g3_stock_def, 'maxmidlen')),
                                by_stock = stocks)
                            else  g3_suitability_exponentiall50(by_stock = stocks) 
                          },
                          catchability_f = 
                            gadget3:::g3a_predate_catchability_project(
                              quota_f = quota, 
                              landings_f = g3_timeareadata('comm_landings', comm_landings[[1]] |>
                                                                            mutate(area = as.numeric(area),
                                                                                   step = as.numeric(step),
                                                                                   year = as.numeric(year))),
                              unit = 'biomass')) 
      
    }else{
      c(
        lln |> 
          g3a_predate_fleet(stocks,
                            suitabilities = {
                              if (dome_lln)
                                gadget3::g3_suitability_andersenfleet(
                                  p5 = max(sapply(stocks, g3_stock_def, 'maxmidlen')),
                                  by_stock = stocks)
                              else  g3_suitability_exponentiall50(
                                alpha = g3_parameterized('alpha', 
                                                         by_year = timevarying_lln,
                                                         by_stock = stocks,
                                                         by_predator = TRUE,
                                                         offset = quote(0 * cur_year)),
                                l50 = g3_parameterized('l50', 
                                                       by_year = timevarying_lln,
                                                       by_stock = stocks,
                                                       by_predator = TRUE,
                                                       offset = quote(0 * cur_year))
                                ) 
                            },
                            catchability_f = 
                              gadget3:::g3a_predate_catchability_project(
                                quota_f = quota,
                                landings_f = g3_timeareadata('lln_landings', lln_landings[[1]] |>
                                                                              mutate(area = as.numeric(area),
                                                                                     step = as.numeric(step),
                                                                                     year = as.numeric(year))),
                                unit = 'biomass')),
        bmt |> 
          g3a_predate_fleet(stocks,
                            suitabilities = {
                              if (dome_bmt)
                                gadget3::g3_suitability_andersenfleet(
                                  p5 = max(sapply(stocks, g3_stock_def, 'maxmidlen')),
                                  by_stock = stocks)
                              else  g3_suitability_exponentiall50(
                                alpha = g3_parameterized('alpha', 
                                                         by_year = timevarying_bmt,
                                                         by_stock = stocks, 
                                                         by_predator = TRUE,
                                                         offset = quote(0 * cur_year)),
                                l50 = g3_parameterized('l50', 
                                                       by_year = timevarying_bmt,
                                                       by_stock = stocks,
                                                       by_predator = TRUE,
                                                       offset = quote(0 * cur_year))
                                ) 
                            },
                            catchability_f = 
                              gadget3:::g3a_predate_catchability_project(
                                quota_f = quota,
                                landings_f = g3_timeareadata('bmt_landings', bmt_landings[[1]] |>
                                                                              mutate(area = as.numeric(area),
                                                                                     step = as.numeric(step),
                                                                                     year = as.numeric(year))),
                                unit = 'biomass'))
      )
    },
    foreign  |> 
      g3a_predate_fleet(stocks,
                        suitabilities = {
                          if (dome_lln || dome_comm){
                            gadget3::g3_suitability_andersenfleet(
                              p1 = gadget3::g3_parameterized(ifelse(single_fleet, 'comm.andersen.p1', 'lln.andersen.p1'), by_stock = stocks),
                              p3 = gadget3::g3_parameterized(ifelse(single_fleet, 'comm.andersen.p3', 'lln.andersen.p3'), by_stock = stocks, exponentiate = TRUE),
                              p4 = gadget3::g3_parameterized(ifelse(single_fleet, 'comm.andersen.p4', 'lln.andersen.p4'), by_stock = stocks, exponentiate = TRUE),
                              p5 = max(sapply(stocks, g3_stock_def, 'maxmidlen')),
                              by_stock = stocks)
                          }else{
                            ## The foreign fleet will share the same longline suitability or, if single_fleet = TRUE, the commercial fleet 
                            gadget3::g3_suitability_exponentiall50(
                              gadget3::g3_parameterized(ifelse(single_fleet, 'comm.alpha', 'lln.alpha'), 
                                                        by_stock = stocks, 
                                                        by_year = timevarying_lln,
                                                        by_predator = FALSE,
                                                        offset = quote(0 * cur_year)),
                              gadget3::g3_parameterized(ifelse(single_fleet, 'comm.l50', 'lln.l50'), 
                                                        by_stock = stocks, 
                                                        by_year = timevarying_lln,
                                                        by_predator = FALSE,
                                                        offset = quote(0 * cur_year))) 
                            }
                          },
                        catchability_f = 
                          gadget3:::g3a_predate_catchability_project(
                            quota_f = quota,
                            landings_f = g3_timeareadata('foreign_landings', foreign_landings[[1]] |>
                                                                          mutate(area = as.numeric(area),
                                                                                 step = as.numeric(step),
                                                                                 year = as.numeric(year))),
                            unit = 'biomass')),
    aut |> 
      g3a_predate_fleet(stocks,
                        suitabilities = g3_suitability_exponentiall50(by_stock = stocks),
                        catchability_f = 
                          g3a_predate_catchability_totalfleet(
                            g3_timeareadata('aut_landings', aut_landings |>
                                              mutate(area = as.numeric(area),
                                                     step = as.numeric(step),
                                                     year = as.numeric(year))))),
    
    list()
  )
     
if (bmt_age){
  fleet_actions <- 
    c(
      fleet_actions,
      list(
        deep_surveys |> 
          g3a_predate_fleet(stocks,
                            suitabilities = g3_suitability_exponentiall50(by_stock = stocks),
                            catchability_f = 
                              g3a_predate_catchability_totalfleet(
                                g3_timeareadata('deep_landings', deep_landings |>
                                                  mutate(area = as.numeric(area),
                                                         step = as.numeric(step),
                                                         year = as.numeric(year)))))
      ),
      list()
    )
}
    
    
    