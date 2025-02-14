## -----------------------------------------------------------------------------
##
## Configure fleets and associated actions
##
## -----------------------------------------------------------------------------

## FLEET STOCKS
## Longline
lln <- 
  g3_fleet('lln') %>% 
  g3s_livesonareas(areas[c('1')])

## Bottom trawl
bmt <- 
  g3_fleet('bmt') %>% 
  g3s_livesonareas(areas[c('1')])

## Foreign
foreign <- 
  g3_fleet('foreign') %>% 
  g3s_livesonareas(areas[c('1')])

## Autumn survey
aut <- 
  g3_fleet("aut") %>%
  g3s_livesonareas(areas[c("1")])

## For single fleet option
comm <- 
  g3_fleet("comm") %>% 
  g3s_livesonareas(areas[c("1")])

if (single_fleet) fleets <- list(comm, foreign) else fleets <- list(bmt, lln, foreign)

## ------------------------------------------------------------------------------

quota <-
  gadget3:::g3_quota(
    gadget3:::g3_quota_hockeyfleet(
      predstocks = fleets,
      preystocks = list(mat_stock),
      btrigger = g3_parameterized('hf.btrigger', value = 1, optimise = FALSE, by_stock = mat_stock),
      harvest_rate = g3_parameterized('hf.harvest_rate', value = 0, optimise = FALSE, by_stock = fleets)
      ),
    by_step = FALSE,
    run_step = 1,
    lag = 2,
    run_f = quote(cur_year >= end_year - 1),
    )


## Fleet actions
fleet_actions <- 
  list(
    if (single_fleet){
      comm %>% 
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
                              pre_projection_landings_f = g3_timeareadata('comm_landings', comm_landings[[1]] %>%
                                                                            mutate(area = as.numeric(area),
                                                                                   step = as.numeric(step),
                                                                                   year = as.numeric(year))),
                              unit = 'biomass')) 
      
    }else{
      c(
        lln %>% 
          g3a_predate_fleet(stocks,
                            suitabilities = {
                              if (dome_lln)
                                gadget3::g3_suitability_andersenfleet(
                                  p5 = max(sapply(stocks, g3_stock_def, 'maxmidlen')),
                                  by_stock = stocks)
                              else  g3_suitability_exponentiall50(by_stock = stocks) 
                            },
                            catchability_f = 
                              gadget3:::g3a_predate_catchability_project(
                                quota_f = quota,
                                pre_projection_landings_f = g3_timeareadata('lln_landings', lln_landings[[1]] %>%
                                                                              mutate(area = as.numeric(area),
                                                                                     step = as.numeric(step),
                                                                                     year = as.numeric(year))),
                                unit = 'biomass')),
        bmt %>% 
          g3a_predate_fleet(stocks,
                            suitabilities = {
                              if (dome_bmt)
                                gadget3::g3_suitability_andersenfleet(
                                  p5 = max(sapply(stocks, g3_stock_def, 'maxmidlen')),
                                  by_stock = stocks)
                              else  g3_suitability_exponentiall50(by_stock = stocks) 
                            },
                            catchability_f = 
                              gadget3:::g3a_predate_catchability_project(
                                quota_f = quota,
                                pre_projection_landings_f = g3_timeareadata('bmt_landings', bmt_landings[[1]] %>%
                                                                              mutate(area = as.numeric(area),
                                                                                     step = as.numeric(step),
                                                                                     year = as.numeric(year))),
                                unit = 'biomass'))
      )
    },
    foreign  %>% 
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
                            gadget3::g3_suitability_exponentiall50(gadget3::g3_parameterized(ifelse(single_fleet, 'comm.alpha', 'lln.alpha')),
                                                                   gadget3::g3_parameterized(ifelse(single_fleet, 'comm.l50', 'lln.l50'))) 
                            }
                          },
                        catchability_f = 
                          gadget3:::g3a_predate_catchability_project(
                            quota_f = quota,
                            pre_projection_landings_f = g3_timeareadata('foreign_landings', foreign_landings[[1]] %>%
                                                                          mutate(area = as.numeric(area),
                                                                                 step = as.numeric(step),
                                                                                 year = as.numeric(year))),
                            unit = 'biomass')),
    aut %>% 
      g3a_predate_fleet(stocks,
                        suitabilities = g3_suitability_exponentiall50(by_stock = stocks),
                        catchability_f = 
                          g3a_predate_catchability_totalfleet(
                            g3_timeareadata('aut_landings', aut_landings %>%
                                              mutate(area = as.numeric(area),
                                                     step = as.numeric(step),
                                                     year = as.numeric(year))))),
    
    list()
  )
     
  
    
    
    