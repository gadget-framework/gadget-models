## -----------------------------------------------------------------------------
##
## Fleet actions:
##
## -----------------------------------------------------------------------------

############ Configure fleets ##################################################

## Survey(s)
igfs <-
  g3_fleet('igfs') %>%
  g3s_livesonareas(areas[c('1')])

## Commercial
lln <-
  g3_fleet('lln') %>%
  g3s_livesonareas(areas[c('1')])

bmt <-
  g3_fleet('bmt') %>%
  g3s_livesonareas(areas[c('1')])

gil <-
  g3_fleet('gil') %>%
  g3s_livesonareas(areas[c('1')])

foreign <-
  g3_fleet('foreign') %>%
  g3s_livesonareas(areas[c('1')])



## create fleet actions
fleet_actions <-
  list(
    lln %>%
      g3a_predate_fleet(stocks,
                        suitabilities = 
                          stocks %>% 
                          set_names(.,map(.,'name')) %>% 
                          map(function(x) g3_suitability_exponentiall50(g3_parameterized('lln.alpha', by_stock = 'species'),
                                                                        g3_parameterized('lln.l50', by_stock = 'species'))),
                        catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata('lln_landings', lln_landings[[1]] %>%
                                                                                               mutate(area = as.numeric(area),
                                                                                                      step = as.numeric(step),
                                                                                                      year = as.numeric(year))))),
    
    bmt %>%
      g3a_predate_fleet(stocks,
                        suitabilities = 
                          stocks %>% 
                            set_names(.,map(.,'name')) %>% 
                            map(function(x) g3_suitability_exponentiall50(g3_parameterized('bmt.alpha', by_stock = 'species'),
                                                                          g3_parameterized('bmt.l50', by_stock = 'species'))),
                          catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata('bmt_landings', bmt_landings[[1]] %>%
                                                                                                 mutate(area = as.numeric(area),
                                                                                                        step = as.numeric(step),
                                                                                                        year = as.numeric(year))))),
    gil %>%
      g3a_predate_fleet(stocks,
                        suitabilities = 
                          stocks %>% 
                            set_names(.,map(.,'name')) %>% 
                            map(function(x) g3_suitability_exponentiall50(g3_parameterized('gil.alpha', by_stock = 'species'),
                                                                          g3_parameterized('gil.l50', by_stock = 'species'))),
                          catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata('gil_landings', gil_landings[[1]] %>%
                                                                                                 mutate(area = as.numeric(area),
                                                                                                        step = as.numeric(step),
                                                                                                        year = as.numeric(year))))),
    foreign  %>%
      g3a_predate_fleet(stocks,
                        suitabilities = 
                          stocks %>% 
                            set_names(.,map(.,'name')) %>% 
                            map(function(x) g3_suitability_exponentiall50(g3_parameterized('lln.alpha', by_stock = 'species'),
                                                                          g3_parameterized('lln.l50', by_stock = 'species'))),
                          catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata('foreign_landings', foreign_landings[[1]] %>%
                                                                                                 mutate(area = as.numeric(area),
                                                                                                        step = as.numeric(step),
                                                                                                        year = as.numeric(year))))),
    
    igfs %>%
      g3a_predate_fleet(stocks,
                        suitabilities = 
                          stocks %>% 
                            set_names(.,map(.,'name')) %>% 
                            map(function(x) g3_suitability_exponentiall50(g3_parameterized('igfs.alpha', by_stock = 'species'),
                                                                          g3_parameterized('igfs.l50', by_stock = 'species'))),
                          catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata('igfs_landings', igfs_landings %>%
                                                                                                 mutate(area = as.numeric(area),
                                                                                                        step = as.numeric(step),
                                                                                                        year = as.numeric(year))))))
