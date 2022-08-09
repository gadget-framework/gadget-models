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

# ## Bounded parameters for fleet suitabilities
# fleet_bounds <- list(
#   
#   'lln.l50' = list(lower = 20, upper = 100),
#   'lln.alpha' = list(lower = 0.01, upper = 1),
#   
#   'bmt.l50' = list(lower = 20, upper = 100),
#   'bmt.alpha' = list(lower = 0.01, upper = 1),
#   
#   'gil.l50' = list(lower = 20, upper = 100),
#   'gil.alpha' = list(lower = 0.01, upper = 1),
#   
#   'igfs.l50' = list(lower = 20, upper = 100),
#   'igfs.alpha' = list(lower = 0.01, upper = 1)
#   
# )

## create fleet actions
fleet_actions <-
  list(
    lln %>%
      g3a_predate_fleet(stocks,
                        suitabilities = 
                          stocks %>% 
                          set_names(.,map(.,'name')) %>% 
                          map(function(x) g3_suitability_exponentiall50(g3_stock_param(x, id = 'species', 'lln.alpha', setup_options$bound_params),
                                                                        g3_stock_param(x, id = 'species', 'lln.l50', setup_options$bound_params))),
                        catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata('lln_landings', lln_landings[[1]] %>%
                                                                                               mutate(area = as.numeric(area),
                                                                                                      step = as.numeric(step),
                                                                                                      year = as.numeric(year))))),
    
    bmt %>%
      g3a_predate_fleet(stocks,
                        suitabilities = 
                          stocks %>% 
                            set_names(.,map(.,'name')) %>% 
                            map(function(x) g3_suitability_exponentiall50(g3_stock_param(x, id = 'species', 'bmt.alpha', setup_options$bound_params),
                                                                          g3_stock_param(x, id = 'species', 'bmt.l50', setup_options$bound_params))),
                          catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata('bmt_landings', bmt_landings[[1]] %>%
                                                                                                 mutate(area = as.numeric(area),
                                                                                                        step = as.numeric(step),
                                                                                                        year = as.numeric(year))))),
    gil %>%
      g3a_predate_fleet(stocks,
                        suitabilities = 
                          stocks %>% 
                            set_names(.,map(.,'name')) %>% 
                            map(function(x) g3_suitability_exponentiall50(g3_stock_param(x, id = 'species', 'gil.alpha', setup_options$bound_params),
                                                                          g3_stock_param(x, id = 'species', 'gil.l50', setup_options$bound_params))),
                          catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata('gil_landings', gil_landings[[1]] %>%
                                                                                                 mutate(area = as.numeric(area),
                                                                                                        step = as.numeric(step),
                                                                                                        year = as.numeric(year))))),
    foreign  %>%
      g3a_predate_fleet(stocks,
                        suitabilities = 
                          stocks %>% 
                            set_names(.,map(.,'name')) %>% 
                            map(function(x) g3_suitability_exponentiall50(g3_stock_param(x, id = 'species', 'lln.alpha', setup_options$bound_params),
                                                                          g3_stock_param(x, id = 'species', 'lln.l50', setup_options$bound_params))),
                          catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata('foreign_landings', foreign_landings[[1]] %>%
                                                                                                 mutate(area = as.numeric(area),
                                                                                                        step = as.numeric(step),
                                                                                                        year = as.numeric(year))))),
    
    igfs %>%
      g3a_predate_fleet(stocks,
                        suitabilities = 
                          stocks %>% 
                            set_names(.,map(.,'name')) %>% 
                            map(function(x) g3_suitability_exponentiall50(g3_stock_param(x, id = 'species', 'igfs.alpha', setup_options$bound_params),
                                                                          g3_stock_param(x, id = 'species', 'igfs.l50', setup_options$bound_params))),
                          catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata('igfs_landings', igfs_landings %>%
                                                                                                 mutate(area = as.numeric(area),
                                                                                                        step = as.numeric(step),
                                                                                                        year = as.numeric(year))))))
