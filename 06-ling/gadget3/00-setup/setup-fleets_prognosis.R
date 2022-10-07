## -----------------------------------------------------------------------------
##
## Fleet actions:
##
## -----------------------------------------------------------------------------


# Use g3a_hcr_assess() to simulate assessment procedure:
#
#    g3a_hcr_assess(
#        list(ling_imm, ling_mat),
#        fishable_fs = g3a_hcr_biomass_weighted(-100,~g3_param('ref.cm'),1),
#        trigger_fs = list(
#            ling_imm = g3a_hcr_biomass_weighted(0,0,0),
#            ling_mat = g3a_hcr_biomass_weighted(0,0,1)),
#        tac_f = g3a_hcr_tac_singletrigger(),
#        assess_run_f = ~cur_step == 2),
#
# NB: Separate gather_run_f & assess_run_f, the former is when the
# g3a_hcr_biomass_weighted() data is gathered, the latter is when
# g3a_hcr_tac_singletrigger() is run.
#
# Create proportions of TAC assigned to each fleet / stock:
#
#    hcr_stock_proportions <- list(0.5)
#    hcr_fleet_proportions <- quote( stock_param(fleet_stock, 'hcr.prop', value = 1/2) )
#
# Wrap default fleet predation with g3a_predate_catchability_hcr(), to scale predation
# based on TAC:
#
#    g3a_predate_fleet(list(ling_imm, ling_mat),
#        catchability_f = g3a_predate_catchability_hcr(
#        g3a_predate_catchability_totalfleet(g3_timeareadata('lln_landings', lln_landings_data)),
#            hcr_stock_proportions,
#       

############ Configure fleets ##################################################

## Survey(s)
igfs <-
  g3_fleet('igfs') %>%
  g3s_livesonareas(areas[c('1')])

## Commercial
lln <-
  g3_fleet('lln') %>%
  g3s_livesonareas(areas[c('1')])


lln_proj <-
  g3_fleet('lln_proj') %>%
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

test <- 
  structure(data.frame(year=2023:2034,step=1:4,area=rep(1,4),total_weight=rep(1,4)),
            area_group = mfdb_group(`1` = 1))


## create fleet actions
fleet_actions_prog <-
  list(
        g3a_hcr_assess(
            list(imm_stock, mat_stock),
            fishable_fs = g3a_hcr_biomass_weighted(-100,~g3_param('ref.cm'),1),
            trigger_fs = list(
                'ling_imm' = g3a_hcr_biomass_weighted(0,0,0),
                'ling_mat' = g3a_hcr_biomass_weighted(0,0,1)),
            tac_f = g3a_hcr_tac_singletrigger(),
            assess_run_f = ~cur_step == 2),
    
        lln_proj %>% 
          g3a_predate_fleet(stocks,
                            suitabilities = 
                              stocks %>% 
                              set_names(.,map(.,'name')) %>% 
                              map(function(x) g3_suitability_exponentiall50(g3_parameterized('lln.alpha', by_stock = 'species'),
                                                                            g3_parameterized('lln.l50', by_stock = 'species'))),
                            catchability_f = g3a_predate_catchability_hcr(g3a_predate_catchability_linearfleet(
                              1
                            ),
                                                                  stock_prop_fs = 1,
                                                                  fleet_prop_fs = 1),
          run_f = ~cur_year_projection),
        
    lln %>%
      g3a_predate_fleet(stocks,
                        suitabilities = 
                          stocks %>% 
                          set_names(.,map(.,'name')) %>% 
                          map(function(x) g3_suitability_exponentiall50(g3_parameterized('lln.alpha', by_stock = 'species'),
                                                                        g3_parameterized('lln.l50', by_stock = 'species'))),
                        catchability_f = 
                          g3a_predate_catchability_totalfleet(g3_timeareadata('lln_landings', lln_landings[[1]] %>%
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
