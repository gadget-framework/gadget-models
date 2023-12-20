## -----------------------------------------------------------------------------
##
## Configure fleets and associated actions
##
## -----------------------------------------------------------------------------

## FLEET STOCKS
## Longline
lln <- 
  gadget3::g3_fleet('lln') %>% 
  gadget3::g3s_livesonareas(areas[c('1')])

## Bottom trawl
bmt <- 
  gadget3::g3_fleet('bmt') %>% 
  gadget3::g3s_livesonareas(areas[c('1')])

## Foreign
foreign <- 
  gadget3::g3_fleet('foreign') %>% 
  gadget3::g3s_livesonareas(areas[c('1')])

## Autumn survey
aut <- 
  gadget3::g3_fleet("aut") %>%
  gadget3::g3s_livesonareas(areas[c("1")])

## For single fleet option
comm <- 
  gadget3::g3_fleet("comm") %>% 
  gadget3::g3s_livesonareas(areas[c("1")])

## ------------------------------------------------------------------------------

## Note: in this example the suitabilities are defined for each fleet across stocks, 
## and we utilise the defaults for the most part, see ?g3_suitability_andersenfleet
## and ?g3_suitability_exponentiall50 for details. However, as we want the suitabilities 
## at the stock not the sub-stock level, we add by_stock = stocks to each suitability function
## so the parameters are defined "bling.l50" rather than "bling_mat.l50"... furthermore,
## for the andersenfleets the parameter "p5" is defined as the max length across stocks


## Fleet actions
fleet_actions <- 
  list(
    if (single_fleet){
      comm %>% 
        gadget3::g3a_predate_fleet(stocks,
                                   suitabilities = 
                                     stocks %>% 
                                     set_names(.,map(.,'name')) %>% 
                                     map(function(x){
                                       if (dome_comm){
                                         ## Dome shaped selection, see ?g3_suitability_andersenfleet
                                         ## This will generate 5 parameters, bling.comm.andersen.[p0-p4]
                                         gadget3::g3_suitability_andersenfleet(
                                           p5 = max(sapply(stocks, g3_stock_def, 'maxmidlen')),
                                           by_stock = stocks)
                                         }else{
                                           ## S-shaped selection, see ?g3_suitability_exponentiall50
                                           ## This will generate two parameters: bling.comm.alpha and bling.comm.l50
                                           gadget3::g3_suitability_exponentiall50(by_stock = stocks) 
                                           }
                                       }),
                                   catchability_f = gadget3::g3a_predate_catchability_totalfleet(
                                     gadget3::g3_timeareadata('comm_landings', comm_landings[[1]] %>%
                                                                mutate(area = as.numeric(area),
                                                                       step = as.numeric(step),
                                                                       year = as.numeric(year)))
                                     )
                                   ) 
      }else{
      c(
        lln %>% 
          gadget3::g3a_predate_fleet(stocks,
                                     suitabilities = 
                                       stocks %>% 
                                       set_names(.,map(.,'name')) %>% 
                                       map(function(x){
                                         if (dome_lln){
                                           gadget3::g3_suitability_andersenfleet(
                                             p5 = max(sapply(stocks, g3_stock_def, 'maxmidlen')),
                                             by_stock = stocks
                                           )
                                           }else{
                                             gadget3::g3_suitability_exponentiall50(by_stock = stocks) 
                                             }
                                         }),
                                     catchability_f = gadget3::g3a_predate_catchability_totalfleet(
                                       gadget3::g3_timeareadata('lln_landings', lln_landings[[1]] %>%
                                                                  mutate(area = as.numeric(area),
                                                                         step = as.numeric(step),
                                                                         year = as.numeric(year)))
                                       )
                                     ),
        bmt %>% 
          gadget3::g3a_predate_fleet(stocks,
                                     suitabilities = 
                                       stocks %>% 
                                       set_names(.,map(.,'name')) %>% 
                                       map(function(x){
                                         if (dome_bmt){
                                           gadget3::g3_suitability_andersenfleet(
                                             p5 = max(sapply(stocks, g3_stock_def, 'maxmidlen')),
                                             by_stock = stocks
                                             )
                                           }else{
                                             gadget3::g3_suitability_exponentiall50(by_stock = stocks) 
                                             }
                                         }),
                                     catchability_f = gadget3::g3a_predate_catchability_totalfleet(
                                       gadget3::g3_timeareadata('bmt_landings', bmt_landings[[1]] %>%
                                                                  mutate(area = as.numeric(area),
                                                                         step = as.numeric(step),
                                                                         year = as.numeric(year)))
                                       )
                                     )
        )},
    foreign  %>% 
      gadget3::g3a_predate_fleet(stocks,
                                 suitabilities = 
                                   stocks %>% 
                                   set_names(.,map(.,'name')) %>% 
                                   map(function(x){
                                     if (dome_lln || dome_comm){
                                       gadget3::g3_suitability_andersenfleet(
                                         p1 = gadget3::g3_parameterized(ifelse(single_fleet, 'comm.andersen.p1', 'lln.andersen.p1'), by_stock = stocks),
                                         p3 = gadget3::g3_parameterized(ifelse(single_fleet, 'comm.andersen.p3', 'lln.andersen.p3'), by_stock = stocks, exponentiate = TRUE),
                                         p4 = gadget3::g3_parameterized(ifelse(single_fleet, 'comm.andersen.p4', 'lln.andersen.p4'), by_stock = stocks, exponentiate = TRUE),
                                         p5 = max(sapply(stocks, g3_stock_def, 'maxmidlen')),
                                         by_stock = stocks
                                         )
                                       }else{
                                         ## The foreign fleet will share the same longline suitability or, if single_fleet = TRUE, the commercial fleet 
                                         gadget3::g3_suitability_exponentiall50(gadget3::g3_parameterized(ifelse(single_fleet, 'comm.alpha', 'lln.alpha')),
                                                                                gadget3::g3_parameterized(ifelse(single_fleet, 'comm.l50', 'lln.l50'))) 
                                         }
                                     }),
                                 catchability_f = gadget3::g3a_predate_catchability_totalfleet(
                                   gadget3::g3_timeareadata('foreign_landings', foreign_landings[[1]] %>%
                                                              mutate(area = as.numeric(area),
                                                                     step = as.numeric(step),
                                                                     year = as.numeric(year)))
                                   )
                                 ),
    aut %>% 
      gadget3::g3a_predate_fleet(stocks,
                                 suitabilities = 
                                   stocks %>% 
                                   set_names(.,map(.,'name')) %>% 
                                   map(function(x){
                                     if (dome_aut){
                                       gadget3::g3_suitability_andersenfleet(
                                         p5 = max(sapply(stocks, g3_stock_def, 'maxmidlen')),
                                         by_stock = stocks
                                         )
                                       }else{
                                         gadget3::g3_suitability_exponentiall50(by_stock = stocks)
                                         }
                                     }), 
                                 catchability_f = gadget3::g3a_predate_catchability_totalfleet(
                                   gadget3::g3_timeareadata('aut_landings', aut_landings %>%
                                                              mutate(area = as.numeric(area),
                                                                     step = as.numeric(step),
                                                                     year = as.numeric(year)))
                                   )
                                 ),
    list()
  )
