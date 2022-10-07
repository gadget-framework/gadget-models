

## Survey(s)
igfs_tag <-
  g3_fleet('igfs_tag') %>%
  g3s_livesonareas(areas[c('1')])


tags <- 1:10
names(tags) <- 1:10

tagging_actions <- list(
  g3a_predate_tagrelease(
    # Setup as-per g3a_predate_fleet
    igfs_tag,
    list(imm_stock, mat_stock),
    
  #  suitabilities = list(g3_suitability_exponentiall50(g3_parameterized('lln.alpha', by_stock = 'species'),
   #                               g3_parameterized('lln.l50', by_stock = 'species'))),
  catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata('lln_landings', lln_landings[[1]] %>%
                                                                         mutate(area = as.numeric(area),
                                                                                step = as.numeric(step),
                                                                                year = as.numeric(year)))),
    
    suitabilities = g3_suitability_straightline(1,0),
    #catchability_f = g3a_predate_catchability_numberfleet(~100),
    
    # Optional tag mortality suitability
    # Formula to decide which tag to output into, generate table
    # with one tag per year
    output_tag_f = g3_timeareadata('fleet_igfs_tags', data.frame(
      year = c(1982:1991),
      tag = tags,
      stringsAsFactors = FALSE), value_field = "tag"),
    
    # Experiment only happens in spring
    run_f = ~cur_step == 2),
  
  g3a_tag_shedding(
    list(imm_stock, mat_stock),
    # i.e. 0.125 will loose their tag each step
    tagshed_f = log(8)))



