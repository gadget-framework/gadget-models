## Collect catches by fleet:
comm.landings <- mfdb_sample_totalweight(mdb, NULL, c(list(
  #gear=c('HLN','LLN'),
  sampling_type = 'LND'),defaults)) 

foreign.landings <-
  mfdb_sample_totalweight(mdb, NULL,
                          c(list(
                            sampling_type = 'FLND',
                            data_source = c('lods.foreign.landings','statlant.foreign.landings'),
                            species = defaults$species),
                            defaults))

igfs.landings <- 
  structure(data.frame(year=defaults$year,step=2,area=1,number=1),
            area_group = mfdb_group(`1` = 1))


## write to file
gadgetfleet('Modelfiles/fleet',gd$dir,missingOkay = TRUE) %>% 
  gadget_update('totalfleet',
                name = 'igfs',
                suitability = paste0('\n',
                                     paste(stock_names,
                                           'function','exponentiall50',
                                           '#tusk.igfs.alpha','#tusk.igfs.l50',
                                           collapse='\n')),
                data = igfs.landings) %>%
  gadget_update('totalfleet',
                name = 'comm',
                suitability = paste0('\n',
                                     paste(stock_names,
                                           'function','exponentiall50',
                                           '#tusk.comm.alpha','#tusk.comm.l50',
                                           collapse='\n')),
                data = comm.landings[[1]]) %>% 
  gadget_update('totalfleet',
                name = 'foreign',
                suitability = paste0('\n',
                                     paste(stock_names,
                                           'function','exponentiall50',
                                           '#tusk.comm.alpha','#tusk.comm.l50',
                                           collapse='\n')),
                data = foreign.landings[[1]]) %>% 
  write.gadget.file(gd$dir)
