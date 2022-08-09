# ---------------------------------------------------------------------
# Retrieve catches VEN for the period 1991-2020
com.catch.ven <- mfdb_sample_totalweight(mdb, c('gear'), list(
    area          = "SD31",
    timestep      = mfdb_timestep_quarterly,
    year          = 1991:2020,
    ## gear          = mfdb_unaggregated(),
    data_source   = 'comm_catch_bench2021',
    sampling_type = 'LND'))[[1]] %>%
    mutate(total_weight = total_weight*1000) %>%
    rename(fleet = gear) %>%
    mutate(fleet = ifelse(year<2008,"comven1","comven2")) # introduction of grid in ~2008
    
## tmp <- com.catch.ven %>% group_by(year,fleet) %>% summarise(wgt=sum(total_weight))
## ggplot(tmp, aes(year,wgt, fill=fleet)) + geom_bar(stat="identity")

# ---------------------------------------------------------------------
# make survey
aco.catch <- 
  structure(rbind(data.frame(year=2009:2012,step=3,area=1,fleet="aco",number=1), # month 9.3
                  data.frame(year=2013:2014,step=4,area=1,fleet="aco",number=1), # month 11.0
                  data.frame(year=2015:2020,step=4,area=1,fleet="aco",number=1)), # month 10.4
            area_group = mfdb_group(`1` = 1))

# ---------------------------------------------------------------------
## write to file
tmp <- gadgetfleet('Modelfiles/fleet',gd$dir,missingOkay = TRUE) %>% 
  gadget_update('totalfleet',
                name = 'aco',
                suitability =
                paste0('\n',
                         paste(c('venimm','venmat'),
                               'function','exponentiall50',
                               '#ven.aco.alpha','#ven.aco.l50',
                               collapse='\n')),
                data = aco.catch) %>%
# consider change in selectivity (~2008 intro of grid)
  gadget_update('totalfleet',
                name = 'comven1',
                ## livesonareas = 1,
                suitability = 
                  paste0('\n',
                         paste(c('venimm','venmat'),
                               'function','exponentiall50',
                               '#ven.com1.alpha','#ven.com1.l50',
                               ## '#ven.com2.alpha','#ven.com2.l50',
                               collapse='\n')),
                data = com.catch.ven) %>%
  gadget_update('totalfleet',
                name = 'comven2',
                ## livesonareas = 1,
                suitability = 
                  paste0('\n',
                         paste(c('venimm','venmat'),
                               'function','exponentiall50',
                               '#ven.com2.alpha','#ven.com2.l50',
                               collapse='\n')),
                data = com.catch.ven)

# Fix strange Rgadget behaviour (bug?)
tmp[2]$component$livesonareas <- 1
tmp[2]$component$amount$data$area <- 1
tmp[3]$component$livesonareas <- 1
tmp[3]$component$amount$data$area <- 1
## tmp[4]$component$livesonareas <- 1
## tmp[4]$component$amount$data$area <- 1
## tmp[5]$component$livesonareas <- 1
## tmp[5]$component$amount$data$area <- 1
write.gadget.file(tmp, gd$dir)
