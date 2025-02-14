# ---------------------------------------------------------------------
# Retrieve catches VEN for the period 1991-2018
com.catch.ven <- read.table("~/../valerio/Share/Gadget/vendace/SS3_input/data/commercial_catch.dat", sep="\t", header=T)
tmp <- expand.grid(year=1991:2018,step=1:4,area=1,fleet="comven")
com.catch.ven <- left_join(tmp,com.catch.ven,by="year") %>%
                 arrange(year,step) %>%
                 mutate(biomass=biomass * 1000, # convert in kg
                        biomass=biomass * # all catches assigned to quarter4
                                rep(c(0,0,0,1), length(1991:2018))) %>%
                 mutate(fleet=ifelse(year<2008,"comven1","comven2")) # introduction of grid in ~2008 
## com.catch.ven1 <- filter(com.catch.ven,year<2008)
## com.catch.ven2 <- filter(com.catch.ven,year>=2008)

## com.catch.ven <- mfdb_sample_totalweight(mdb, c('vessel'), list(
##     area          = mfdb_group('1'="SD3031"),
##     timestep      = mfdb_timestep_quarterly,
##     year          = 1991:2018,
##     data_source   = 'landings_vendace',
##     sampling_type = 'LND'))[[1]] %>%
##                   mutate(total_weight = total_weight*1000)
## com.catch.ven$vessel <- as.factor('comven')
## tmp <- com.catch.ven %>% group_by(year,fleet) %>% summarise(wgt=sum(biomass))
## ggplot(tmp, aes(year,wgt, fill=fleet)) + geom_bar(stat="identity")

# ---------------------------------------------------------------------
# Retrieve biomass removed by SEAL for the period 1991-2018
seal.cons.ven <- read.table("~/../valerio/Share/Gadget/vendace/SS3_input/data/seal_consumption_vendace_high.dat", sep="\t", header=T)
tmp <- expand.grid(year=1991:2018,step=1:4,area=1,fleet="sealven")
seal.cons.ven <- left_join(tmp,seal.cons.ven,by="year") %>%
                 arrange(year,step) %>%
                 mutate(biomass=biomass * 1000, # convert in kg
                        biomass=biomass * # partition consumption by seal 25% on each quarter
                                rep(c(0.25,0.25,0.25,0.25), length(1991:2018)))

## seal.cons.ven <- mfdb_sample_totalweight(mdb, c('vessel'), list(
##     area          = mfdb_group('1'="SD3031"),
##     timestep      = mfdb_timestep_quarterly,
##     year          = 1991:2018,
##     data_source   = 'seal_biomass_vendace',
##     sampling_type = 'LND'))[[1]] %>%
##                   mutate(total_weight = total_weight*1000)
## seal.cond.ven$vessel <- as.factor('sealven')

## tmp <- seal.cons.ven %>% group_by(year) %>% summarise(wgt=sum(biomass))
## ggplot(tmp, aes(year,wgt)) + geom_bar(stat="identity")

# ---------------------------------------------------------------------
# make survey
aco.catch <- 
  structure(rbind(data.frame(year=2009:2012,step=3,area=1,fleet="aco",number=1), # month 9.3
                  data.frame(year=2013:2014,step=4,area=1,fleet="aco",number=1), # month 11.0
                  data.frame(year=2015:2018,step=4,area=1,fleet="aco",number=1)), # month 10.4
            area_group = mfdb_group(`1` = 1))

# ---------------------------------------------------------------------
# make reference fleet
## ref.catch <- 
##   structure(rbind(data.frame(year=2007:2018,step=4,area=1,fleet="reffleet",number=1)),
##             area_group = mfdb_group(`1` = 1))

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
  ## gadget_update('totalfleet',
  ##               name = 'reffleet',
  ##               suitability =
  ##               paste0('\n',
  ##                        paste(c('venimm','venmat'),
  ##                              'function','exponentiall50',
  ##                              ## '#ven.ref.alpha','#ven.ref.l50',
  ##                              '#ven.com2.alpha','#ven.com2.l50', # same selection of com2
  ##                              collapse='\n')),
  ##               data = ref.catch) %>%
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
                data = com.catch.ven) %>%
  gadget_update('totalfleet',
                name = 'sealven',
                ## livesonareas = 1,
                suitability = 
                  paste0('\n',
                         paste(c('venimm','venmat'),
                               'function','exponentiall50',
                               '#ven.sea.alpha','#ven.sea.l50',
                               collapse='\n')),
                data = seal.cons.ven)

# Fix strange Rgadget behaviour (bug?)
tmp[2]$component$livesonareas <- 1
tmp[2]$component$amount$data$area <- 1
tmp[3]$component$livesonareas <- 1
tmp[3]$component$amount$data$area <- 1
tmp[4]$component$livesonareas <- 1
tmp[4]$component$amount$data$area <- 1
## tmp[5]$component$livesonareas <- 1
## tmp[5]$component$amount$data$area <- 1
write.gadget.file(tmp, gd$dir)
