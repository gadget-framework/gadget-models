# Query reference fleet, all fishery and acoustic and make index abundance
## si.ven.reff <- mfdb_sample_count(mdb, c('age'), list(
##     area          = "SD3031",
##     timestep      = mfdb_timestep_quarterly,
##     year          = 2007:2018,
##     sampling_type = 'LND',
##     data_source   = 'refFleet_agedistribution_vendace',
##     age           = mfdb_group('all'=0:10)))[[1]]
## si.ven.reff$area <- "area1"
## names(attributes(si.ven.reff)$area) <- "area1"
## attributes(si.ven.reff)$area$area1 <- "area1"

si.ven.cpue <- mfdb_sample_count(mdb, c('length'), list(
    area          = "SD31",
    timestep      = mfdb_timestep_quarterly,
    year          = 1996:2020,
    sampling_type = 'LND',
    data_source   = 'cpue_comm_all_bench2021'))[[1]]
si.ven.cpue$area <- "area1"
names(attributes(si.ven.cpue)$area) <- "area1"
attributes(si.ven.cpue)$area$area1 <- "area1"
attributes(si.ven.cpue)$length$all <- c(minlength,maxlength)

si.ven.aco <- mfdb_sample_count(mdb, c('age'), list(
    area          = "SD3031",
    timestep      = mfdb_timestep_quarterly,
    year          = 2009:2020,
    sampling_type = 'RES',
    data_source   = 'acoustic_agedistribution_vendace',
    age           = mfdb_group('all'=0:10)))[[1]]
si.ven.aco$area <- "area1"
names(attributes(si.ven.aco)$area) <- "area1"
attributes(si.ven.aco)$area$area1 <- "area1"

## ggplot() +
##     geom_point(data=si.ven.cpue, aes(year,number/max(number))) + geom_line(data=si.ven.cpue, aes(year,number/max(number))) +
##     geom_point(data=si.ven.aco, aes(year,number/max(number)),col=2) + geom_line(data=si.ven.aco, aes(year,number/max(number)),col=2) +
##     geom_point(data=si.ven.reff, aes(year,number/max(number)),col=3) + geom_line(data=si.ven.reff, aes(year,number/max(number)),col=3)
        
