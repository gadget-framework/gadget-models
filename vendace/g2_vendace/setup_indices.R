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
    area          = "SD3031",
    timestep      = mfdb_timestep_quarterly,
    year          = 1996:2018,
    sampling_type = 'LND',
    data_source   = 'cpue_allfishery_vendace'))[[1]]
si.ven.cpue$area <- "area1"
names(attributes(si.ven.cpue)$area) <- "area1"
attributes(si.ven.cpue)$area$area1 <- "area1"
attributes(si.ven.cpue)$length$all <- c(minlength,maxlength)

si.ven.aco <- mfdb_sample_count(mdb, c('age'), list(
    area          = "SD3031",
    timestep      = mfdb_timestep_quarterly,
    year          = 2009:2018,
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
        

# ---------------------------------------------------------------------
# Query reference fleet catch and make index abundance ageXX
# age0:
si.ven.reff0 <- mfdb_sample_count(mdb, c('age'), list(
    area          = "SD3031",
    timestep      = mfdb_timestep_quarterly,
    year          = 2008:2018,
    ## year          = 2007:2018,
    sampling_type = 'LND',
    data_source   = 'refFleet_agedistribution_vendace',
    age           = mfdb_group('age0'=0)))[[1]]
si.ven.reff0$area <- "area1"
names(attributes(si.ven.reff0)$area) <- "area1"
attributes(si.ven.reff0)$area$area1 <- "area1"
# age1:
si.ven.reff1 <- mfdb_sample_count(mdb, c('age'), list(
    area          = "SD3031",
    timestep      = mfdb_timestep_quarterly,
    year          = 2008:2018,
    ## year          = 2007:2018,
    sampling_type = 'LND',
    data_source   = 'refFleet_agedistribution_vendace',
    age           = mfdb_group('age1'=1)))[[1]]
si.ven.reff1$area <- "area1"
names(attributes(si.ven.reff1)$area) <- "area1"
attributes(si.ven.reff1)$area$area1 <- "area1"
# age2:
si.ven.reff2 <- mfdb_sample_count(mdb, c('age'), list(
    area          = "SD3031",
    timestep      = mfdb_timestep_quarterly,
    year          = 2008:2018,
    ## year          = 2007:2018,
    sampling_type = 'LND',
    data_source   = 'refFleet_agedistribution_vendace',
    age           = mfdb_group('age2'=2)))[[1]]
si.ven.reff2$area <- "area1"
names(attributes(si.ven.reff2)$area) <- "area1"
attributes(si.ven.reff2)$area$area1 <- "area1"
# age3:
si.ven.reff3 <- mfdb_sample_count(mdb, c('age'), list(
    area          = "SD3031",
    timestep      = mfdb_timestep_quarterly,
    year          = 2008:2018,
    ## year          = 2007:2018,
    sampling_type = 'LND',
    data_source   = 'refFleet_agedistribution_vendace',
    age           = mfdb_group('age3'=3)))[[1]]
si.ven.reff3$area <- "area1"
names(attributes(si.ven.reff3)$area) <- "area1"
attributes(si.ven.reff3)$area$area1 <- "area1"
# age4:
si.ven.reff4 <- mfdb_sample_count(mdb, c('age'), list(
    area          = "SD3031",
    timestep      = mfdb_timestep_quarterly,
    year          = 2008:2018,
    ## year          = 2007:2018,
    sampling_type = 'LND',
    data_source   = 'refFleet_agedistribution_vendace',
    age           = mfdb_group('age4'=4)))[[1]]
si.ven.reff4$area <- "area1"
names(attributes(si.ven.reff4)$area) <- "area1"
attributes(si.ven.reff4)$area$area1 <- "area1"
# age5:
si.ven.reff5 <- mfdb_sample_count(mdb, c('age'), list(
    area          = "SD3031",
    timestep      = mfdb_timestep_quarterly,
    year          = 2008:2018,
    ## year          = 2007:2018,
    sampling_type = 'LND',
    data_source   = 'refFleet_agedistribution_vendace',
    age           = mfdb_group('age5'=5:10)))[[1]]
si.ven.reff5$area <- "area1"
names(attributes(si.ven.reff5)$area) <- "area1"
attributes(si.ven.reff5)$area$area1 <- "area1"


## # ---------------------------------------------------------------------
## # Query Acoustic and make index abundance ageXX
## # age0:
## si.ven.aco0 <- mfdb_sample_count(mdb, c('age'), list(
##     area          = "SD3031",
##     timestep      = mfdb_timestep_quarterly,
##     year          = 2009:2018,
##     sampling_type = 'RES',
##     data_source   = 'acoustic_agedistribution_vendace',
##     age           = mfdb_group('age0'=0)))[[1]]
## si.ven.aco0$area <- "area1"
## # age1:
## si.ven.aco1 <- mfdb_sample_count(mdb, c('age'), list(
##     area          = "SD3031",
##     timestep      = mfdb_timestep_quarterly,
##     year          = 2009:2018,
##     sampling_type = 'RES',
##     data_source   = 'acoustic_agedistribution_vendace',
##     age           = mfdb_group('age1'=1)))[[1]]
## si.ven.aco1$area <- "area1"
## # age2:
## si.ven.aco2 <- mfdb_sample_count(mdb, c('age'), list(
##     area          = "SD3031",
##     timestep      = mfdb_timestep_quarterly,
##     year          = 2009:2018,
##     sampling_type = 'RES',
##     data_source   = 'acoustic_agedistribution_vendace',
##     age           = mfdb_group('age2'=2)))[[1]]
## si.ven.aco2$area <- "area1"
## # age3:
## si.ven.aco3 <- mfdb_sample_count(mdb, c('age'), list(
##     area          = "SD3031",
##     timestep      = mfdb_timestep_quarterly,
##     year          = 2009:2018,
##     sampling_type = 'RES',
##     data_source   = 'acoustic_agedistribution_vendace',
##     age           = mfdb_group('age3'=3)))[[1]]
## si.ven.aco3$area <- "area1"
## # age4:
## si.ven.aco4 <- mfdb_sample_count(mdb, c('age'), list(
##     area          = "SD3031",
##     timestep      = mfdb_timestep_quarterly,
##     year          = 2009:2018,
##     sampling_type = 'RES',
##     data_source   = 'acoustic_agedistribution_vendace',
##     age           = mfdb_group('age4'=4)))[[1]]
## si.ven.aco4$area <- "area1"
## # age5:
## si.ven.aco5 <- mfdb_sample_count(mdb, c('age'), list(
##     area          = "SD3031",
##     timestep      = mfdb_timestep_quarterly,
##     year          = 2009:2018,
##     sampling_type = 'RES',
##     data_source   = 'acoustic_agedistribution_vendace',
##     age           = mfdb_group('age5'=5)))[[1]]
## si.ven.aco5$area <- "area1"
## # age6:
## si.ven.aco6 <- mfdb_sample_count(mdb, c('age'), list(
##     area          = "SD3031",
##     timestep      = mfdb_timestep_quarterly,
##     year          = 2009:2018,
##     sampling_type = 'RES',
##     data_source   = 'acoustic_agedistribution_vendace',
##     age           = mfdb_group('age6'=6)))[[1]]
## si.ven.aco6$area <- "area1"
## # age7:
## si.ven.aco7 <- mfdb_sample_count(mdb, c('age'), list(
##     area          = "SD3031",
##     timestep      = mfdb_timestep_quarterly,
##     year          = 2009:2018,
##     sampling_type = 'RES',
##     data_source   = 'acoustic_agedistribution_vendace',
##     age           = mfdb_group('age7'=7)))[[1]]
## si.ven.aco7$area <- "area1"
## # age8:
## si.ven.aco8 <- mfdb_sample_count(mdb, c('age'), list(
##     area          = "SD3031",
##     timestep      = mfdb_timestep_quarterly,
##     year          = 2009:2018,
##     sampling_type = 'RES',
##     data_source   = 'acoustic_agedistribution_vendace',
##     age           = mfdb_group('age8'=8:10)))[[1]]
## si.ven.aco8$area <- "area1"
