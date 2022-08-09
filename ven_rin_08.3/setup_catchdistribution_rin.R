# biol data are now in mfdb so can be:
# extracted as raw, aggregated, re-uploaded as tmp, and extracted again with all the attributes for a LH component
require(tidyverse)

# retrieve raw individual data from mfdb
bio <- mfdb_sample_rawdata(mdb, c('age','sex','maturity_stage'), list(
            year = mfdb_unaggregated(),
            area = mfdb_unaggregated(),
            timestep = mfdb_unaggregated(),
            maturity_stage = mfdb_unaggregated(),
            sex = mfdb_unaggregated(),
            age = mfdb_unaggregated(),
            data_source = "ringed_seal_individuals"))[[1]]

# aggregated by decade
bio <- bio %>%
    mutate(year10 = ifelse(year %in% 1970:1979, 1974,
                    ifelse(year %in% 1980:1989, 1984,
                    ifelse(year %in% 1990:1999, 1994,
                    ifelse(year %in% 2000:2009, 2004,
                    ifelse(year %in% 2010:2019, 2014, year))))))

mfdb_import_survey(mdb,
    data_source = 'tmp',
    data.frame(
        year  = bio$year10, # upload as pentads
        month = bio$step,
        areacell = bio$area,
        species = c('MAM'), # generic aquatic mammal
        sampling_type = 'LND',
        length = bio$raw_length,
        age = bio$age,
        weight = bio$raw_weight,
        sex = bio$sex,
        maturity_stage = bio$maturity_stage,
#        vessel=PEL, # ***to be customized for the Baltic???
        stringsAsFactors = TRUE))

# ---------------------------------------------------------------------
minage <- rin.imm[[1]]$minage
maxage <- rin.mat[[1]]$maxage
maxlength <- rin.mat[[1]]$maxlength 
minlength <- rin.imm[[1]]$minlength
dl <- 5

# ---------------------------------------------------------------------
# Query ALK from biological data
ll <- mfdb_interval("len", seq(minlength, maxlength, by = dl))
## names(ll) <- paste("len",minlength:maxlength, sep="")

alk.rin <- mfdb_sample_count(mdb, c('age','length'), list(
    area            = "SD3031",
    timestep        = mfdb_timestep_quarterly,
    year            = 1970:2019,
    sampling_type   = 'LND',
    species         = 'MAM',
    length          = ll,
    age             = mfdb_group('age0'=0,'age1'=1,'age2'=2,'age3'=3,'age4'=4,'age5'=5,'age6'=6,'age7'=7,'age8'=8,'age9'=9,
                                 'age10'=10,'age11'=11,'age12'=12,'age13'=13,'age14'=14,'age15'=15,'age16'=16,'age17'=17,'age18'=18,'age19'=19,
                                 'age20'=20,'age21'=21,'age22'=22,'age23'=23,'age24'=24,'age25'=25),
    data_source     = 'tmp'))[[1]]
    ## data_source     = 'ringed_seal_individuals'))[[1]]

alk.rin$area <- "area1"
names(attributes(alk.rin)$area) <- "area1"
attributes(alk.rin)$area$area1 <- "area1"

# ---------------------------------------------------------------------
# Query maturity
## ll <- mfdb_interval("len", seq(minlength, maxlength, by = dl))
## names(ll) <- paste("len",4:21, sep="")
aa <- mfdb_interval("all",c(minage,maxage), open_ended = c("upper","lower"))
names(aa) <- "all"

mat.rin <- mfdb_sample_count(mdb, c('maturity_stage','age','length'), list(
    area            = "SD3031",
    timestep        = mfdb_timestep_yearly,
    year            = 2000:2020, # previous decades have too few data
    sampling_type   = 'LND',
    species         = 'MAM',
    length          = ll,
    age             = aa,
    maturity_stage  = mfdb_group('rinimm'=1,'rinmat'=5),
    data_source     = 'tmp'))[[1]]


