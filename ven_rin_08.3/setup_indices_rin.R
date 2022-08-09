# Make index abundance (N) based on population estimates

si.rin <- mfdb_sample_count(mdb, c('age'), list(
    area          = "SD3031",
    timestep      = 1, #mfdb_timestep_quarterly,
    ## year          = 1991:2020,
    year          = c(1991:2012,2016),
    sampling_type = 'RES',
    data_source   = 'number_onice_ringedseal_bench'))[[1]]
si.rin$area <- "area1"
names(attributes(si.rin)$area) <- "area1"
attributes(si.rin)$area$area1 <- "area1"
attributes(si.rin)$age$all <- c(1,40)

# remove NA
si.rin <- si.rin %>% filter(!is.na(number))

# to set an acoustic type instead
si.rin$age <- "aco.rin"

# assume 30-50% of seals are in the water (=50-70% on ice)
si.rin$number <- si.rin$number * 100 / 70

