## -----------------------------------------------------------------------------
## Survey indices
## -----------------------------------------------------------------------------

igfs.SI1 <- 
  mfdb_sample_count(mdb, c('length'), c(list(
    data_source = 'iceland-ldist',
    sampling_type = 'IGFS',
    length = mfdb_interval("len", c(20,52),open_ended = 'lower')),
    defaults))

igfs.SI2a <- 
  mfdb_sample_count(mdb, c('length'), c(list(
    data_source = 'iceland-ldist',
    sampling_type = 'IGFS',
    length = mfdb_interval("len", c(52,60))),
    defaults))


igfs.SI2b <- 
  mfdb_sample_count(mdb, 
                    c('length'), 
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'IGFS',
                      length = mfdb_interval("len", c(60,72))),
                      defaults))


igfs.SI3a <- 
  mfdb_sample_count(mdb, 
                    c('length'), 
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'IGFS',
                      length = mfdb_interval("len", c(72,80))),
                      defaults))


igfs.SI3b <- 
  mfdb_sample_count(mdb, 
                    c('length'), 
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'IGFS',
                      length = mfdb_interval("len", c(80,92))),
                      defaults))

igfs.SI3c <- 
  mfdb_sample_count(mdb, 
                    c('length'), 
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'IGFS',
                      length = mfdb_interval("len", c(92,100))),
                      defaults))


igfs.SI3d <- 
  mfdb_sample_count(mdb, 
                    c('length'),
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'IGFS',
                      length = mfdb_interval("len", c(100,160),open_ended = 'upper')),
                      defaults))

if (TRUE){
  save(igfs.SI1, igfs.SI2a, igfs.SI2b, 
       igfs.SI3a, igfs.SI3b, igfs.SI3c, igfs.SI3d, 
       file = file.path(base_dir, 'data', 'indices.Rdata'))
}

