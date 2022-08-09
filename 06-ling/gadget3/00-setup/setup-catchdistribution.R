## -----------------------------------------------------------------------------
## Catch age and length distributions:
## -----------------------------------------------------------------------------

minage <- gadget3:::stock_definition(imm_stock, 'minage')
maxage <- gadget3:::stock_definition(mat_stock, 'maxage')
minlength <- gadget3:::stock_definition(imm_stock, 'lengthgroups') %>% min()
maxlength <- gadget3:::stock_definition(mat_stock, 'lengthgroups') %>% max()
dl <- gadget3:::stock_definition(mat_stock, 'stock__dl') %>% min()

## Query length data to create IGFS catch distribution components
ldist.igfs <-
  mfdb_sample_count(mdb,
                    c('age', 'length'),
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'IGFS',
                      age = mfdb_interval("all",c(minage,maxage),
                                       open_ended = c("upper","lower")),
                      length = mfdb_interval("len",
                                             seq(minlength, maxlength, by = dl),
                                             open_ended = c("upper","lower"))),
                      defaults))

# for(i in seq_along(ldist.igfs)){
#   attributes(ldist.igfs[[i]])$age$all <- minage:maxage
#   attr(attributes(ldist.igfs[[i]])$length$len0,'min') <- minlength
# }


## Age IGFS
aldist.igfs <-
  mfdb_sample_count(mdb,
                    c('age', 'length'),
                    c(list(sampling_type = 'IGFS',
                           data_source = 'iceland-aldist',
                           age = mfdb_interval('age',c(minage:maxage),open_ended = c('upper')),
#                           age = mfdb_interval('age',c(minage:maxage),open_ended = c('upper')),
                           length = mfdb_interval("len",
                                                  seq(minlength, maxlength, by = dl),
                                                  open_ended = c("upper","lower"))),
                      defaults))
# for(i in seq_along(aldist.igfs)){
#   attr(attributes(aldist.igfs[[i]])$length$len0,'min') <- minlength
# }

matp.igfs <-
  mfdb_sample_count(mdb, c('maturity_stage','age','length'),
                    append(defaults,
                           list(sampling_type='IGFS',
                                age=mfdb_group(mat_ages=minage:maxage),
                                length = mfdb_interval('len',
                                                       seq(minlength, maxlength, by = 2*dl),
                                                       open_ended = c('lower','upper')),
                                maturity_stage = mfdb_group(lingimm = 1, lingmat = 2:5))))




ldist.lln <-
  mfdb_sample_count(mdb,
                    c('age', 'length'),
                    c(list(
                      sampling_type = 'SEA',
                      data_source = 'iceland-ldist',
                      gear = c('LLN','HLN'),
                      age = mfdb_interval("all",c(minage,maxage),
                                          open_ended = c("upper","lower")),
                      length = mfdb_interval("len",
                                             seq(minlength, maxlength, by = dl),
                                             open_ended = c("upper","lower"))),
                      defaults))


aldist.lln <-
  mfdb_sample_count(mdb,
                    c('age', 'length'),
                    c(list(sampling_type = 'SEA',
                           data_source = 'iceland-aldist',
                           gear = c('LLN','HLN'),
                           age =  mfdb_interval('age',c(minage:maxage),open_ended = c('upper')),
#                           age =  mfdb_interval('age',c(minage:maxage),open_ended = c('upper')),
                           length = mfdb_interval("len",
                                                  seq(minlength, maxlength, by = dl),
                                                  open_ended = c("upper","lower"))),
                      defaults))

ldist.bmt <-
  mfdb_sample_count(mdb,
                    c('age', 'length'),
                    c(list(
                      sampling_type = 'SEA',
                      data_source = 'iceland-ldist',
                      gear=c('BMT','NPT','DSE','PSE','PGT','SHT'),
                      age = mfdb_interval("all",c(minage,maxage),
                                          open_ended = c("upper","lower")),
                      length = mfdb_interval("len",
                                             seq(minlength, maxlength, by = dl),
                                             open_ended = c("upper","lower"))),
                      defaults))

aldist.bmt <-
  mfdb_sample_count(mdb,
                    c('age', 'length'),
                    c(list(sampling_type = 'SEA',
                           data_source = 'iceland-aldist',
                           gear=c('BMT','NPT','DSE','PSE','PGT','SHT'),
                           age = mfdb_interval('age',c(minage:maxage),open_ended = c('upper')),
#                           age = mfdb_interval('age',c(minage:maxage),open_ended = c('upper')),
                           length = mfdb_interval("len",
                                                  seq(minlength, maxlength, by = dl),
                                                  open_ended = c("upper","lower"))),
                      defaults))

ldist.gil <-
  mfdb_sample_count(mdb, c('age', 'length'),
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'SEA',
                      gear='GIL',
                      age = mfdb_interval("all",c(minage,maxage),
                                          open_ended = c("lower")),
                      length = mfdb_interval("len",
                                             seq(minlength, maxlength, by = dl),
                                             open_ended = c("upper","lower"))),
                      defaults))


aldist.gil <-
  mfdb_sample_count(mdb, c('age', 'length'),
                    c(list(sampling_type = 'SEA',
                           data_source = 'iceland-aldist',
                           gear='GIL',
                           age =  mfdb_interval('age',c(minage:maxage),open_ended = c('upper')),
#                           age =  mfdb_interval('age',c(minage:maxage),open_ended = c('upper')),
                           length = mfdb_interval("len",
                                                  seq(minlength, maxlength, by = dl),
                                                  open_ended = c("upper","lower"))),
                      defaults))


if (TRUE){
  save(aldist.bmt, ldist.bmt, 
       aldist.lln, ldist.lln,
       aldist.igfs, ldist.igfs,
       aldist.gil, ldist.gil, 
       matp.igfs, file.path(base_dir, 'data', 'catchdistribution_data.Rdata'))
}
