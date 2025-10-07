## -----------------------------------------------------------------------------
##
## Catch distributions
##
## -----------------------------------------------------------------------------

minage <- lapply(stocks, gadget3::g3_stock_def, 'minage') |> unlist() |> min()
maxage <- lapply(stocks, gadget3::g3_stock_def, 'maxage') |> unlist() |> max()
minlen <- lapply(stocks, gadget3::g3_stock_def, 'minlen') |> unlist() |> min()
maxlen <- 
  lapply(stocks, gadget3::g3_stock_def, 'maxlen') |> 
  unlist() |> (\(.) replace(., is.infinite(.), NA))() |> max(na.rm = TRUE)
dl <- lapply(stocks, function(x) diff(gadget3::g3_stock_def(x, 'minlen'))) |> unlist() |> min()
  
## Query length data to create catch distribution components
## AUTUMN SURVEY
ldist.aut <-
  mfdb_sample_count(mdb, 
                    c('age', 'length'), 
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'AUT',
                      age = mfdb_interval("all",c(minage,maxage),
                                       open_ended = c("upper","lower")),
                      length = mfdb_interval("len", 
                                             seq(minlen, maxlen, by = dl),
                                             open_ended = c("upper","lower"))),
                      defaults))

## Maturity
matp.aut <- 
  mfdb_sample_count(mdb, c('maturity_stage','age','length'),
                    append(defaults,
                           list(sampling_type='AUT',
                                #age=mfdb_group(mat_ages=minage:maxage),
                                length = mfdb_interval('len',
                                                       seq(minlen, maxlen, by = 2*dl),
                                                       open_ended = c('lower','upper')),              
                                maturity_stage = mfdb_group(blingimm = 1, blingmat = 2:5))))

matp.aut_F <- 
  mfdb_sample_count(mdb, c('maturity_stage','age','length'),
                    append(defaults,
                           list(sampling_type='AUT',
                                sex = 'F',
                                #age=mfdb_group(mat_ages=minage:maxage),
                                length = mfdb_interval('len',
                                                       seq(minlen, maxlen, by = 2*dl),
                                                       open_ended = c('lower','upper')),              
                                maturity_stage = mfdb_group(blingimm = 1, blingmat = 2:5))))

for(i in seq_along(matp.aut)){
  attributes(matp.aut[[i]])$age$all <- minage:maxage
}
for(i in seq_along(matp.aut_F)){
  attributes(matp.aut_F[[i]])$age$all <- minage:maxage
}

ldist.aut[[1]]$step <- 4
matp.aut[[1]]$step <- 4

## LONGLINE
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
                                             seq(minlen, maxlen, by = dl),
                                             open_ended = c("upper","lower"))),
                      defaults))

## BOTTOM TRAWL LENGTH AND AGE
ldist.bmt <- 
  mfdb_sample_count(mdb,
                    c('age', 'length'), 
                    c(list(
                      sampling_type = 'SEA',
                      data_source = 'iceland-ldist',
                      gear=c('BMT','NPT','DSE','PSE','PGT','SHT','GIL'),
                      age = mfdb_interval("all",c(minage,maxage),
                                          open_ended = c("upper","lower")),
                      length = mfdb_interval("len", 
                                             seq(minlen, maxlen, by = dl),
                                             open_ended = c("upper","lower"))),
                      defaults))

ldist.comm <- 
  mfdb_sample_count(mdb,
                    c('age', 'length'), 
                    c(list(
                      sampling_type = 'SEA',
                      data_source = 'iceland-ldist',
                      gear=c('BMT','NPT','DSE','PSE','PGT','SHT','GIL','HLN','LLN'),
                      age = mfdb_interval("all",c(minage,maxage),
                                          open_ended = c("upper","lower")),
                      length = mfdb_interval("len", 
                                             seq(minlen, maxlen, by = dl),
                                             open_ended = c("upper","lower"))),
                      defaults))

aldist.bmt <-
  mfdb_sample_count(mdb,
                    c('age', 'length'),
                    c(list(sampling_type = 'SEA',
                           data_source = 'iceland-aldist',
                           gear=c('BMT','NPT','DSE','PSE','PGT','SHT','GIL'),
                           age = mfdb_interval('age',c(minage:maxage),open_ended = c('upper')),
                           length = mfdb_interval("len",
                                                  seq(minlen, maxlen, by = dl),
                                                  open_ended = c("upper","lower"))),
                      defaults))

library(mar)
mar <- connect_mar()

ldist.is <-
  list(
    mar::les_stod(mar) %>% 
      dplyr::left_join(les_syni(mar), by = "stod_id") %>% 
      dplyr::left_join(les_lengd(mar), by = "synis_id") %>% 
      mar::skala_med_taldir() %>% 
      dplyr::left_join(tidypax:::mfdb_gear_mapping(mar), by = "veidarfaeri") %>% 
      dplyr::filter(synaflokkur_nr %in% 10:11) |> 
      inner_join(
        les_aldur(mar) |> filter(tegund_nr == 7, aldur > 0) |> select(synis_id, tegund_nr)
      ) |> 
      mutate(step = case_when(man %in% 1:3 ~ 1,
                              man %in% 4:6 ~ 2,
                              man %in% 7:9 ~ 3, .default = 4),
             area = '1') |> 
      collect(n = Inf) |> 
      drop_na(lengd) |> 
      rename(year = ar, length = lengd) |>
      select(year, step, area, length, synaflokkur_nr) |> 
      mutate(length = case_when(length < 21 ~ 21,
                                length > 139 ~ 139,
                                .default = length)) |> 
      mutate(bin = cut(length, breaks = seq(20, 140, by = 4), right = FALSE)) |> 
      group_by(year, step, area) |> 
      count(bin) |> 
      mutate(tmp = str_sub(bin, 2,-2)) |> 
      separate(tmp, c('length', 'lmax'), sep = ',') |> 
      select(year, step, area, length, number = n) |> 
      mutate(length = paste0('len', length),
             step = as.character(step)) |> 
      ungroup() |> 
      filter(year > 1940) 
  )

aldist.is <-
  list(
    les_syni(mar) |>
      left_join(les_stod(mar)) |>
      filter(ar %in% defaults$year) |>
      inner_join(les_aldur(mar) |> filter(tegund_nr == 7, aldur > 0)) |> 
      filter(synaflokkur_nr %in% c(10,11)) |> 
      mutate(step = case_when(man %in% 1:3 ~ 1,
                              man %in% 4:6 ~ 2,
                              man %in% 7:9 ~ 3, .default = 4),
             area = '1') |> 
      collect(n = Inf) |> 
      drop_na(aldur, lengd) |> 
      rename(year = ar, length = lengd, age = aldur) |>
      select(year, step, area, age, length, synaflokkur_nr) |> 
      mutate(length = case_when(length < 21 ~ 21,
                                length > 139 ~ 139,
                                .default = length)) |> 
      mutate(bin = cut(length, breaks = seq(20, 140, by = 4), right = FALSE)) |> 
      group_by(year, step, area, age) |> 
      count(bin) |> 
      mutate(tmp = str_sub(bin, 2,-2)) |> 
      separate(tmp, c('length', 'lmax'), sep = ',') |> 
      select(year, step, area, age, length, number = n) |> 
      mutate(age = paste0('age', age),
             length = paste0('len', length),
             step = as.character(step)) |> 
      ungroup()
  )

DBI::dbDisconnect(mar)

attributes(aldist.is[[1]])$year <- attributes(aldist.bmt[[1]])$year
attributes(aldist.is[[1]])$step <- attributes(aldist.bmt[[1]])$step
attributes(aldist.is[[1]])$area <- attributes(aldist.bmt[[1]])$area
attributes(aldist.is[[1]])$age <- attributes(aldist.bmt[[1]])$age
attributes(aldist.is[[1]])$length <- attributes(aldist.bmt[[1]])$length

attributes(ldist.is[[1]])$year <- attributes(ldist.bmt[[1]])$year
attributes(ldist.is[[1]])$step <- attributes(ldist.bmt[[1]])$step
attributes(ldist.is[[1]])$area <- attributes(ldist.bmt[[1]])$area
attributes(ldist.is[[1]])$length <- attributes(ldist.bmt[[1]])$length


## OUTPUT
if (defaults$save_bootstrap_data){
  save(ldist.aut, matp.aut, matp.aut_F,
       ldist.lln, ldist.bmt, aldist.bmt, ldist.comm, aldist.is, ldist.is,
       file = file.path(base_dir, 'data', 'catchdistribution_bootstrap_data.Rdata'))
}else{
  save(ldist.aut, matp.aut, matp.aut_F,
       ldist.lln, ldist.bmt, aldist.bmt, ldist.comm, aldist.is, ldist.is,
       file = file.path(base_dir, 'data', 'catchdistribution_data.Rdata'))
}

