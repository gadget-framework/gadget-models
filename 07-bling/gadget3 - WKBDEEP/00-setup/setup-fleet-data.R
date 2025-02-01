## -----------------------------------------------------------------------------
##
## Catches by fleet
##
## -----------------------------------------------------------------------------

## Survey
aut_landings <- 
  structure(data.frame(year = defaults$year, step = 4, area = 1, total_weight = 1),
            area_group = mfdb_group(`1` = 1))

## Survey
deep_landings <- 
  structure(expand.grid(year = 1978:1985, step = c(1,2,3,4), area = 1, total_weight = 1),
            area_group = mfdb_group(`1` = 1))

## Long- & Handline
lln_landings <- mfdb_sample_totalweight(mdb, NULL, c(list(
  gear=c('HLN','LLN'),
  sampling_type = 'LND',
  species = defaults$species),
  defaults))
  
## Bottom trawl etc (Gilnet included here)
bmt_landings_tmp <- mfdb_sample_totalweight(mdb, NULL, c(list(
  gear=c('BMT','NPT','DSE','PSE','PGT','SHT','GIL'),
  sampling_type = 'LND',
  species = defaults$species),
  defaults))

if (TRUE){
  ## Area 14
  gn_landings <- 
    read_csv("benchmarks/WKBDEEP/data/greenland/logbooks_00-23.csv") |> 
    filter(code == 'BLI',
           area %in% c('ices14a', 'ices14b')) |> 
    mutate(month = month(time1),
           step = case_when(month %in% 1:3 ~ 1,
                            month %in% 4:6 ~ 2,
                            month %in% 7:9 ~ 3,
                            .default = 4),
           step = as.character(step),
           area = "1") |> 
    group_by(year, step, area) |> 
    summarise(total_weight = sum(catch_t)*1e3)
}else{
  
  defaults2 <- defaults
  defaults2$area <- mfdb_group("1" = sort(unique(c(reitmapping$SUBDIVISION, qq))))
  
  gn_landings <- 
    mfdb_sample_totalweight(mdb, NULL, c(list(
      sampling_type = 'LOG',
      species = defaults$species,
      data_source = "mdas_greenland-logbooks"),
      defaults))
}

bmt_landings <- list()
bmt_landings[[1]] <- 
  bmt_landings_tmp[[1]] |> 
  bind_rows(gn_landings) |> 
  group_by(year, step, area) |> 
  summarise(total_weight = sum(total_weight, na.rm = TRUE), .groups = 'drop')

# bmt_landings_tmp[[1]] |>
#   mutate(o = 'iceland') |>
#   bind_rows(bmt_landings[[1]] |> mutate(o = 'Greenland + Iceland')) |>
#   group_by(year, o) |>
#   summarise(c = sum(total_weight)) |>
#   ggplot(aes(year, c, fill = o)) + geom_bar(stat = 'identity', position = 'dodge')


foreign_landings <-
  mfdb_sample_totalweight(mdb, NULL,
                          c(list(
                            sampling_type = 'FLND',
                            data_source = c('lods.foreign.landings','statlant.foreign.landings'),
                            species = defaults$species),
                            defaults))

## For single fleet option:
comm_landings <- mfdb_sample_totalweight(mdb, NULL, c(list(
  gear=c('BMT','NPT','DSE','PSE','PGT','SHT','GIL','HLN', 'LLN'),
  sampling_type = 'LND',
  species = defaults$species),
  defaults)) 
  

save(lln_landings, bmt_landings, foreign_landings, aut_landings, comm_landings, deep_landings,
     file = file.path(base_dir, "data", "fleet_data.Rdata"))

