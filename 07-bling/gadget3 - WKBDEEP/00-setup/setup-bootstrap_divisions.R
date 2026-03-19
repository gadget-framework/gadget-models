library(mfdb)
library(tidyverse)

mdb <- mfdb('Iceland', db_params = list(host = 'mfdb.hafro.is'))

dat <- 
  mfdb_dplyr_sample(mdb) |> 
  left_join(
    mfdb_dplyr_division(mdb) |> select(division, areacell)
    ) |> 
  filter(data_source == 'iceland-ldist',
         sampling_type == 'AUT', 
         species=='BLI',
         year > 2000,
         year != 2011) |>
  group_by(year, division) |> 
  summarise(n = sum(count, na.rm = TRUE)) |> 
  collect(n = Inf) 

nodata_divs <- 
  dat |> 
  rename(name = division) |> 
  group_by(name) |> 
  summarise(n = sum(n)) |> 
  filter(n < 230) |> 
  pull(name)

dat2 <- 
  dat |> 
  rename(name = division) |> 
  filter(!(name %in% nodata_divs)) |> 
  pivot_wider(names_from = name, values_from = n, values_fill = 0)

view(dat2)


  
