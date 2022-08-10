## extract data on prey species composition

# predator length groups
## predLenGrp <- c(40, 80, 110, 140, 160)
predLenGrp <- c(40, 160)

df1 <- mfdb_stomach_preyweightratio(mdb,
                       c("predator_length","prey_length"),
                       list(year = 2007:2020,
                            timestep = mfdb_timestep_quarterly,
                            predator_length = mfdb_interval("len", predLenGrp, open_ended=F),
                            prey_length = mfdb_interval("ven", c(3.5,20.5), open_ended=F),
                            prey_species = mfdb_group('vendace'='FVE'),
                            data_source="ringed_seal_stomachs_bench_FVE-PLNcorr"))[[1]]
df2 <- mfdb_stomach_preyweightratio(mdb,
                       c("predator_length","prey_length"),
                       list(year = 2007:2020,
                            timestep = mfdb_timestep_quarterly,
                            predator_length = mfdb_interval("len", predLenGrp, open_ended=F),
                            prey_length = mfdb_interval("her", c(1,25), open_ended=F),
                            prey_species = mfdb_group('her'="HER"),
                            data_source="ringed_seal_stomachs_bench_FVE-PLNcorr"))[[1]]
df3 <- mfdb_stomach_preyweightratio(mdb,
                       c("predator_length","prey_length"),
                       list(year = 2007:2020,
                            timestep = mfdb_timestep_quarterly,
                            predator_length = mfdb_interval("len", predLenGrp, open_ended=F),
                            prey_length = mfdb_interval("other", c(1,30), open_ended=F),
                            prey_species = mfdb_group('other'=c("TSS","SPR","ELP","SDG","SAN","SAE","SME","PLN","FPE","MQS","SLZ","SAL","FBU","TNS","CDY","FCY","FRU","FPI")),
                            data_source="ringed_seal_stomachs_bench_FVE-PLNcorr"))[[1]]

stom1.rin <- mfdb_concatenate_results(df1,df2,df3)

# --------------------------------------
## extract data on prey (vendace) length composition

# predator length groups
## predLenGrp <- c(40, 80, 120, 160) # L50 mat estimated ~118 cm
predLenGrp <- c(40, 160)
preyLenGrp <- seq(8.5, 19.5, 1)

df <- mfdb_stomach_preycount(mdb,
                       c("predator_length","prey_length"),
                       list(year = c(2007,2008,2012,2013,2014,2015,2017,2018,2019,2020),
                            timestep = mfdb_timestep_quarterly,
                            predator_length = mfdb_interval("len", predLenGrp, open_ended=F),
                            prey_length = mfdb_interval("len", preyLenGrp, open_ended=F),
                            prey_species = mfdb_group('vendace'='FVE'),
                            data_source="ringed_seal_stomachs_bench_orig"))[[1]]

# remove selected quarters with too few data
stom2.rin <- df %>%
             filter(!(year==2008 & step==3) &
                    !(year==2014 & step==3) &
                    !(year==2017 & step==3) &
                    !(year==2019 & step==2) &
                    !(year==2020 & step==3) &
                    !(year==2020 & step==4)) %>%
             rename(ratio=number)

