
year_range <- 1991:2020
base_dir <- 'vendace'
mat_stock <- 'venmat'
imm_stock <- 'venimm'
stock_names <- c(imm_stock,mat_stock)
species_name <- 'vendace'

# define 'default' spatial and temporal aggregation
defaults.ven <- list(
    area = mfdb_group('area1'=as.character(squares$ICES_Rectangle[squares$SD %in% 30:31])),
    timestep = mfdb_timestep_quarterly,
    year = year_range,
    species = 'FVE')

gadgetfile('Modelfiles/time',
           file_type = 'time',
           components = list(list(firstyear = min(defaults.ven$year),
                                  firststep=1,
                                  lastyear=max(defaults.ven$year),
                                  laststep=4,
                                  notimesteps=c(4,3,3,3,3)))) %>% 
write.gadget.file(gd$dir)

## Write out and update mainfile with areafile location
area <- expand.grid(year=min(defaults.ven$year):max(defaults.ven$year),
                    step=1:4,
                    area="area1",
                    mean=5)
area <- arrange(area,year,step)

gadget_areafile(
  size = mfdb_area_size(mdb, defaults.ven)[[1]],
  ## temperature = mfdb_temperature(mdb, defaults.ven)[[1]]) %>% 
  temperature = area) %>% 
gadget_dir_write(gd,.)

source('utils.R')
source('setup_fleets_ven.R')
source('setup_model_ven.R')
source('setup_catchdistribution_ven.R')
source('setup_indices_ven.R')
source('setup_likelihood_ven.R')

## Sys.setenv(GADGET_WORKING_DIR=normalizePath(gd$dir))
file.copy(sprintf('optinfofile','./'),gd$dir)


## callGadget(l=1,i='params.in',p='params.init', log='init.log')

file.copy(sprintf('%s/run.R','./'),gd$dir)
