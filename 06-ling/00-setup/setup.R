library(mfdb)
library(tidyverse)
library(Rgadget)
source('R/utils.R')
bootstrap <- FALSE
## Create a gadget directory, define some defaults to use with our queries below
gd <- gadget_directory("06-ling/12-new_ass")
mdb<-mfdb('Iceland',db_params=list(host='mfdb.hafro.is'))
vers <- c('12-new_ass', '02-growth_rest')[1]
  year_range <- 1982:2018
base_dir <- '06-ling'
mat_stock <- 'lingmat'
imm_stock <- 'lingimm'
stock_names <- c(imm_stock,mat_stock)
species_name <- 'ling'
## Create a gadget directory, define some defaults to use with our queries below
gd <- gadget_directory(sprintf(paste0("%s/",vers),base_dir))
#mdb<-mfdb('Iceland')#,db_params=list(host='hafgeimur.hafro.is'))

reitmapping <- 
  read.table(
        system.file("demo-data", "reitmapping.tsv", package="mfdb"),
        header=TRUE,
        as.is=TRUE)

defaults <- list(
    area = mfdb_group("1" = unique(reitmapping$SUBDIVISION)),
    timestep = mfdb_timestep_quarterly,
    year = year_range,
    species = 'LIN')

schedule <- 
  expand_grid(year = defaults$year,
              step = defaults$timestep %>% names() %>% as.numeric()) %>% 
  arrange(year,step)


gadgetfile('Modelfiles/time',
           file_type = 'time',
           components = list(list(firstyear = min(defaults$year),
                                  firststep=1,
                                  lastyear=max(defaults$year),
                                  laststep=4,
                                  notimesteps=c(4,3,3,3,3)))) %>% 
  write.gadget.file(gd$dir)

## Write out areafile and update mainfile with areafile location
gadgetfile('Modelfiles/area',
           file_type = 'area',
           components = list(list(areas = 1,
                                  size = 1,
                                  temperature = schedule %>% mutate(area = 1, temperature = 5)))) %>% 
  write.gadget.file(gd$dir)


source('R/utils.R')
source('06-ling/00-setup/setup-fleets.R')
if(vers=='02-growth_rest'|vers=='05-2017noage_growth_rest'){
  source(sprintf('%s/00-setup/setup-model_growth_rest.R',base_dir))
} else {
  source(sprintf('%s/00-setup/setup-model.R',base_dir))}
source('06-ling/00-setup/setup-catchdistribution.R')
source('06-ling/00-setup/setup-indices.R')
source('06-ling/00-setup/setup-likelihood.R')

Sys.setenv(GADGET_WORKING_DIR=normalizePath(gd$dir))
callGadget(l=1,i='params.in',p='params.init')

if(FALSE){
  source('06-ling/00-setup/setup-fixed_slope.R')
  ## setting up model variants
  source('06-ling/00-setup/setup-est_slope.R')
  #source('06-ling/00-setup/setup-three_fleets.R')
  source('06-ling/00-setup/setup-single_fleet.R')
}


if(bootstrap){
  source('06-ling/00-setup/setup-bootstrap.R')
  file.copy(sprintf('%s/bootrun.R','06-ling/00-setup'),gd$dir)
}

file.copy(sprintf('%s/itterfitter.sh','06-ling/00-setup'),gd$dir)
file.copy(sprintf('%s/run.R','06-ling/00-setup'),gd$dir)
file.copy(sprintf('%s/optinfofile','06-ling/00-setup'),gd$dir)
file.copy(sprintf('%s/run-fixed_slope.R','06-ling/00-setup'),gd$dir)
