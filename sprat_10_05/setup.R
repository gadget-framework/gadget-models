library(mfdb)
library(tidyverse)
library(Rgadget)

rm(list=ls())

## Create a gadget directory, define some defaults to use with our queries below
dirName <- "sprat01"
system(paste("rm -r", dirName))
if(sum(match(list.files(),dirName), na.rm=T)==1){
    print(paste("folder",dirName,"exists"))
} else {gd <- gadget_directory(dirName)}

mdb <- mfdb('Baltic', db_params=list(dbname="baltic_wgsam2019"))

# load ICES rect and subd for convenience
source("area_units.R")

year_range <- 1974:2018
base_dir <- 'sprat'
mat_stock <- 'sprmat'
imm_stock <- 'sprimm'
stock_names <- c(imm_stock,mat_stock)
species_name <- 'sprat'

# define 'default' spatial and temporal aggregation
defaults.spr <- list(
    year = year_range,
    timestep = mfdb_timestep_quarterly,
    area=mfdb_group('area1'= as.character(squares[squares$SD %in% c("24","25","26","27","28.2","29"),"ICES_Rectangle"])),
    species = 'SPR')

gadgetfile('Modelfiles/time',
           file_type = 'time',
           components = list(list(firstyear = min(defaults.spr$year),
                                  firststep=1,
                                  lastyear=max(defaults.spr$year),
                                  laststep=4,
                                  notimesteps=c(4,3,3,3,3)))) %>% 
write.gadget.file(gd$dir)

## Write out areafile and update mainfile with areafile location
area <- expand.grid(year=min(defaults.spr$year):max(defaults.spr$year),
                    step=1:4,
                    area="area1",
                    mean=5)
area <- arrange(area,year,step)

gadget_areafile(
  size = mfdb_area_size(mdb, defaults.spr)[[1]],
  temperature = area) %>% 
gadget_dir_write(gd,.)

source('utils.R')
source('setup_fleets.R')
source('setup_model.R')
source('setup_catchdistribution.R')
source('setup_catchstatistic.R')
source('setup_indices.R')
source('setup_likelihood.R')

Sys.setenv(GADGET_WORKING_DIR=normalizePath(gd$dir))
file.copy(sprintf('optinfofile','./'),gd$dir)


## callGadget(l=1,i='params.in',p='params.init', log='init.log')

file.copy(sprintf('%s/run.R','./'),gd$dir)
