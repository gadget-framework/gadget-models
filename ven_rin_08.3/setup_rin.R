library(mfdb)
library(tidyverse)
library(Rgadget)

## rm(list=ls())

## ## Create a gadget directory, define some defaults to use with our queries below
## dirName <- "ringseal_01"
## system(paste("rm -r", dirName))
## if(sum(match(list.files(),dirName), na.rm=T)==1){
##     print(paste("folder",dirName,"exists"))
## } else {gd <- gadget_directory(dirName)}

## mdb <- mfdb('Baltic', db_params=list(dbname="bothnia"))

## # load ICES rect and subd for convenience
## source("area_units.R")

## year_range <- 1991:2020
base_dir <- 'ringseal'
mat_stock <- 'rinmat'
imm_stock <- 'rinimm'
stock_names <- c(imm_stock,mat_stock)
species_name <- 'ringed_seal'

# define 'default' spatial and temporal aggregation (same as vendace)
defaults.rin <- list(
    area = mfdb_group('area1'=as.character(squares$ICES_Rectangle[squares$SD %in% 30:31])),
    timestep = mfdb_timestep_quarterly,
    year = year_range,
    species = 'RIN')

## gadgetfile('Modelfiles/time',
##            file_type = 'time',
##            components = list(list(firstyear = min(defaults.rin$year),
##                                   firststep=1,
##                                   lastyear=max(defaults.rin$year),
##                                   laststep=4,
##                                   notimesteps=c(4,3,3,3,3)))) %>% 
## write.gadget.file(gd$dir)

## Write out areafile and update mainfile with areafile location
## area <- expand.grid(year=min(defaults.rin$year):max(defaults.rin$year),
##                     step=1:4,
##                     area="area1",
##                     mean=5)
## area <- arrange(area,year,step)

## gadget_areafile(
##   size = mfdb_area_size(mdb, defaults.rin)[[1]],
##   ## temperature = mfdb_temperature(mdb, defaults.rin)[[1]]) %>% 
##   temperature = area) %>% 
## gadget_dir_write(gd,.)

## source('utils.R')
source('setup_fleets_rin.R')
source('setup_model_rin.R')
source('setup_catchdistribution_rin.R')
## source('setup_catchstatistic_rin.R')
source('setup_indices_rin.R')
source('setup_stomachs_rin.R')
source('setup_likelihood_rin.R')

## Sys.setenv(GADGET_WORKING_DIR=normalizePath(gd$dir))
## file.copy(sprintf('optinfofile','./'),gd$dir)


## callGadget(l=1,i='params.in',p='params.init', log='init.log')

## file.copy(sprintf('%s/itterfitter.sh','06-ling/00-setup'),gd$dir)
## file.copy(sprintf('%s/run.R','./'),gd$dir)
## file.copy(sprintf('%s/optinfofile','06-ling/00-setup'),gd$dir)
## file.copy(sprintf('%s/run-fixed_slope.R','06-ling/00-setup'),gd$dir)
