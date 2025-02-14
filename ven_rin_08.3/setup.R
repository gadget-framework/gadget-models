library(mfdb)
library(tidyverse)
library(Rgadget)

rm(list=ls())

## Create a gadget directory, define some defaults to use with our queries below
dirName <- "ven-rin03"
system(paste("rm -r", dirName))
if(sum(match(list.files(),dirName), na.rm=T)==1){
    print(paste("folder",dirName,"exists"))
} else {gd <- gadget_directory(dirName)}

mdb <- mfdb('Baltic', db_params=list(dbname="bothnia"))

# load ICES rect and subd for convenience
source("area_units.R")

source('setup_ven.R')
source('setup_rin.R')
source('setup_other.R')
source('setup_param_fromSingleSpp.R')
source('setup_print.R')
