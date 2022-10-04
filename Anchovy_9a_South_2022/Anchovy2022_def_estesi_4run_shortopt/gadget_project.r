require(gadget2)
#devtools::install_github('hafro/rgadget')
require(Rgadget)
# setwd("~/Documents/MATH4FISH/MATH4FISH_Gitlab/gadget-error/Gadget-error_withgadgetproject/Anchovy2022_def_estesi_4run_shortopt")
base_dir <- 'Anchovy2022_def_estesi_4run_shortopt'
vers <- c('01-base')
gd <- gadget.variant.dir(sprintf(paste0("%s/",vers),base_dir))
fit <- gadget_fit( gd= base_dir, params.in = 'params.out',fit="FIT")
