## -----------------------------------------------------------------------------
##
## SETUP MODEL
##
## -----------------------------------------------------------------------------

##### Configure stocks #########################################################

## stocks
imm_stock <-
  g3_stock(c(species = 'ling', 'imm'), lengthgroups = seq(20, 160, 4)) %>%
  g3s_livesonareas(areas[c('1')]) %>%
  g3s_age(minage = 3, maxage = 10)

mat_stock <-
  g3_stock(c(species = 'ling', 'mat'), lengthgroups = seq(20, 160, 4)) %>%
  g3s_livesonareas(areas[c('1')]) %>%
  g3s_age(minage = 5, maxage = 15)

## List of stocks
stocks <- list(imm_stock, mat_stock)

###### Compile stock actions ###################################################

## Maximum number of length groups a stock can group within a time step (maxlengthgroupgrowth)
mlgg <- 5

## How should the initial abundance be setup?
## Options:
## 0 - population is initialised at equilibrium
## 1 - parameter for each age group (across stocks)
## 2 - parameter for each age group of each stock
init_abund_mode <- 2

