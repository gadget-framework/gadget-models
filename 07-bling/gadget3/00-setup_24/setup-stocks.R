## -----------------------------------------------------------------------------
##
## SETUP MODEL STOCKS
##
## -----------------------------------------------------------------------------

if (single_stock_model){
  
  single_stock <- 
    g3_stock(c(species = tolower(species_name)), seq(20, 140, 4)) |> 
    g3s_livesonareas(areas[c('1')]) |> 
    g3s_age(minage = 3, maxage = 20)
  
  ## Dummy stock for projections
  dummy_stock <- 
    g3_stock(c(species = tolower(species_name), 'dummy'),
             lengthgroups = seq(min(g3_stock_def(single_stock, 'minlen')),
                                tail(g3_stock_def(single_stock, 'maxlen'), n = 2)[[1]],
                                min(g3_stock_def(single_stock, 'dl')))) |> 
    g3s_livesonareas(areas[c('1')]) |> 
    g3s_age(minage = 0, maxage = g3_stock_def(single_stock, 'minage') - 1)
  
  stocks <- list(single_stock)
  
}else{
  
  ## Immature
  imm_stock <- 
    g3_stock(c(species = tolower(species_name), 'imm'), seq(20, 112, 4)) |>
    g3s_age(minage = 3, maxage = 15) |> 
    g3s_livesonareas(areas[c('1')]) 
    
  
  ## Mature
  mat_stock <- 
    g3_stock(c(species = tolower(species_name), 'mat'), seq(36, 140, 4)) |>
    g3s_age(minage = 5, maxage = 20) |> 
    g3s_livesonareas(areas[c('1')]) 
    
  
  ## Dummy stock for projections
  dummy_stock <- 
    g3_stock(c(species = tolower(species_name), 'dummy'),
             lengthgroups = seq(min(g3_stock_def(imm_stock, 'minlen')),
                                tail(g3_stock_def(imm_stock, 'maxlen'), n = 2)[[1]],
                                min(g3_stock_def(imm_stock, 'dl')))) |> 
    g3s_age(minage = 0, maxage = g3_stock_def(imm_stock, 'minage') - 1) |> 
    g3s_livesonareas(areas[c('1')]) 

  stocks <- list(imm_stock, mat_stock)  
  
}
