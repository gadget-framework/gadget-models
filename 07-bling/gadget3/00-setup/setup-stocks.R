## -----------------------------------------------------------------------------
##
## SETUP MODEL STOCKS
##
## -----------------------------------------------------------------------------

if (single_stock_model){
  
  single_stock <- 
    gadget3::g3_stock(c(species = 'bling'), seq(20, 140, 4)) %>% 
    gadget3::g3s_livesonareas(areas[c('1')]) %>% 
    gadget3::g3s_age(minage = 3, maxage = 20)
  
  stocks <- list(single_stock)
  
}else{
  
  ## Immature
  imm_stock <- 
    gadget3::g3_stock(c(species = 'bling', 'imm'), seq(20, 140, 4)) %>%
    gadget3::g3s_livesonareas(areas[c('1')]) %>%
    gadget3::g3s_age(minage = 3, maxage = 15)
  
  ## Mature
  mat_stock <- 
    gadget3::g3_stock(c(species = 'bling', 'mat'), seq(20, 140, 4)) %>%
    gadget3::g3s_livesonareas(areas[c('1')]) %>%
    gadget3::g3s_age(minage = 5, maxage = 20)

  stocks <- list(imm_stock, mat_stock)  
  
}
