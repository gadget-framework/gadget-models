## -----------------------------------------------------------------------------
## Survey indices
## -----------------------------------------------------------------------------

## Autumn survey indices
aut.SI <- 
  mfdb_sample_count(mdb, 
                    c('length'), 
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'AUT',
                      length = mfdb_interval('len', si_len_breaks, open_ended = c('lower', 'upper'))),
                      defaults)) %>%
  purrr::map(function(y){
    y %>% 
      split(.,~length) %>% 
      purrr::map(function(x){
        structure(x, length = attr(x, 'length')[unique(x$length)])
      })
  }) 


## OUTPUT
if (defaults$save_bootstrap_data){
  save(aut.SI, file = file.path(base_dir, 'data', 'bootstrap_indices.Rdata'))
}else{
  save(aut.SI, file = file.path(base_dir, 'data', 'indices.Rdata'))  
}


