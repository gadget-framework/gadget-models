## -----------------------------------------------------------------------------
## Survey indices
## -----------------------------------------------------------------------------

## Autumn survey indices
aut.SI <- 
  mfdb::mfdb_sample_count(mdb, 
                          c('length'), 
                          c(list(
                            data_source = 'iceland-ldist',
                            sampling_type = 'AUT',
                            length = mfdb::mfdb_interval('len', si_length_intervals, open_ended = c('lower', 'upper'))),
                            defaults)) %>% 
  purrr::map(function(y){
    y %>% 
      split(.,~length) %>% 
      purrr::map(function(x){
        structure(x, length = attr(x, 'length')[unique(x$length)])
        })
    }) 


## OUTPUT
save(aut.SI, file = file.path(base_dir, 'data', 'indices.Rdata'))  



