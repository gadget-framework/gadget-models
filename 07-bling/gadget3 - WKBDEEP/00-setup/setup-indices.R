## -----------------------------------------------------------------------------
## Survey indices
## -----------------------------------------------------------------------------

create_intervals <-  function (prefix, vect) {
  
  x<-
    structure(vect[1:(length(vect)-1)], names = paste0(prefix, vect[1:(length(vect)-1)])) %>% 
    as.list(.) %>% 
    purrr::map(~structure(seq(.,vect[-1][which(vect[1:(length(vect)-1)]==.)],1)[-length(seq(.,vect[-1][which(vect[1:(length(vect)-1)]==.)],1))], 
                          min = ., 
                          max = vect[-1][which(vect[1:(length(vect)-1)]==.)]))
  
  x[[length(x)]] <- c(x[[length(x)]], attributes(x[[length(x)]])$max) %>% 
    structure(., 
              min = min(.), 
              max = max(.))
  
  
  return(x)
}

## Autumn survey indices
if (stratified_indices){
  
  si_var <- log(si_cv^2+1)
  
  load(file = 'benchmarks/WKBDEEP/gadget3/data/survey_indices.Rdata')
  tmp <- 
    survey_indices |> 
    filter(index == 'Combined') |> 
    mutate(step = '4',
           area = '1',
           length = paste0('len', gsub('(.+) - (.+)', '\\1', index_agg)),
           upper = gsub('(.+) - (.+)', '\\2', index_agg))  |> 
    rename(number = b) |> 
    select(year, step, area, length, number)
  
  aggs <- c(unique(sort(as.numeric(gsub('len', '', tmp$length)))), 140)
  tmp <- structure(tmp, area = '1', length = create_intervals('len',aggs))
  
  aut.SI <- list('x' = list())
  for (i in names(attributes(tmp)$length)){
    aut.SI[['x']][[i]] <- tmp[tmp$length == i,]
    attributes(aut.SI[['x']][[i]])$length <- attributes(tmp)$length[i]
  }
  
  attributes(attributes(aut.SI[[1]]$len20)$length[[1]])$min_open_ended <- TRUE
  attributes(attributes(aut.SI[[1]]$len104)$length[[1]])$max_open_ended <- TRUE
  
  if (defaults$save_bootstrap_data){
    tmp <- aut.SI[[1]]
    aut.SI <- list()
    for (i in 1:nboots){
      aut.SI[[i]] <- list()
      for (j in names(tmp)){
        aut.SI[[i]][[j]] <- 
          tmp[[j]] |>
          mutate(number = rlnorm(dplyr::n(), 
                                 log(number)-si_var/2,
                                 sdlog = sqrt(si_var))) 
      }
    }
  }
  
}else{
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
}


## OUTPUT
if (defaults$save_bootstrap_data){
  save(aut.SI, file = file.path(base_dir, 'data', 'bootstrap_indices.Rdata'))
}else{
  save(aut.SI, file = file.path(base_dir, 'data', 'indices.Rdata'))  
}

# 
# 
# data <- 
#   aut.SI |> 
#   map(bind_rows, .id = 'id') |> 
#   bind_rows(.id = 'id') |> 
#   mutate(type = 'cv') |> 
#   bind_rows(
#     aut.SI2 |> 
#       map(bind_rows, .id = 'id') |> 
#       bind_rows(.id = 'id') |> 
#       mutate(type = 'boot')
#   )
# 
# data |> 
#   ggplot(aes(year, number)) + 
#   geom_ribbon(data = 
#                 data |> 
#                 group_by(year, type, length) |> 
#                 summarise(u = quantile(number, 0.975),
#                           l = quantile(number, 0.025)),
#               aes(x = year, y = NULL, ymin = l, ymax = u, col = type),
#               alpha = 0.2
#               ) + 
#   facet_wrap(~length*type, scales = 'free_y')
# 
# data |> 
#   filter(length == 'len44') |> 
#   ggplot(aes(year, number)) + 
#   geom_line(aes(col = id), show.legend = FALSE) + 
#   facet_wrap(~type, scales = 'free_y')
# 
# 
# 
