g3_iterative_setup <- function(lik_out,
                               grouping = list()){
  
  g_df <- 
    grouping %>% 
    purrr::map(~tibble::tibble(comp = .)) %>% 
    dplyr::bind_rows(.id = 'group') %>% 
    dplyr::bind_rows(tibble::tibble(comp = NA_character_,
                                    group = NA_character_))
  
  param <- attr(lik_out,'param')
  
  #out <- attributes(model(param$value))
  
  num_df <- 
    lik_out %>% 
    dplyr::mutate(param_name = paste0(comp,'_weight'),
                  comp = gsub('^.dist_[a-z]+_','',comp)) %>% 
    dplyr::left_join(g_df,by='comp') %>% 
    dplyr::mutate(init_weight = 1/value,
                  group = purrr::map2(group,comp,~tidyr::replace_na(.x,.y)) %>% 
                    unlist())
  param$value[num_df$param_name] <- 
    num_df$init_weight
  
  
  num_df %>% 
    split(.$group) %>% 
    purrr::map(
      function(x){
        param$value[x$param_name] <- 
          x$init_weight*1e4
        param
      }
    )
}


g3_lik_out <- function(model,param){
  if(!('data.frame' %in% class(param) & 
       sum(c("switch", "value","optimise" ) %in% names(param))==3)){
    stop('Error in param, expected data.frame')
  }
  
  res <- model(param$value)
  out <- attributes(res)
  nll <- res[[1]]
  
  lik.out <- 
    out[grep('dist_.+_obs__num',names(out),value = TRUE)] %>% 
    purrr::map(~sum(.>0)) %>% 
    purrr::map(~tibble::tibble(df = .)) %>% 
    dplyr::bind_rows(.id = 'comp') %>% 
    dplyr::mutate(comp = gsub('_obs__num','',comp)) %>% 
    dplyr::left_join(
      out[grep('nll_.dist_.+__num',names(out),value = TRUE)] %>% 
        purrr::map(sum) %>% 
        purrr::map(~tibble::tibble(value = .)) %>% 
        dplyr::bind_rows(.id = 'comp') %>% 
        dplyr::mutate(comp = gsub('nll_(.+)__num$','\\1',comp)),
      by = 'comp') %>% 
    dplyr::left_join(param %>% 
                       select(comp = switch,weight=value) %>% 
                       mutate(comp = gsub('_weight','',comp),
                              weight = unlist(weight)),
                     by = 'comp')
  attr(lik.out, 'param') <- param
  attr(lik.out, 'actions') <- attr(model,'actions')
  attr(lik.out, 'model_out') <- out
  attr(lik.out, 'nll')
  return(lik.out)
}

# g3_iterative_make_fun <- function(tmb_model,param){
#   obj_fun <- g3_tmb_adfun(tmb_model,param)
#   attr(obj_fun,'param') <- param
# }

g3_iterative_run <- function(param, tmb_model){
  #tmb_model <- g3_to_tmb(attr(model,'actions'))
  
  # Compile and generate TMB ADFun (see ?TMB::MakeADFun)
  obj_fun <- g3_tmb_adfun(tmb_model,param)
  
  # if (opt_bounded){
  #   fit.opt <- nlminb(g3_tmb_par(tmb_param),
  #                     model_tmb$fn, 
  #                     model_tmb$gr,
  #                     upper = g3_tmb_upper(tmb_param),
  #                     lower = g3_tmb_lower(tmb_param),
  #                     control = list(trace = 2, iter.max=1000))
  # }
  # else{
    fit.opt <- optim(g3_tmb_par(param),
                     obj_fun$fn,
                     obj_fun$gr,
                     method = 'BFGS',
                     control = list(trace = 2,maxit = 1000, reltol = .Machine$double.eps^2))
    
  # }
  
  p <- g3_tmb_relist(param,fit.opt$par)
  param$value[names(p)] <- p
  return(param)
  
}

g3_iterative_final <- function(lik_out_list, jitter_params = FALSE){
  weights <- 
    lik_out_list %>% 
    dplyr::bind_rows(.id = 'group') %>% 
    dplyr::group_by(comp) %>% 
    dplyr::filter(value == min(value)) %>% 
    dplyr::select(comp,df,value) %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(weight = ifelse(value==0,0,df/value),
                  param_name = paste0(comp,'_weight'))
  
  params <- 
    lik_out_list %>% 
    purrr::map(~attr(.,'param')) %>% 
    purrr::map(function(x){
      x$value[weights$param_name] <- 
        weights$weight
      x
    })
  if (jitter_params) params <- lapply(params, FUN = g3_jitter, jitter_fraction = 0.2)
  return(params)
}
