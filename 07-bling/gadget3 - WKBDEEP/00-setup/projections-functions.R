plot_stockrecruit <- function(fit, mat, imm, recruitment_age){
  fit$res.by.year %>% 
    dplyr::filter(.data$stock == mat,
                  .data$year > 1999) %>% 
    dplyr::select(.data$year, ssb = .data$total.biomass) %>% 
    dplyr::left_join(fit$res.by.year %>% 
                       dplyr::filter(.data$stock == imm) %>% 
                       dplyr::select(.data$year, .data$recruitment), by = 'year') %>% 
    dplyr::arrange(.data$year) %>%
    dplyr::mutate(r1 = dplyr::lead(.data$recruitment, n = recruitment_age)) %>% 
    #filter(year > 1999) %>% 
    ggplot2::ggplot(ggplot2::aes(.data$ssb/1e6, .data$r1/1e6)) +  
    ggplot2::expand_limits(y = 0, x = 0) + 
    ggplot2::labs(x='SSB (in kt)',y='Recruitment (millions)') +
    #geom_vline(xintercept = local(refPoints$blim)/1e3, lty = 'dashed', col = 'red') + 
    ggplot2::geom_text(ggplot2::aes(label = substring(.data$year, 3, 4))) + 
    ggplot2::theme_bw() + 
    ggplot2::geom_vline(xintercept = 
                          fit$res.by.year %>% 
                          dplyr::filter(.data$stock == 'bli_mat') %>% 
                          dplyr::pull(.data$total.biomass) %>% 
                          min()/1e6, lty = 'dashed')  
}

g3p_run <- function(obj_fun,
                    params, 
                    mat_stock,
                    imm_stock,
                    f_ages,
                    rec_stock = NULL,
                    rec_step = 1,
                    rec_age = NULL,
                    sexratio = NULL,
                    ncores = parallel::detectCores()){
  
  
  res <- parallel::mclapply(setNames(names(params), names(params)), function(x){
    ## Get model reports
    reports <- obj_fun$report(g3_tmb_par(params[[x]]))
    ## Process reports
    res <- g3p_process_reports(reports, mat_stock, imm_stock, f_ages, rec_stock,
                               rec_step, rec_age, sexratio = sexratio)
    res$id <- x
    return(res)
  }, mc.cores = ncores)
  return(do.call('rbind', res))
}

g3p_process_reports <- function(reports, 
                                mat_stock,
                                imm_stock,
                                f_ages,
                                rec_stock = NULL,
                                rec_step = 1,
                                rec_age = NULL,
                                sexratio = NULL){
  
  
  ## ADD timesteps argument or find timesteps from reports (for fbar)
  ## ADD find array locations using names of dimensions instead of hardcoding
  ## HR option
  ## Better way of dealing with stocks
  
  if (length(f_ages) == 2) f_ages <- f_ages[1]:f_ages[2]
  if (is.null(rec_stock)) rec_stock <- imm_stock
  
  ## Recruitment
  numR <- g3_array_agg(ar = reports[grepl(paste0('dstart_', rec_stock, '__num$'), names(reports))][[1]],
                       agg = sum,
                       margins = c('year'),
                       step = (rec_step + 1),
                       age = rec_age)
  out <- data.frame(year = as.numeric(names(numR)), rec5 = numR)
  
  numR0 <- g3_array_agg(ar = reports[grepl(paste0('dstart_', rec_stock, '__num$'), names(reports))][[1]],
                        agg = sum,
                        margins = c('year'),
                        step = (rec_step + 1),
                        age = 2)
  out <- merge(out, data.frame(year = as.numeric(names(numR0)), rec = numR0), by = 'year')
  
  ## SSB
  if (TRUE){
    
    ssbR <- g3_array_agg(ar = reports[grepl(paste0('dstart_', mat_stock, '__num$'), names(reports))][[1]],
                         agg = sum,
                         margins = c('year','age','length'),
                         step = 1) |> 
      as.data.frame.table(stringsAsFactors = FALSE) |> 
      gadgetutils:::split_length() |> 
      mutate(length = (ifelse(is.infinite(upper),lower + 4, upper) + lower )/2) |> 
      left_join(sexratio, by = 'length') |> 
      mutate(n = Freq*prop, year = as.integer(year)) |> 
      select(length, age, year, n) |> 
      left_join(
        reports[grepl(paste0('dstart_', mat_stock, '__wgt$'), names(reports))][[1]] |> 
          as.data.frame.table(stringsAsFactors = FALSE) |> 
          gadgetutils::extract_year_step() |> 
          gadgetutils:::split_length() |> 
          mutate(length = (ifelse(is.infinite(upper),lower + 4, upper) + lower )/2) |> 
          filter(step == 1) |> 
          select(length, age, year, wgt = Freq)
        , by = c('length', 'age', 'year')) |> 
      mutate(ssb = n*wgt) |> 
      group_by(year) |> summarise(ssb = sum(ssb))
    
  }else{
    ssbR <- g3_array_agg(ar =
                           reports[grepl(paste0('dstart_', mat_stock, '__num$'), names(reports))][[1]] *
                           reports[grepl(paste0('dstart_', mat_stock, '__wgt$'), names(reports))][[1]],
                         agg = sum,
                         margins = c('year'),
                         step = 1)
    ssbR <- data.frame(year = as.numeric(names(ssbR)), ssb = ssbR)
    
  }
  
  out <- merge(out, ssbR, by = 'year')
  
  ## Catches
  tmp <- 
    do.call('rbind',
            lapply(reports[grepl('__predby_', names(reports))], function(x){
              g3_array_agg(x, agg = sum, margins = 'year') |> stack() 
            })
    )
  catR <- aggregate(list(catch = tmp$values), by = list(year = tmp$ind), FUN = sum)
  rm(tmp)
  out <- merge(out, catR, by = 'year')
  
  ## Fbar
  ## Aggregate catch numbers over fleets and stocks
  imm_ar <-
    Reduce('+', reports[grepl(paste0('^detail_', imm_stock, '__predby_'), names(reports))]) / 
    reports[grepl(paste0('dstart_', imm_stock, '__wgt$'), names(reports))][[1]]
  imm_ar[is.na(imm_ar)] <- 0
  
  mat_ar <-
    Reduce('+', reports[grepl(paste0('^detail_', mat_stock, '__predby_'), names(reports))]) / 
    reports[grepl(paste0('dstart_', mat_stock, '__wgt$'), names(reports))][[1]]
  mat_ar[is.na(mat_ar)] <- 0
  
  ## Catch numbers
  catN <- 
    g3_array_combine(list(
      g3_array_agg(ar = imm_ar, agg = sum, margins = c('year', 'step', 'age')),
      g3_array_agg(ar = mat_ar, agg = sum, margins = c('year', 'step', 'age'))   
    ))
  
  ## Stock numbers
  N0 <- 
    g3_array_combine(list(
      g3_array_agg(ar = reports[grepl(paste0('dstart_', mat_stock, '__num$'), names(reports))][[1]], agg = sum, margins = c('year', 'step', 'age')),
      g3_array_agg(ar = reports[grepl(paste0('dstart_', imm_stock, '__num$'), names(reports))][[1]], agg = sum, margins = c('year', 'step', 'age'))
    ))
  
  ## Fbar
  fbar <- 4*(-log(1 - 
                    apply(catN[paste0('age', f_ages),,],2:3,sum)/
                    apply(N0[paste0('age', f_ages),,],2:3,sum)))
  fbar[is.na(fbar)] <- 0
  
  
  if ('age' %in% names(dim(fbar))) fbar <- apply(fbar, MARGIN = 3, FUN = mean)
  else fbar <- apply(fbar, MARGIN = 2, FUN = mean)
  
  fbar <- data.frame(year = as.numeric(names(fbar)), fbar = fbar)
  
  return(merge(out, fbar, by = 'year'))
  
}


## OLD VERSION
run_proj <- function(adfun, pars){
  
  reports <- adfun$report(gadget3::g3_tmb_par(pars))
  
  matbio <- 
    apply(reports$detail_bli_mat__num*reports$detail_bli_mat__wgt, 4, sum)
  immbio <- 
    apply(reports$detail_bli_imm__num*reports$detail_bli_imm__wgt, 4, sum)
  catch <- 
    apply(reports$detail_bli_mat__predby_bmt + 
            reports$detail_bli_mat__predby_bmt_proj + 
            reports$detail_bli_mat__predby_lln + 
            reports$detail_bli_mat__predby_lln_proj, 4, sum) + 
    apply(reports$detail_bli_imm__predby_bmt + 
            reports$detail_bli_imm__predby_bmt_proj + 
            reports$detail_bli_imm__predby_lln + 
            reports$detail_bli_imm__predby_lln_proj, 4, sum)
  rec <- 
    apply(reports$detail_bli_imm__num[,,1,],2,sum)
  
  matfbar <-
    apply(((reports$detail_bli_mat__predby_lln +
              reports$detail_bli_mat__predby_lln_proj +
              reports$detail_bli_mat__predby_bmt +
              reports$detail_bli_mat__predby_bmt_proj)/reports$detail_bli_mat__wgt)[,,paste0('age',20:20),],
          2, sum, na.rm = TRUE)/apply(reports$detail_bli_mat__num[,,paste0('age',20:20),],2,sum, na.rm = TRUE)
  
  matfbar <- 4*(-log(1 - matfbar))
  #matfbar <- gadgetutils::extract_year_step( data.frame(time = names(matfbar), mortality = matfbar) )
  #matfbar <- aggregate(list(fbar = matfbar$mortality), by = list(year = matfbar$year), FUN = mean, na.rm = TRUE)
  #matfbar <- apply(matfbar, 2, mean, na.rm = TRUE)
  
  immfbar <- 
    apply(((reports$detail_bli_imm__predby_lln +
              reports$detail_bli_imm__predby_lln_proj +
              reports$detail_bli_imm__predby_bmt +
              reports$detail_bli_imm__predby_bmt_proj)/reports$detail_bli_imm__wgt)[,,paste0('age',15:15),], 
          2, sum, na.rm = TRUE)/apply(reports$detail_bli_imm__num[,,paste0('age',15:15),],2,sum, na.rm = TRUE) 
  
  immfbar <- 4*(-log(1 - immfbar))
  #immfbar <- gadgetutils::extract_year_step( data.frame(time = names(immfbar), mortality = immfbar) )
  #immfbar <- aggregate(list(fbar = immfbar$mortality), by = list(year = immfbar$year), FUN = mean, na.rm = TRUE)
  #immfbar <- apply(immfbar, 2, mean, na.rm = TRUE)
  fbar <- pmax(immfbar, matfbar)
  
  
  out <- 
    tibble(time = names(matbio),
           ssb = matbio) |> 
    left_join(
      tibble(time = names(catch),
             catch = catch),
      by = 'time') |> 
    left_join(
      tibble(time = names(fbar),
             fbar = fbar), 
      by = 'time') |> 
    left_join(
      tibble(time = names(rec),
             rec = rec), 
      by = 'time') |> 
    gadgetutils::extract_year_step() |>
    select(year, step, catch, ssb, fbar, rec)
  
  #out[out$step > 1, 'ssb'] <- 0
  #out[out$step != 2, 'rec'] <- 0
  
  return(out)
  
}



