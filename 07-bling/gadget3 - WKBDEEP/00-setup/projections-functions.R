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

g3p_setup_pars <- function(model, 
                           params, 
                           blim,
                           btrigger,
                           harvest_rates,
                           harvest_rate_trials,
                           fleet_proportions,
                           rec_list,
                           rec_years = NULL,
                           rec_scale = 1e-04,
                           rec_block_size = 7,
                           rec_method = 'bootstrap',
                           project_years = 100,
                           assess_err = FALSE,
                           advice_rho = 0.423,
                           advice_cv = 0.212,
                           blim_pattern = 'spawn_blim',
                           btrigger_pattern  = '*.hf.btrigger',
                           rec_pattern = 'project_rec.[0-9]',
                           hr_pattern = '*.hf.harvest_rate',
                           quota_prop_pattern = '*.quota.prop'){
  
  ## Check model
  if ('g3_r' %in% class(model)) model <- g3_to_tmb(attr(model, 'actions'))
  if (!('g3_cpp' %in% class(model))) stop("The 'model' argument should be a g3 model of class 'g3_r' or 'g3_cpp'")
  
  ## Turn optimise to TRUE for parameters we'll be changing, ie, btrigger,
  ## projected recruits and harvest rates, ensures g3_tmb_par() gets what we need
  base_par <- attr(model, 'parameter_template')
  
  ## Fleet proportions
  all_fleet_switches <- base_par[grepl(quota_prop_pattern, base_par$switch), 'switch']
  fleet_proportions$fleet <- paste0(fleet_proportions$fleet, gsub('\\*', '', quota_prop_pattern))
  if (!all(all_fleet_switches %in% fleet_proportions$fleet)){
    fleet_proportions <- rbind(fleet_proportions, 
                               expand.grid(fleet = all_fleet_switches[!(all_fleet_switches %in% fleet_proportions$fleet)],
                                           prop = 0))
  }
  if (sum(fleet_proportions$prop) != 1){
    warning('Scaling fleet proportions to sum to 1')
    fleet_proportions$prop <- fleet_proportions$prop/sum(fleet_proportions$prop)
  }
  
  ## BASE PARAMETERS FINISHED
  ## ONTO VARIANTS: recruitment, btrigger and harvest rates
  
  ## Check params, turn into list if its a data.frame
  if (is.data.frame(params))  params <- list('base' = params)
  if (!all(names(params[[1]]) %in% names(base_par))) stop("The 'params' argument should be a g3 parameter data.frame or list of g3 parameter data.frames")
  
  ## The setup:
  ## We will simulate the model with each harvest rate (harvest_rates argument)
  ## n number of times, where n = harvest_rate_trials. If assess_err = TRUE,
  ## each replicate will have a unique recruitment series and harvest rate, 
  ## if assess_err = FALSE, each replicate will have a unique recritment series.
  ## This will be setup for each of the input parameters (argument 'params')
  
  simdf <- expand.grid(hr = harvest_rates,
                       rep = 1:length(params),
                       trial = 1:harvest_rate_trials)
  simdf$id <- paste('h', simdf$hr, simdf$trial, simdf$rep, sep = '-')
  
  ## Loop over simdf to create list of input parameters
  out <- 
    lapply(split(simdf, simdf$id), function(x){
      ## Take values from estimated pars
      pars <- base_par
      pars$value[params[[x$rep]]$switch] <- params[[x$rep]]$value
      ## Ensure all estimated pars plus harvest rates, projected recruitments
      ## and btrigger are optimise = TRUE. 
      pars$optimise <- params[[x$rep]]$optimise[match(pars$switch, params[[x$rep]]$switch)]
      pars[grepl(paste(btrigger_pattern, 
                       rec_pattern, 
                       hr_pattern, 
                       sep = '|'), base_par$switch), 'optimise'] <- TRUE
    
      pars <-  
        g3_init_val(pars, btrigger_pattern, btrigger, optimise = TRUE) |> 
        g3_init_val(blim_pattern, blim, optimise = TRUE) |> 
        g3_init_val('project_years', project_years, optimise = FALSE) |> 
        g3experiments::g3p_project_advice_error(hr_pattern = hr_pattern,
                                                hr_target = x$hr,
                                                advice_rho = advice_rho,
                                                advice_cv = ifelse(assess_err, advice_cv, 0))
      
      ## Merge fleet proportions into base parameters
      pars[match(fleet_proportions$fleet, pars$switch),'value'] <- fleet_proportions$prop
      
      ## Projected recruitment may come from a random walk which we will assume 
      ## if the recruitment pattern is not found.
      if (any(grepl(rec_pattern, pars$switch))){
        if (is.null(rec_years)) rec_years <- rec_list[[x$rep]]$year
        pars <- g3experiments::g3p_project_rec(pars, 
                                               rec_pattern = rec_pattern,
                                               recruitment = rec_list[[x$rep]][rec_list[[x$rep]]$year %in% rec_years,],
                                               method = rec_method,
                                               scale = rec_scale,
                                               block_size = rec_block_size) 
      }
      return(pars)
    })
  return(out)
}

g3p_run <- function(obj_fun,
                    params, 
                    mat_stock,
                    imm_stock,
                    f_ages,
                    rec_stock = NULL,
                    rec_step = 1,
                    rec_age = NULL,
                    ncores = parallel::detectCores()){
  
  
  res <- parallel::mclapply(setNames(names(params), names(params)), function(x){
    ## Get model reports
    reports <- obj_fun$report(g3_tmb_par(params[[x]]))
    ## Process reports
    res <- g3p_process_reports(reports, mat_stock, imm_stock, f_ages, rec_stock,
                               rec_step, rec_age)
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
                                rec_age = NULL){
  
  
  ## ADD timesteps argument or find timesteps from reports (for fbar)
  ## ADD find array locations using names of dimensions instead of hardcoding
  ## HR option
  ## Better way of dealing with stocks
  
  
  if (is.null(rec_stock)) rec_stock <- imm_stock
  
  ## Recruitment
  numR <- g3_array_agg(ar = reports[grepl(paste0('dstart_', rec_stock, '__num$'), names(reports))][[1]],
                       agg = sum,
                       margins = c('year'),
                       step = (rec_step + 1),
                       age = rec_age)
  out <- data.frame(year = as.numeric(names(numR)), rec = numR)
  
  ## SSB
  ssbR <- g3_array_agg(ar = 
                         reports[grepl(paste0('dstart_', mat_stock, '__num$'), names(reports))][[1]] * 
                         reports[grepl(paste0('dstart_', mat_stock, '__wgt$'), names(reports))][[1]],
                       agg = sum,
                       margins = c('year'),
                       step = 1)
  ssbR <- data.frame(year = as.numeric(names(ssbR)), ssb = ssbR)
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
  fbar <- 4*(-log(1 - catN[paste0('age', f_ages),,]/N0[paste0('age', f_ages),,]))
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



