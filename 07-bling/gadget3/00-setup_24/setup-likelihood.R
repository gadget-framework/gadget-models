## -------------------------------------------------------------------------------
##
## Runner to set up likelihoods
##
## -------------------------------------------------------------------------------

nll_breakdown <- TRUE  # Turn to TRUE to get per-step nll
lik_report <- TRUE

likelihood_actions <- list(
  g3l_understocking(stocks, nll_breakdown = nll_breakdown, weight = 100),
  
  if (single_fleet){
    
    g3l_catchdistribution(
      nll_name = 'ldist_comm',
      obs_data = ldist.comm[[boot_repl]] |> filter(year >= min(defaults$year)),
      fleets = list(comm),
      stocks = stocks,
      function_f = g3l_distribution_sumofsquares(),
      nll_breakdown = nll_breakdown,
      report = lik_report)
    
  }else{
    
    c(
      g3l_catchdistribution(
      nll_name = 'ldist_lln',
      obs_data = ldist.lln[[boot_repl]] |> filter(year >= min(defaults$year)),
      fleets = list(lln),
      stocks = stocks,
      function_f = g3l_distribution_sumofsquares(),
      nll_breakdown = nll_breakdown,
      report = lik_report),
    
    g3l_catchdistribution(
      nll_name = 'ldist_bmt',
      obs_data = ldist.bmt[[boot_repl]] |>
        filter(!(year == 2020 & step == 2)) |> 
        filter(year >= min(defaults$year)),
      fleets = list(bmt),
      stocks = stocks,
      function_f = g3l_distribution_sumofsquares(),
      nll_breakdown = nll_breakdown,
      report = lik_report),
    
    if (bmt_age){
      g3l_catchdistribution(
        nll_name = 'aldist_bmt',
        (aldist.bmt[[boot_repl]]),
        fleets = list(bmt),
        stocks = stocks,
        function_f = g3l_distribution_sumofsquares(over = {if (bmt_age_stratified) c('area', 'length') else 'area' }),
        nll_breakdown = nll_breakdown,
        report = lik_report)
    }else NULL
    )
  },
  
  g3l_catchdistribution(
    nll_name = 'ldist_aut',
    obs_data = ldist.aut[[boot_repl]] |> dplyr::filter(year > 2000, year != 2011),
    fleets = list(aut),
    stocks = stocks,
    function_f = g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),
  
  if (!single_stock_model){
    
    g3l_catchdistribution(
      nll_name = 'matp_aut',
      (matp.aut[[boot_repl]] |> 
         dplyr::filter(year > 2000, year != 2011) |> 
         dplyr::rename(stock = maturity_stage) |>
         dplyr::mutate(stock = dplyr::recode(as.factor(stock), blingimm = 'bli_imm', blingmat = 'bli_mat'))),
      fleets = list(aut),
      stocks = stocks,
      function_f = g3l_distribution_sumofsquares(),
      nll_breakdown = nll_breakdown,
      report = lik_report) 
  })

## Hack to fix likelihood names for plotting, should be done in plot_si
fix_name_length <- function(x) paste0('len',paste(rep('0',6-nchar(x)),collapse=''),gsub('len','',x))

si_lik_actions <- list()
for (i in names(aut.SI[[boot_repl]])){
  ## SLope
  if (i %in% paste0('len', si_len_slopes)){
    sibeta <- NULL
  }else{
    sibeta <- 1
  }
  ## Include selection pattern for SIs
  if (selection_si_lik){
    tmp <- g3l_catchdistribution(
      nll_name = paste0('si_', fix_name_length(i), '_AUT'),
      obs_data = aut.SI[[boot_repl]][[i]] |>
        dplyr::filter(year > 2000, year != 2011, 
                      year <= max(defaults$year) - peel),
      fleets = list(aut),
      stocks = stocks,
      function_f = g3l_distribution_surveyindices_log(beta = sibeta),
      nll_breakdown = nll_breakdown,
      report = lik_report
    )
  }else{
    tmp <- g3l_abundancedistribution(
      nll_name = paste0('si_', fix_name_length(i), '_AUT'),
      obs_data = aut.SI[[boot_repl]][[i]] |> 
        dplyr::filter(year > 2000, year != 2011, 
                      year <= max(defaults$year) - peel),
      fleets = list(),
      stocks = stocks,
      function_f = g3l_distribution_surveyindices_log(beta = sibeta),
      nll_breakdown = nll_breakdown,
      report = lik_report
    )
  } 
  si_lik_actions <- c(si_lik_actions, tmp)
}
  
likelihood_actions <- c(likelihood_actions, list(si_lik_actions), list())
rm(si_lik_actions, tmp)

