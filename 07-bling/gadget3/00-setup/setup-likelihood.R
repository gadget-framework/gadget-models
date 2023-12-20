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
      'ldist_comm',
      obs_data = ldist.comm[[1]],
      fleets = list(comm),
      stocks = stocks,
      function_f = g3l_distribution_sumofsquares(),
      nll_breakdown = nll_breakdown,
      report = lik_report)
    
  }else{
    
    c(
      g3l_catchdistribution(
      'ldist_lln',
      obs_data = ldist.lln[[1]],
      fleets = list(lln),
      stocks = stocks,
      function_f = g3l_distribution_sumofsquares(),
      nll_breakdown = nll_breakdown,
      report = lik_report),
    
    g3l_catchdistribution(
      'ldist_bmt',
      obs_data = ldist.bmt[[1]] %>%
        filter(!(year == 2020 & step == 2)),
      fleets = list(bmt),
      stocks = stocks,
      function_f = g3l_distribution_sumofsquares(),
      nll_breakdown = nll_breakdown,
      report = lik_report)
    )
  },
  
  g3l_catchdistribution(
    'ldist_aut',
    obs_data = ldist.aut[[1]] %>% filter(year > 1999, year != 2011),
    fleets = list(aut),
    stocks = stocks,
    function_f = g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),
  
  if (!single_stock_model){
    
    g3l_catchdistribution(
      'matp_aut',
      obs_data = (matp.aut[[1]] %>% 
                    filter(year > 1999, year != 2011) %>% 
                    rename(stock = maturity_stage) %>%
                    mutate(stock = recode(as.factor(stock), blingimm = 'bling_imm', blingmat = 'bling_mat'))),
      fleets = list(aut),
      stocks = stocks,
      function_f = g3l_distribution_sumofsquares(),
      nll_breakdown = nll_breakdown,
      report = lik_report) 
  },
  
  g3l_abundancedistribution(
    'si_aut_1',
    obs_data = (aut.SI[[1]]$len20 %>% 
                  filter(year > 1999, year != 2011) %>% 
                  filter(year < (max(defaults$year) - peel))),
    fleets = list(),
    stocks = stocks,
    function_f = g3l_distribution_surveyindices_log(beta = 1), 
    nll_breakdown = nll_breakdown,
    report = lik_report),
  
  g3l_abundancedistribution(
    'si_aut_2a',
    obs_data = (aut.SI[[1]]$len52 %>% 
                  filter(year > 1999, year != 2011) %>% 
                  filter(year < (max(defaults$year) - peel))),
    fleets = list(),
    stocks = stocks,
    function_f = g3l_distribution_surveyindices_log(beta = 1), 
    nll_breakdown = nll_breakdown,
    report = lik_report),
  
  g3l_abundancedistribution(
    'si_aut_2b',
    obs_data = (aut.SI[[1]]$len60 %>% 
                  filter(year > 1999, year != 2011) %>% 
                  filter(year < (max(defaults$year) - peel))),
    fleets = list(),
    stocks = stocks,
    function_f = g3l_distribution_surveyindices_log(beta = 1),
    nll_breakdown = nll_breakdown,
    report = lik_report),
  
  g3l_abundancedistribution(
    'si_aut_3a',
    obs_data = (aut.SI[[1]]$len72 %>% 
                  filter(year > 1999, year != 2011) %>% 
                  filter(year < (max(defaults$year) - peel))),
    fleets = list(),
    stocks = stocks,
    function_f = g3l_distribution_surveyindices_log(beta = 1),
    nll_breakdown = nll_breakdown,
    report = lik_report),
  
  g3l_abundancedistribution(
    'si_aut_3b',
    obs_data = (aut.SI[[1]]$len80 %>% 
                  filter(year > 1999, year != 2011) %>% 
                  filter(year < (max(defaults$year) - peel))),
    fleets = list(),
    stocks = stocks,
    function_f = g3l_distribution_surveyindices_log(beta = 1),
    nll_breakdown = nll_breakdown,
    report = lik_report),
  
  g3l_abundancedistribution(
    'si_aut_3c',
    obs_data = (aut.SI[[1]]$len92 %>% 
                  filter(year > 1999, year != 2011) %>% 
                  filter(year < (max(defaults$year) - peel))),
    fleets = list(),
    stocks = stocks,
    function_f = g3l_distribution_surveyindices_log(beta = 1),
    nll_breakdown = nll_breakdown,
    report = lik_report),
  
  g3l_abundancedistribution(
    'si_aut_3d',
    obs_data = (aut.SI[[1]]$len100 %>% 
                  filter(year > 1999, year != 2011) %>% 
                  filter(year < (max(defaults$year) - peel))),
    fleets = list(),
    stocks = stocks,
    function_f = g3l_distribution_surveyindices_log(beta = 1),
    nll_breakdown = nll_breakdown,
    report = lik_report),
  
  list()
  
)
