## Setup a random walk for recruitment...
random_actions <-
  list(
    g3l_random_walk(nll_name = 'ling_imm_randomwalk',
                    param_f = ~g3_param_table(
                      'ling.rec', 
                      expand.grid(cur_year = seq(start_year, end_year)),
                      random = TRUE),
                    sigma_f = ~g3_param('ling.rec_sigma', value = 1, optimise = FALSE),
                    log_f = TRUE,
                    period = 'year'),
    list())



random_actions2 <-
  list(
    g3l_random_dnorm(nll_name = 'ling_imm_randomwalk',
                     param_f = ~g3_param_table('ling.rec', 
                                               expand.grid(cur_year = seq(start_year, end_year)),
                                               random = TRUE),
                     mean_f = ~g3_param('ling.rec_mu', value = 1, optimise = FALSE),
                     sigma_f = ~g3_param('ling.rec_sigma', value = 1, optimise = FALSE),
                     log_f = TRUE),
    list())


random_actions3 <-
  list(
    g3l_random_dnorm(nll_name = 'ling_imm_randomwalk',
                    param_f = ~g3_param('ling.Linf', random = TRUE),
                    mean_f = ~g3_param('ling.Linf_mu', value = 1, optimise = FALSE),
                    sigma_f = ~g3_param('ling.Linf_sigma', value = 1, optimise = FALSE),
                    log_f = TRUE),
    list())
