## Setup a random walk for recruitment...
## First, setup variable to store previous years recruitment

## FIX SD TO VALUE, TRY 0.2

# setup_custom_action <- function(x, sigma){
#   
#   stock__prevrec <- gadget3:::g3_global_formula(init_val = 0.0)
#   
#   gadget3:::f_substitute(
#     ~if (cur_step_final) {
#       if(cur_year > start_year){
#         #if (stock__prevrec > 0){ 
#         nll <- nll + dnorm(x, stock__prevrec, sigma, 1)
#       }
#       stock__prevrec <- x 
#     },
#     list(x = x,
#     #     mu = mu,
#          sigma = sigma)
#   )
# }

custom_action <- g3l_random_walk(
                          paste0(imm_stock$name_parts[['species']], '_rec'),
                          param_f = g3_year_table(imm_stock,
                                            id = 'species',
                                            param_name = 'rec',
                                            bound_param = FALSE,
                                            random = TRUE),
                          sigma_f = g3_stock_param(imm_stock,
                                                  id = 'species',
                                                  param_name = 'rec.sigma',
                                                  bound_param = FALSE),
                          log_f = TRUE,
                          period = 'year')
                          #x0 = g3_year_table(imm_stock,
                          #                   id = 'species',
                          #                   param_name = 'rec',
                          #                   bound_param = FALSE,
                          #                   random = TRUE))
                      
# custom_action <- list("010:g3l_custom" =
#                         setup_custom_action(
#                           x = gadget3:::f_substitute(~log(x),
#                                                      list(x = g3_year_table(imm_stock,
#                                                                             id = 'species',
#                                                                             param_name = 'rec',
#                                                                             bound_param = FALSE,
#                                                                             random = TRUE))),
#                           mu = ~stock__prevrec, 
#                           sigma = gadget3:::f_substitute(~log(x), 
#                                                          list(x = g3_stock_param(imm_stock,
#                                                                                  id = 'species',
#                                                                                  param_name = 'rec.sd',
#                                                                                  bound_param = FALSE))),
#                           x0 = gadget3:::f_substitute(~log(x),
#                                                       list(x = g3_year_table(imm_stock,
#                                                                              id = 'species',
#                                                                              param_name = 'rec',
#                                                                              bound_param = FALSE,
#                                                                              random = TRUE))))
# )

# test_model <- g3_to_r(c(
#   mat_actions,
#   imm_actions,
#   fleet_actions,
#   likelihood_actions,
#   time_actions,
#   list(custom_action)))

