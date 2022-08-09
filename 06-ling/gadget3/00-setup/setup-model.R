## -----------------------------------------------------------------------------
##
## SETUP MODEL
##
## -----------------------------------------------------------------------------

##### Configure stocks #########################################################

## stocks
imm_stock <-
  g3_stock(c(species = 'ling', 'imm'), lengthgroups = seq(20, 160, 4)) %>%
  g3s_livesonareas(areas[c('1')]) %>%
  g3s_age(minage = 3, maxage = 10)

mat_stock <-
  g3_stock(c(species = 'ling', 'mat'), lengthgroups = seq(20, 160, 4)) %>%
  g3s_livesonareas(areas[c('1')]) %>%
  g3s_age(minage = 5, maxage = 15)

## List of stocks
stocks <- list(imm_stock, mat_stock)

# # -----------------------------
# # Define bounded parameters
# # -----------------------------
# 
# ## MODEL PARAMETERS
# # List containing the parameter name, its lower and upper bounds.
# # The function "g3_stock_param" is a wrapper for g3_param that inserts the species/stock name into the parameter reference
# # if lower and upper are NULL, then the parameter will be unbounded e.g. ~g3_param("ling.k")
# # if the lower and upper parameters are integers a reference to a bounded parameter will be created e.g. ~bounded(g3_param(ling.k), lower, upper)
# # "g3_stock_param_table" is an equivalent function that creates a reference to a table of parameters e.g. g3_param_table(ling.init, minage:maxage)
# 
# 
# model_params <- list(
#   
#   ## Initial conditions:
#   'walpha' = list(lower = NULL, upper = NULL),
#   'wbeta' = list(lower = NULL, upper = NULL),
#   'init.F' = list(lower = 0.2, upper = 0.8),
#   'init.sd' = list(lower = NULL, upper = NULL),
#   'scalar' = list(lower = 1, upper = 100),      ## Scalar for initial abundance and recruitment (all stocks)
#   'init' = list(lower = 0.001, upper = 300),
#   'renew' = list(lower = 0.001, upper = 300),
#   
#   ## Renewal:
#   'rec.sd' = list(lower = 1, upper = 20),
#   'recl' = list(lower = 1, upper = 20),
#   ## Growth:
#   'Linf' = list(lower = 150, upper = 200),          
#   'K' = list(lower = 40, upper = 120),             
#   'bbin' = list(lower = 1e-08, upper = 100),          
#   
#   ## Maturity:
#   'mat1' = list(lower = NULL, upper = NULL),
#   'mat2' = list(lower = 20, upper = 120),                 
#   'mat_initial_alpha' = list(lower = 1e-08, upper = 100), 
#   'mat_initial_a50' = list(lower = min(gadget3:::stock_definition(imm_stock, 'minage'),
#                                        gadget3:::stock_definition(mat_stock, 'minage')),
#                            upper = max(gadget3:::stock_definition(imm_stock, 'maxage'),
#                                        gadget3:::stock_definition(mat_stock, 'maxage'))),
#   
#   ## Mortality
#   'prop_mat0' = list(lower = 0.1, upper = 0.9),
#   'B0' = list(lower = NULL, upper = NULL),
#   'M' = list(lower = NULL, upper = NULL)
#   
# )


## Maximum number of length groups a stock can group within a time step (maxlengthgroupgrowth)
mlgg <- 10

## setup stock actions
imm_actions <- model_actions(imm = imm_stock,
                             mat = mat_stock,
                             mature = FALSE,
                             comp_id = 'species',
                             init_mode = setup_options$initial_abund_mode,
                             bound_param = setup_options$bound_params)

## Mature stock actions
mat_actions <- model_actions(imm = imm_stock,
                             mat = mat_stock,
                             mature = TRUE,
                             comp_id = 'species',
                             init_mode = setup_options$initial_abund_mode,
                             bound_param = setup_options$param_opt_mode)

## Combine stock actions
stock_actions <- c(imm_actions, mat_actions)

