## -----------------------------------------------------------------------------
##
## Runner to set up parameters and actions for a single stock model using g3 defaults as far as possible
##
## -----------------------------------------------------------------------------

## Function to set up an maturity ogive (used for initial conditions)
g3a_initial_ageprop <- function(alpha = gadget3::g3_parameterized('mat_initial_alpha', by_stock = by_stock), 
                                a50 = gadget3::g3_parameterized('mat_initial_a50', by_stock = by_stock),
                                by_stock = FALSE){  
  gadget3:::f_substitute(~bounded(-1 * alpha * (age - a50), 0, 1),
                         list(alpha = alpha, 
                              a50 = a50)
  )
}

## Some parameters are used in multiple actions and their configurations may depend
#  upon options defined in the setup.R script, therefore it is easiest to define them here
#  so they can be implemented across actions in a consistent manner

## Growth coefficient - defining it here as we have an option to make this a time varying parameter in the setup
vonb_K <- gadget3::g3_parameterized(name = 'K', by_year = timevarying_K, by_stock = stocks)

## There are various ways to parameterize the initial conditions in the two stock model
#  we consider two: (1) a single scalar that decays with age according to natural mortality and initial fishing mortality
#                   (2) a single scalar that multiplies age-specific numbers that decay with natural and initial fishing mortality, numbers at age are split between stocks via an ogive
#                   (3) a single scalar that multiplies age- and stock-specific numbers that decay with natural and initial fishing mortality
# (1)
if (!init_abund_by_age){
  initabun <- gadget3::g3a_renewal_initabund(init = 1L, 
                                             M = g3_parameterized('M', by_age = TRUE, by_stock = stocks),
                                             proportion_f = g3a_initial_ageprop(by_stock = stocks),
                                             by_stock = stocks)  
}else{
  # (2)
  if (init_abund_by_age && !init_abund_by_stock){
    initabun <- gadget3::g3a_renewal_initabund(by_stock = stocks,
                                               M = g3_parameterized('M', by_age = TRUE, by_stock = stocks),
                                               proportion_f = g3a_initial_ageprop(by_stock = stocks))  
  }else{
    # (3)
    initabun <- gadget3::g3a_renewal_initabund(M = g3_parameterized('M', by_age = TRUE, by_stock = stocks))
  }
}


## -----------------------------------------------------------------------------
## Setup model actions
## -----------------------------------------------------------------------------

## IMMATURE ACTIONS

imm_actions <- 
  list(
    
    ## INITIAL CONDITIONS
    g3a_initialconditions_normalcv(imm_stock,
                                   factor_f = initabun,
                                   mean_f = gadget3::g3a_renewal_vonb_t0(K = vonb_K, by_stock = stocks),  # by_stock = stocks ensures Linf and t0 params are across stocks,
                                   by_stock = stocks),
    
    ## NATURAL MORTALITY
    gadget3::g3a_naturalmortality(imm_stock, gadget3::g3a_naturalmortality_exp(by_stock = stocks)),

    ## AGEING
    gadget3::g3a_age(imm_stock, output_stocks = list(mat_stock)),

    ## GROWTH AND MATURITY
    gadget3::g3a_growmature(imm_stock,
                            ## Growth
                            impl_f = gadget3::g3a_grow_impl_bbinom(
                              delta_len_f = gadget3::g3a_grow_lengthvbsimple(kappa_f = vonb_K, by_stock = stocks),
                              delta_wgt_f = gadget3::g3a_grow_weightsimple(),
                              beta_f = gadget3::g3_parameterized(name = 'bbin', 
                                                                 by_stock = stocks, 
                                                                 exponentiate = exponentiate_bbin),
                              maxlengthgroupgrowth = maxlengthgroupgrowth
                            ),
                            ## Maturity
                            maturity_f = gadget3::g3a_mature_continuous(
                              alpha = gadget3::g3_parameterized('mat_alpha', by_stock = TRUE),
                              l50 = gadget3::g3_parameterized('mat_l50', by_stock = TRUE)
                            ),
                            output_stocks = list(mat_stock),
                            transition_f = ~TRUE
    ),

    # RENEWAL
    gadget3::g3a_renewal_normalparam(imm_stock,
                                     mean_f = gadget3::g3a_renewal_vonb_t0(K = vonb_K,
                                                                           by_stock = stocks)),
    
    list()
    
  )

## MATURE ACTIONS

mat_actions <- 
  list(
    
    ## INITIAL CONDITIONS
    g3a_initialconditions_normalcv(mat_stock,
                                      factor_f = initabun,
                                      mean_f = gadget3::g3a_renewal_vonb_t0(K = vonb_K, by_stock = stocks),  # by_stock = stocks ensures Linf and t0 params are across stocks,
                                      by_stock = stocks),
    
    ## NATURAL MORTALITY
    gadget3::g3a_naturalmortality(mat_stock, gadget3::g3a_naturalmortality_exp(by_stock = stocks)),

    ## AGEING
    gadget3::g3a_age(mat_stock),

    # GROWTH
    gadget3::g3a_growmature(mat_stock,
                            impl_f = gadget3::g3a_grow_impl_bbinom(
                              delta_len_f =
                                gadget3::g3a_grow_lengthvbsimple(kappa_f = vonb_K, by_stock = stocks),
                              delta_wgt_f = gadget3::g3a_grow_weightsimple(),
                              beta_f = gadget3::g3_parameterized(name = 'bbin', 
                                                                 by_stock = stocks, 
                                                                 exponentiate = exponentiate_bbin),
                              maxlengthgroupgrowth = maxlengthgroupgrowth)
    ),
    
    list()
    
  )

## Compile stock actions
stock_actions <- c(imm_actions, mat_actions, list())

