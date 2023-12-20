## -----------------------------------------------------------------------------
##
## Runner to set up parameters and actions for a single stock model using g3 defaults as far as possible
##
## -----------------------------------------------------------------------------

## Some parameters are used in multiple actions and their configurations may depend
#  upon options defined in the setup.R script, therefore it is easiest to define them here
#  so they can be implemented across actions in a consistent manner

## Growth coefficient - defining it here as we have an option to make this a time varying parameter in the setup
vonb_K <- gadget3::g3_parameterized(name = 'K', by_year = timevarying_K, by_stock = TRUE)

## There are various ways to parameterize the initial conditions in the single stock model
#  we consider two: (1) a single scalar that decays with age according to natural mortality and initial fishing mortality
#                   (2) a single scalar that multiplies age-specific numbers that decay with natural and initial fishing mortality

if (!init_abund_by_age){
  initabun <- gadget3::g3a_renewal_initabund(init = 1L)
}else{
  initabun <- gadget3::g3a_renewal_initabund()
}

## -----------------------------------------------------------------------------
## Setup model actions
## -----------------------------------------------------------------------------

stock_actions <- 
  list(
    
    ## INITIAL CONDITIONS
    gadget3::g3a_initialconditions_normalcv(stock = single_stock,
                                                 factor_f = initabun,
                                                 mean_f = gadget3::g3a_renewal_vonb_t0(K = vonb_K)  
                                                 ),
    
    ## NATURAL MORTALITY
    gadget3::g3a_naturalmortality(single_stock, gadget3::g3a_naturalmortality_exp()),
    
    ## AGEING
    gadget3::g3a_age(single_stock),
    
    ## GROWTH
    gadget3::g3a_growmature(single_stock,
                            impl_f = gadget3::g3a_grow_impl_bbinom(
                              delta_len_f = gadget3::g3a_grow_lengthvbsimple(kappa_f = vonb_K),
                              delta_wgt_f = gadget3::g3a_grow_weightsimple(),
                              beta_f = gadget3::g3_parameterized(name = 'bbin', 
                                                                 by_stock = TRUE, 
                                                                 exponentiate = exponentiate_bbin),
                              maxlengthgroupgrowth = maxlengthgroupgrowth
                              )
                            ),
    
    ## RENEWAL
    gadget3::g3a_renewal_normalparam(single_stock,
                                     mean_f = gadget3::g3a_renewal_vonb_t0(K = vonb_K)),
    list()
  )

