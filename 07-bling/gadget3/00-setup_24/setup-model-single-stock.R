## -----------------------------------------------------------------------------
##
## Setup stock actions for a single stock
##
## -----------------------------------------------------------------------------

stock_actions <- 
  list(
    ## Initial conditions
    g3a_initialconditions_normalcv(single_stock,
                                   factor_f = 
                                     g3a_renewal_initabund(scalar = g3_parameterized(ifelse(init_rec_scalar, 'init.rec.scalar', 'init.scalar'),
                                                                                     value = 1, 
                                                                                     by_stock = TRUE),
                                                           init = g3_parameterized('init', 
                                                                                   by_stock = TRUE, 
                                                                                   by_age = initabun_by_age, 
                                                                                   value = 1),
                                                           recage = g3_stock_def(single_stock, 'minage')),
                                   mean_f = 
                                     g3a_renewal_vonb_t0(K = g3_parameterized('K', 
                                                                              by_year = timevarying_K, 
                                                                              by_stock = TRUE))),
    ## Natural mortality
    gadget3::g3a_naturalmortality(single_stock, gadget3::g3a_naturalmortality_exp()),
    ## Ageing
    gadget3::g3a_age(single_stock),
    ## Growth 
    gadget3::g3a_growmature(single_stock,
                            ## Growth
                            impl_f = gadget3::g3a_grow_impl_bbinom(
                              delta_len_f = gadget3::g3a_grow_lengthvbsimple(
                                kappa_f = g3_parameterized('K', by_year = timevarying_K, by_stock = TRUE)
                                ),
                              delta_wgt_f = gadget3::g3a_grow_weightsimple(),
                              beta_f = gadget3::g3_parameterized('bbin', by_stock = TRUE, exponentiate = exponentiate_bbin),
                              maxlengthgroupgrowth = maxlengthgroupgrowth
                              )
                            ),
    # Renewal
    gadget3::g3a_renewal_normalparam(single_stock,
                                     factor_f = g3_parameterized('rec',
                                                                 by_stock = TRUE,
                                                                 by_year = TRUE,
                                                                 scale = g3_parameterized(
                                                                   name = ifelse(init_rec_scalar, 'init.rec.scalar', 'rec.scalar'),
                                                                   value = 1,
                                                                   by_stock = TRUE),
                                                                 exponentiate = exponentiate_recruitment, 
                                                                 ifmissing = NaN),
                                     mean_f = g3a_renewal_vonb_t0(K = g3_parameterized('K', 
                                                                                       by_year = timevarying_K, 
                                                                                       by_stock = TRUE))),
    
    list()
    
  )

