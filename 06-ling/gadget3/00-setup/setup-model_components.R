## -----------------------------------------------------------------------------
##
## Immature actions
## 
## -----------------------------------------------------------------------------

comp_id <- 'species'

initial_conditions_imm <- list(g3a_initialconditions_normalparam(imm_stock,
                                                                 factor_f = 
                                                                   init_abund(imm_stock,
                                                                              mat_stock,
                                                                              comp_id,
                                                                              mature = FALSE,
                                                                              init_mode = init_abund_mode),
                                                                 mean_f = g3a_renewal_vonb(by_stock = 'species'),
                                                                 by_age = TRUE,
                                                                 wgt_by_stock = 'species'))

initial_conditions_mat <- list(g3a_initialconditions_normalparam(mat_stock,
                                                                 factor_f = 
                                                                   init_abund(imm_stock,
                                                                              mat_stock,
                                                                              comp_id,
                                                                              mature = TRUE,
                                                                              init_mode = init_abund_mode),
                                                                 mean_f = g3a_renewal_vonb(by_stock = 'species'),
                                                                 by_age = TRUE,
                                                                 wgt_by_stock = 'species'))
                               

natural_mortality_imm <- list(g3a_naturalmortality(imm_stock, g3a_naturalmortality_exp()))
natural_mortality_mat <- list(g3a_naturalmortality(mat_stock, g3a_naturalmortality_exp()))


ageing_imm <- list(g3a_age(imm_stock, list(mat_stock)))
ageing_mat <- list(g3a_age(mat_stock, list()))

renewal_imm <- list(g3a_renewal_normalparam(imm_stock,
                                            factor_f = g3_parameterized('rec', 
                                                                        by_stock = list(imm_stock, mat_stock), 
                                                                        by_year = TRUE, 
                                                                        scale = g3_parameterized(name = 'rec.scalar',
                                                                                                 by_stock = list(imm_stock, mat_stock),
                                                                                                 exponentiate = FALSE,
                                                                                                 ),
                                                                        ifmissing = NaN),
                                            mean_f = g3a_renewal_vonb(by_stock = 'species'),
                                            by_stock = 'species',
                                            wgt_by_stock = 'species',
                                            run_f = gadget3:::f_substitute(~cur_step == 1 &&
                                                                             age == minage &&
                                                                             cur_time > 0 &&
                                                                             !cur_year_projection,
                                                                           list(minage = gadget3:::stock_definition(imm_stock, 'minage')))))
                                            



growmature_imm <- list(g3a_growmature(imm_stock,
                                      impl_f = g3a_grow_impl_bbinom(
                                        delta_len_f = g3a_grow_lengthvbsimple(by_stock = 'species'),
                                        delta_wgt_f = g3a_grow_weightsimple(by_stock = 'species'),
                                        beta_f = g3_parameterized('bbin', by_stock = 'species', scale = 10),
                                        maxlengthgroupgrowth = mlgg,
                                        by_stock = 'species'),
                                      maturity_f = g3a_mature_continuous(alpha = g3_parameterized('mat_alpha', by_stock = 'species', scale = 0.001),
                                                                         l50 = g3_parameterized('mat_l50', by_stock = 'species')),
                                      output_stocks = list(mat_stock),
                                      transition_f = ~cur_time > 0))

growmature_mat <- list(g3a_growmature(mat_stock,
                                      impl_f = g3a_grow_impl_bbinom(
                                        delta_len_f = g3a_grow_lengthvbsimple(by_stock = 'species'),
                                        delta_wgt_f = g3a_grow_weightsimple(by_stock = 'species'),
                                        beta_f = g3_parameterized('bbin', by_stock = 'species', scale = 10),
                                        maxlengthgroupgrowth = mlgg,
                                        by_stock = 'species')))

# spawning <- list(g3a_spawn(mat_stock,
#                            recruitment_f = g3a_spawn_recruitment_ricker(
#                              ~g3_param("ricker.mu"),
#                              ~g3_param("ricker.lambda")),
#                            #proportion_f = g3_suitability_exponentiall50(alpha = ~-g3_param("spawn.prop.alpha"), l50 =  ~g3_param("spawn.prop.l50")),
#                            #mortality_f = g3_suitability_straightline(alpha = ~g3_param("spawn.mort.alpha"), beta =  ~g3_param("spawn.mort.beta")),
#                            #weightloss_f = ~g3_param("spawn.weightloss"),
#                            output_stocks = list(imm_stock),
#                            mean_f = 50,
#                            stddev_f = 0.9,
#                            alpha_f = 1,
#                            beta_f = 1,
#                            run_f = ~cur_step==1))

spawning <- list(g3a_spawn(mat_stock,
                           recruitment_f = g3a_spawn_recruitment_simplessb(mu = g3_parameterized('spawn.mu', by_stock = 'species')),
                          #  recruitment_f = g3a_spawn_recruitment_hockeystick(r0 = 1e8,
                          #                                                   blim = g3_parameterized(name = 'blim',
                          #                                                                           by_stock = FALSE,
                          #                                                                           scale = 1,
                          #                                                                           value = 100)),

                           output_stocks = list(imm_stock),
                           mean_f = g3a_renewal_vonb(by_stock = 'species'),
                           stddev_f = g3_parameterized('rec.sd', by_stock = 'species'),
                           alpha_f = g3_parameterized('walpha', by_stock = 'species'),
                           beta_f = g3_parameterized('wbeta', by_stock = 'species'),
                           run_f = ~cur_step == 1 && cur_year_projection))

