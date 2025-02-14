## -----------------------------------------------------------------------------
##
## Runner to set up stocks parameters and actions using g3 defaults as far as possible
##
## -----------------------------------------------------------------------------

## Setup formulas that depend on setup options

## Two internal functions (from gadgetutils) that should not be made visible
# (1) ogive for splitting ages between stocks
g3a_initial_ageprop <- function(alpha = g3_parameterized('mat_initial_alpha', by_stock = by_stock), 
                                a50 = g3_parameterized('mat_initial_a50', by_stock = by_stock),
                                by_stock = 'species'){  
  gadget3:::f_substitute(~bounded(-1 * alpha * (age - a50), 0, 1),
                         list(alpha = alpha, 
                              a50 = a50)
  )
}

initabun <- g3a_renewal_initabund(scalar = g3_parameterized(ifelse(init_rec_scalar, 'init.rec.scalar', 'init.scalar'),
                                                            value = 1, 
                                                            by_stock = {if (initabun_by_stock) TRUE else stocks}),
                                  init = g3_parameterized('init', 
                                                          by_stock = {if (initabun_by_stock) TRUE else stocks}, 
                                                          by_age = initabun_by_age, 
                                                          value = 1),
                                  M = g3_parameterized('M', by_stock = stocks, by_age = TRUE),
                                  init_F = g3_parameterized('init_F', 
                                                            by_stock = stocks,
                                                            value = {if (initabun_by_age) 0 else 0.1},
                                                            optimise = {if (initabun_by_age) FALSE else TRUE}),
                                  proportion_f = {if (initabun_by_stock) ~1 else (g3a_initial_ageprop())},
                                  recage = g3_stock_def(imm_stock, 'minage'))

## Some common parameters
common_pars <- 
  list(
    mean_len = g3a_renewal_vonb_t0(K = g3_parameterized('K', by_year = timevarying_K, by_stock = stocks), by_stock = stocks),
    var_len = g3_parameterized('lencv', by_stock = stocks, by_age = TRUE),
    mean_rec_len = gadget3:::f_substitute(quote(Linf * (1 - exp(-1 * K * (age - t0)))),
                                          list(Linf = g3_parameterized("Linf", by_stock = stocks), 
                                               K = g3_parameterized('K', by_stock = stocks, by_year = timevarying_K), 
                                               t0 = g3_parameterized("t0", by_stock = stocks),
                                               age = g3_stock_def(imm_stock, 'minage')))
  )

## If using recl instead of t0
if (mean_len_recl){
  common_pars$mean_len <- g3a_renewal_vonb_recl(K = g3_parameterized('K', by_year = timevarying_K, by_stock = stocks),
                                                recage = g3_stock_def(imm_stock, 'minage'),
                                                by_stock = stocks)
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
                                   mean_f = common_pars$mean_len,
                                   cv_f = common_pars$var_len
                                   ),
    ## NATURAL MORTALITY
    g3a_naturalmortality(imm_stock, g3a_naturalmortality_exp(by_stock = stocks)),
    
    ## AGEING
    g3a_age(imm_stock, output_stocks = list(mat_stock)),

    ## GROWTH AND MATURITY
    g3a_growmature(imm_stock,
                   ## Growth
                   impl_f = g3a_grow_impl_bbinom(
                     delta_len_f = 
                       g3a_grow_lengthvbsimple(
                         kappa_f = g3_parameterized('K', by_stock = stocks, by_year = timevarying_K),
                         by_stock = stocks
                         ),
                     beta_f = g3_parameterized('bbin', by_stock = stocks, exponentiate = exponentiate_bbin),
                     maxlengthgroupgrowth = maxlengthgroupgrowth
                     ),
                   ## Maturity
                   maturity_f = g3a_mature_continuous(),
                   output_stocks = list(mat_stock),
                   transition_f = ~TRUE
                   ),
    # RENEWAL
    {if (recruitment_free_pars)
      g3a_renewal_normalcv(imm_stock,
                           factor_f = g3_parameterized('rec',
                                                       by_stock = TRUE,
                                                       by_year = TRUE,
                                                       scale = g3_parameterized(
                                                         name = ifelse(init_rec_scalar, 'init.rec.scalar', 'rec.scalar'),
                                                         value = 1,
                                                         by_stock = TRUE),
                                                       exponentiate = exponentiate_recruitment,
                                                       ifmissing = NaN),
                           mean_f = common_pars$mean_len,
                           cv_f = g3_parameterized('recl.cv', by_stock = stocks),
                           run_projection = FALSE)
    },
    
    list()
    
  )

## MATURE ACTIONS

mat_actions <- 
  list(
    ## INITIAL CONDITIONS
    g3a_initialconditions_normalcv(mat_stock,
                                   factor_f = initabun,
                                   mean_f = common_pars$mean_len,
                                   cv_f = common_pars$var_len
                                   ),
    ## NATURAL MORTALITY
    g3a_naturalmortality(mat_stock, g3a_naturalmortality_exp(by_stock = stocks)),
    ## AGEING
    g3a_age(mat_stock),
    # GROWTH
    g3a_growmature(mat_stock,
                   impl_f = g3a_grow_impl_bbinom(
                     delta_len_f = 
                       g3a_grow_lengthvbsimple(
                         kappa_f = g3_parameterized('K', by_stock = stocks, by_year = timevarying_K),
                         by_stock = stocks
                       ),
                     beta_f = g3_parameterized('bbin', by_stock = stocks, exponentiate = exponentiate_bbin),
                     maxlengthgroupgrowth = maxlengthgroupgrowth
                   )
    ),
    {
      if (recruitment_free_pars){
        ## SPAWNING - for projections
        g3a_spawn(
          proportion_f = g3_suitability_blueling(),
          stock = mat_stock,
          recruitment_f =
            g3a_spawn_recruitment_hockeystick(
              r0 = gadget3:::f_substitute(~scale * g3_param_table('project_rec',
                                                                  expand.grid(cur_year = seq(end_year - (minage) + 1, 
                                                                                             end_year + py)), 
                                                                  value = 0,
                                                                  optimise = FALSE,
                                                                  ifmissing = 0),
                                          list(py = defaults$project_years,
                                               minage = gadget3::g3_stock_def(imm_stock, 'minage'),
                                               scale = 1e4)),
              blim = g3_parameterized('spawn_blim', value = 1, optimise = FALSE)
            ),
          output_stocks = list(dummy_stock),
          mean_f = common_pars$mean_rec_len,
          stddev_f = g3_formula(a * b,
                                a = common_pars$mean_rec_len,
                                b = g3_parameterized('recl.cv', by_stock = stocks)),
          run_f = g3_formula(cur_year >= (end_year - minage + 1) && cur_step == 1,
                             list(minage = g3_stock_def(imm_stock, 'minage')))
        )
      }else{
        ## SPAWNING - for projections
        c(
          g3a_spawn(stock = mat_stock,
                    recruitment_f =
                      g3a_spawn_recruitment_hockeystick(
                        r0 = g3_param_project(
                          param_name = 'rec',
                          project_fs = g3_param_project_logar1(),
                          scale = g3_parameterized('rec.scalar', scale = 1e4, by_stock = stocks),
                          random = FALSE,
                          by_stock = stocks,
                          by_step = FALSE
                          ),
                        blim = g3_parameterized('spawn_blim', value = 1, optimise = FALSE)
                        ),
                    output_stocks = list(dummy_stock),
                    mean_f = g3_parameterized('recl', by_stock = stocks),
                    stddev_f = g3_formula(a * b,
                                          a = g3_parameterized('recl', by_stock = stocks),
                                          b = g3_parameterized('recl.cv', by_stock = stocks)),
                    run_f = g3_formula(cur_step == 1 && cur_year > start_year)
                    ),
          ## NB: Dummy parameter so model will compile in TMB
          quote( nll <- nll + g3_param("x", value = 0, optimise = FALSE))
        )
      }
    },
    list()
    )

## Dummy stock actions
if (recruitment_free_pars){
  dummy_actions <- 
    list(
      g3a_age(dummy_stock,
              output_stocks = list(imm_stock),
              run_f = g3_formula(cur_year >= (end_year - minage + 2) && cur_step == 1),
              #run_f = ~cur_step == 1,
              run_at = 1,
              transition_at = 8)
    )
}else{
  dummy_actions <- 
    list(
      ## INITIAL CONDITIONS
      ## The dummy stocks initial conditions will mimic the minimum age class'
      ## of the immature stock, i.e., same scalar and length parameters.
      ## The init parameters will be stock specific and can really be thought of
      ## as recruitment for the immature stock in start_year + 1:3
      g3a_initialconditions_normalcv(dummy_stock,
                                     factor_f = 
                                       gadget3:::f_substitute(~scalar * init,
                                                              list(scalar = g3_parameterized(ifelse(init_rec_scalar, 'init.rec.scalar', 'init.scalar'),
                                                                                             value = 1, 
                                                                                             by_stock = {if (initabun_by_stock) TRUE else stocks}),
                                                                   init = g3_parameterized('init', 
                                                                                           by_stock = dummy_stock, 
                                                                                           by_age = TRUE, 
                                                                                           value = 1))
                                       ),
                                     mean_f = g3_parameterized('recl', by_stock = stocks),
                                     cv_f = g3_parameterized('recl.cv', by_stock = stocks)),
      g3a_age(dummy_stock,
              output_stocks = list(imm_stock),
              run_f = ~cur_step == 1 && cur_year > start_year,
              run_at = 1,
              transition_at = 8)
    )
}


## Compile stock actions
stock_actions <- c(imm_actions, mat_actions, dummy_actions, list())

