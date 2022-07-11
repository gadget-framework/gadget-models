stock_renewal <- function(stock, id = 'species', bound_param = TRUE){

  gadget3:::f_substitute(~scalar * exp(renew),
                         list(scalar = g3_stock_param(stock,
                                                      id,
                                                      'rec.scalar', 
                                                      bound_param),
                              renew = g3_year_table(stock,
                                                    id, 
                                                    'rec',
                                                    FALSE,
                                                    random = TRUE)))
  
}

model_actions <- function(imm, mat, mature = TRUE, 
                          comp_id = 'species', init_mode, bound_param){
  
  stock <- if(mature) mat else imm

  ## Stock specific variables
  if (mature) output_stock <- list() else output_stock <- list(mat)

  stock_actions <- list(
    ## INITIAL CONDITIONS
    g3a_initialconditions_normalparam(stock,
                                      # NB: area & age factor together (gadget2 just multiplied them)
                                      # initial abundance at age is 1e4 * q
                                      factor_f =
                                        gadgetutils::init_abund(imm, mat, comp_id, mature, init_mode, bound_param),
                                      mean_f = g3a_renewal_vonb(by_stock = comp_id),
                                      stddev_f = gadgetutils::init_sd(stock, comp_id, parametric = FALSE, bound_param = FALSE),
                                      alpha_f = g3_stock_param(stock, comp_id, 'walpha', bound_param),
                                      beta_f = g3_stock_param(stock, comp_id, 'wbeta', bound_param)),
    
    ## NATURAL MORALITY
    g3a_naturalmortality(stock, g3a_naturalmortality_exp({
      #bounded_table(stock, 'M', model_params, id = 'species')
      if (init_mode == 0){
        g3_stock_param(stock, comp_id, 'M', bound_param)
      }else{
        if (init_mode == 1){
          g3_stock_table(list(imm, mat), comp_id, 'M', bound_param)
          }else{
            #g3_stock_table(stock, 'full', 'M', bound_param)
            g3_stock_param(stock, 'full', 'M', bound_param)
          }
      }
    })),
    
    ## AGING
    g3a_age(stock, output_stocks = output_stock)
  )
    
  if (!mature){
    
    stock_actions <- c(stock_actions, list(
      
      ## RENEWAL
      g3a_renewal_normalparam(imm,
                              factor_f = stock_renewal(imm, id = comp_id, bound_param),
                              mean_f = g3a_renewal_vonb(by_stock = 'species'),
                              stddev_f = g3_stock_param(imm, comp_id, 'rec.sd', bound_param),
                              alpha_f = g3_stock_param(imm, comp_id, 'walpha', bound_param),
                              beta_f = g3_stock_param(imm, comp_id, 'wbeta', bound_param),
                              run_f = gadget3:::f_substitute(
                                ~cur_step == 1 && age == minage && cur_time > 0,
                                list(minage = gadget3:::g3_step(~stock_with(imm, imm__minage))))),
      
      ## GROWTH AND MATURATION
      g3a_growmature(imm,
                     impl_f = g3a_grow_impl_bbinom(
                       g3a_grow_lengthvbsimple(g3_stock_param(imm, comp_id, 'Linf', bound_param),
                                               {gadget3:::f_substitute(~(0.001 * K), 
                                                                       list(K = g3_stock_param(imm,
                                                                                               comp_id,
                                                                                               'K',
                                                                                               bound_param)))}),      
                       g3a_grow_weightsimple(g3_stock_param(imm, comp_id, 'walpha', bound_param),
                                             g3_stock_param(imm, comp_id, 'wbeta', bound_param)),   
                       beta_f = {gadget3:::f_substitute(~(10 * bbin), 
                                                        list(bbin = g3_stock_param(imm,
                                                                                   comp_id,
                                                                                   'bbin', 
                                                                                   bound_param)))},
                       maxlengthgroupgrowth = mlgg),
                     maturity_f = g3a_mature_continuous(
                       alpha = {gadget3:::f_substitute(~(0.001 * exp(mat1)), 
                                                       list(mat1 = g3_stock_param(imm,
                                                                                  comp_id,
                                                                                  'mat1', 
                                                                                  bound_param)))},
                       l50 = g3_stock_param(imm, comp_id, 'mat2', bound_param)
                     ),
                     output_stocks = list(mat),
                     transition_f = ~cur_time > 0),
      
      list()
      
    ))
  }
  else{
    
    stock_actions <- c(stock_actions, list(
      
      g3a_growmature(mat,
                     impl_f = g3a_grow_impl_bbinom(
                       g3a_grow_lengthvbsimple(g3_stock_param(mat, comp_id, 'Linf', bound_param),
                                               {gadget3:::f_substitute(~(0.001 * K), 
                                                                       list(K = g3_stock_param(mat,
                                                                                               comp_id,
                                                                                               'K',
                                                                                               bound_param)))}),       
                       g3a_grow_weightsimple(g3_stock_param(mat, comp_id, 'walpha', bound_param),
                                             g3_stock_param(mat, comp_id, 'wbeta', bound_param)),    
                       beta_f = {gadget3:::f_substitute(~(10 * bbin), 
                                                        list(bbin = g3_stock_param(mat,
                                                                                   comp_id,
                                                                                   'bbin', 
                                                                                   bound_param)))},
                       maxlengthgroupgrowth = mlgg)),
      list()
      
    ))
  }
  return(stock_actions)
}
