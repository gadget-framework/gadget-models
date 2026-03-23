## Simply put the definition is, next quota period catch should be based on a fixed fishing mortality (F_{target}), 
## scaled linearly to zero if SSB_{t+1} < B_{trigger}. Sounds simple enough. Couple of snags though:

## There is a interim period between the end of the assessment period and the advisory period that needs to be taken 
## into account (e.g. interim year catch) in the projection.
## In age based assessment the catch is calculated according to the Baranov equation:
  
#  C_{t+1,a} = N_{ya} W_{ya}F_a/Z_{a,t+1} (1 – e^{-Z_{a,t+1}) where Z = F_a+M_a and
#  C_{t+1} = \sum_a C_{t+1,a}

## How do the equivalent in g3 when doing simulations (e.g. estimate Fmsy)? Do we project and rewind 
## or simply implement an age-based projection?
  
## SSB_{t+t} is based on a projection, so if we are to follow the old ICES rule we need to scale down 
## the F based on the projected SSB at the start of the advisory year.
## Gadget typically works in terms of harvest rates, although this mostly equivalent 
## (i.e. there is one-to-one non-linear mapping between the two) there are annoying differences 
## when setting the target as people expect working with Fs. How are we going to deal with that? 
  
## Going back to the original question, if we assume the HR notation the the catch according to the hockey quota function should be:

# C_{t} = HR_t * B_{ref,t} = HR_{target}*min(SSB_t/B_{trigger}, 1) * \sum_{alsft} N_{alst} * W_{alst} * S_{alsft}*p_{ft}

## All of these variables are fairly easy to attain directly from the population dynamics. 
## The catch would be distributed to the different fleets by p_{ft}, and each fleet would allocate the catch to different timestep.

## The SSB_t is defined either as the biomass of a particular stock at time t (i.e. the mature stock) 
## or if you are dealing with a single stock you will need a selection function to calculation:

# SSB_t = \sum_l S_lt*N_lt*W_lt

## For fixed F projections we could use the age based formulation from above (or a variant there of).

## Regarding the old linear and quotafleet functions from gadget2, 
## I would not miss them and I think the quota functionality supercedes them. 


# Assessment function, gets pulled into model by g3_formula
# Function is projects a stock forwards lag_steps, retrieves the biomass,
# compares with a trigger, scales a target F, then computes the catch for the 
# scaled target F based on Baranov
assess_fn <- function (
  cur_year,
  steps_per_year,
  cons,
  abund,
  meanwgt,
  #suits,
  M,
  btrigger,
  ftarget,
  trigger_stock = NULL,
  biomass_prop = 1L,
  lag_steps = 2L) {
  
  # Numbers-at-age per stock
  stock_N_at_age <-
    lapply(setNames(names(abund), names(abund)), function(prey_n){
      g3_array_agg(abund[[prey_n]], c("age"), year = cur_year, step = 1)
    })
  
  # Biomass at age per stock
  stock_B_at_age <- 
    lapply(setNames(names(abund), names(abund)), function(prey_n){
      g3_array_agg(abund[[prey_n]] * meanwgt[[prey_n]], c("age"), 
                   year = cur_year, step = 1) 
      })
  
  # Mean weight at age per stock
  stock_W_at_age <- list()
  for (stock in names(stock_N_at_age)){
    stock_W_at_age[[stock]] <- 
      stock_B_at_age[[stock]] / pmax(stock_N_at_age[[stock]], 1e-12)
  }
  
  # TO-DO: suitability at age
  suit_at_age <- 1

  ## Project forward stocks lag_steps  
  lag_fraction <- lag_steps / steps_per_year
  stock_N_imp <- list()
  stock_B_imp <- list()
  for (stock in names(stock_N_at_age)){
    stock_N_imp[[stock]] <- stock_N_at_age[[stock]] * exp(-M * lag_fraction)
    stock_B_imp[[stock]] <- stock_N_imp[[stock]] * stock_W_at_age[[stock]]
  }
  # Mean weight at age from projected stocks
  W_at_age_imp <- 
    g3_array_combine(stock_B_imp) / pmax(g3_array_combine(stock_N_imp), 1e-12)
    
  # Sort out the trigger biomass
  if (is.null(trigger_stock)) trigger_B <- g3_array_combine(stock_B_imp)
  else  trigger_B <- g3_array_combine(stock_B_imp[trigger_stock])
  
  # Dimensions of biomass prop
  if (length(biomass_prop) == 1L) {
    biomass_prop <- rep(biomass_prop, length(trigger_B))
  }
  stopifnot(length(biomass_prop) == length(trigger_B))
  
  # Corresponding biomass for the comparison
  # Using weight-at-age from last step of model, should it be the average of a recent period?
  biomass_impl <- sum(trigger_B * biomass_prop, na.rm = TRUE)
  # Scalar for sliding rule
  scaler <- min(biomass_impl / btrigger, 1.0)
  # Scaled F
  Fapplied <- ftarget * scaler
  # F at age
  F_at_age <- Fapplied * suit_at_age
  
  # Biomass quota from Baranov
  # Numbers aggregated across stocks 
  Z_at_age <- M + F_at_age
  catch_num <- 
    g3_array_combine(stock_N_imp) * F_at_age / pmax(Z_at_age, 1e-12) * (1 - exp(-Z_at_age))
  
  # Assuming weight-at-age does not change during the lag, or should we use mean
  # weight-at-age of last n years?
  catch_bio <- sum(catch_num * W_at_age_imp, na.rm = TRUE)
  
  # TODO: fix this output
  assess_outputs[[as.character(cur_year)]] <<- list(
    stock_N_at_age = stock_N_at_age,
    stock_B_at_age = stock_B_at_age,
    stock_W_at_age = stock_W_at_age,
    stock_N_imp = stock_N_imp,
    stock_B_imp = stock_B_imp,
    trigger_B = trigger_B,
    biomass_prop = biomass_prop,
    biomass_impl = biomass_impl,
    scaler = scaler,
    Fapplied = Fapplied,
    F_at_age = F_at_age,
    catch_num = catch_num,
    catch_bio = catch_bio
  )
  
  catch_bio
}