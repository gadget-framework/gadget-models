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
assess_fn <- function (
    # The start model year, as defined by g3a_time,
  start_year,
  # The current model year, as defined by g3a_time,
  cur_year,
  # Nested list of pred -> prey -> detail_prey_pred__cons
  cons,
  # List of prey -> dstart_prey__num
  abund,
  # List of prey -> dstart_prey__wgt
  meanwgt ) {
  years <- seq(start_year, cur_year)
  
  ## catch in numbers at age over all fleets
  cn <- g3_array_combine(lapply(names(cons), function (pred_n) lapply(names(cons[[pred_n]]), function (prey_n) {
    g3_array_agg(cons[[pred_n]][[prey_n]] / pmax(meanwgt[[prey_n]], 0.001), c("age", "year"), year = years)
  })))
  
  ## Abundance by age at step 1
  smb <- g3_array_combine(lapply(names(abund), function (prey_n) {
    g3_array_agg(abund[[prey_n]], c("age", "year"), year = years, step = 1)
  }))
  
  ## total abundance by maturity at step 1 by age
  immtotal <- g3_array_agg(abund$st_imm, c("age", "year"), year = years, step = 1)
  mattotal <- g3_array_agg(abund$st_mat, c("age", "year"), year = years, step = 1)
  
  ## Log outputs in globalenv
  assess_outputs[[as.character(cur_year)]] <<- list(
    cn = cn,
    smb = smb,
    immtotal = immtotal,
    mattotal = mattotal )
  
  ## Perform the assessment
  (sum(immtotal) + sum(mattotal)) / 1e10
}