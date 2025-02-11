library(mfdb)
library(gadget3)
library(gadgetutils)
library(gadgetplots)
library(tidyverse)
library(g3experiments)


base_dir <- 'benchmarks/WKBDEEP/gadget3'
vers <- 'models/26-baseline_IFgr_Linf0_K0_t00_mlgg4_reccv0_penrec1_sd0.4_agedata0_t0_ff'
outpath <- file.path(base_dir, vers, 'PROJ')
theme_set(theme_bw())

## Load the desired model, params and fit
load(file = file.path(base_dir, vers, 'WGTS', 'fit.Rdata'))

## Quantile function
quantile_df <- function(x, scale = 1, probs = c(0.05,0.25, 0.5, 0.75, 0.95)) {
  tibble(
    value = quantile(x/scale, probs, na.rm = TRUE),
    prob = probs
  )
}

## Variables
f_round <- 3

## -----------------------------------------------------------------------------
## Stock and fleets
## -----------------------------------------------------------------------------

## Median SSB for 2000:2005 (see TR)
## Blim, Bpa, Btrigger
blim <- 
  fit$res.by.year |> 
  filter(stock == 'bli_mat') |> 
  mutate(total.biomass = total.biomass/1e3) |> 
  pull(total.biomass) |> 
  min()

bpa  <- blim * exp(1.645 * 0.2)
btrigger <- bpa


## Load the data
load(file.path(outpath, 'results_msy_nobtrigger.Rdata'))
load(file.path(outpath, 'results_msy.Rdata'))
load(file.path(outpath, 'results_pre.Rdata'))

results_pre <- results_pre %>% separate(id, into = c('h', 'hr_target', 'trial', 'boot'), sep = '-') |> select(-h) 
results_msy_nobtrigger <- results_msy_nobtrigger %>% separate(id, into = c('h', 'hr_target', 'trial', 'boot'), sep = '-') |> select(-h) 
results_msy <- results_msy %>% separate(id, into = c('h', 'hr_target', 'trial', 'boot'), sep = '-') |> select(-h) 

## -----------------------------------------------------------------------------

## Precautionary reference points
res_pre <- 
  results_pre |> 
  filter(year > (max(year) -50)) %>% 
  group_by(hr_target) %>% 
  reframe(f = round(mean(fbar), digits = f_round),
          quantile_df(ssb, 1e3)) 
  
## Flim
Elim <- 
  res_pre %>% 
  filter(prob == 0.5, value > local(blim)) %>% 
  filter(hr_target == max(hr_target, na.rm = TRUE)) %>% 
  rename(hr_lim = hr_target, flim = f)
  
## -----------------------------------------------------------------------------

# results_msy_nobtrigger %>% 
#   filter(step == 1) %>% 
#   group_by(year, hr_target) %>% 
#   reframe(quantile_df(ssb, 1e3)) -> test
# 
# hrt <- 0.09
# test %>% 
#   filter(hr_target %in% hrt, prob == 0.5) %>% 
#   ggplot(aes(year, value)) + geom_line() + geom_ribbon(data = test %>% 
#                                                          filter(hr_target %in% hrt, prob %in% c(0.05,0.5,0.95)) %>% 
#                                                          pivot_wider(names_from = prob,values_from = value),
#                                                        aes(y=0.5,ymin = `0.05`, ymax = `0.95`),
#                                                        alpha = 0.5)


## MSY reference points

## Yield curve no btrigger
yield_dat <- 
  results_msy_nobtrigger |> 
  filter(year > (max(year) -50)) %>% 
  group_by(year, hr_target, boot, trial) %>% 
  summarise(y = sum(catch), 
            ssb = mean(ssb),
            f = mean(fbar)) %>% 
  group_by(hr_target) %>% 
  reframe(f = round(mean(f), digits = f_round),
          ssb = mean(ssb),
          quantile_df(y, 1e3)) 

## Median yield
Emsy <- yield_dat %>% filter(prob == 0.5) %>% filter(value == max(value))

Emsy_range <- yield_dat %>% 
  filter(prob == 0.5) %>% 
  filter(value > 0.95*max(value)) %>% 
  filter(hr_target == min(hr_target) | hr_target == max(hr_target))


ssb_dat <- 
  results_msy |> 
  filter(year > (max(year) -50)) %>% 
  group_by(year, hr_target, boot, trial) %>% 
  summarise(ssb = mean(ssb), 
            f = mean(fbar)) %>% 
  group_by(hr_target) %>% 
  reframe(f = round(mean(f), digits = f_round),
          quantile_df(ssb, 1e3)) 

Epa <- 
  ssb_dat %>% 
  filter(prob == 0.05) %>% 
  filter(value > blim) %>% 
  filter(hr_target == max(hr_target))


Pbref <- 
  results_msy |> 
  filter(year > (max(year) -50)) %>% 
  group_by(year, hr_target, boot, trial) %>% 
  summarise(ssb = mean(ssb), 
            f = mean(fbar)) %>% 
  group_by(hr_target) %>% 
  summarise(f = round(mean(f), digits = f_round),
            pbpa = mean(ssb < bpa * 1e3),
            pblim = mean(ssb < blim * 1e3))


# fit$res.by.year %>% 
#   left_join(fbar_func(fit))

fbar_func <- function(fit, age_range = 10:20){
  fit$stock.prey %>% 
    filter(age %in% age_range) %>% 
    group_by(year,age) %>% 
    summarise(c=sum(number_consumed), 
              n=sum(number[step==1])) %>% 
    mutate(f=-log(1-c/n)) %>% 
    group_by(year) %>% 
    summarise(fbar = mean(f))
}



proj_plot <- 
  cowplot::plot_grid(
    
    yield_dat %>% 
      filter(prob==0.5) %>% 
      ggplot(aes(f, value/1e3)) + 
      geom_ribbon(data = yield_dat %>%
                    mutate(value = value/1e3) %>% 
                    filter(prob %in% c(0.05,0.5,0.95)) %>% 
                    pivot_wider(names_from = prob,values_from = value),
                  aes(y=0.5,ymin = `0.05`, ymax = `0.95`),
                  alpha = 0.5) +
      geom_ribbon(data = yield_dat %>%
                    mutate(value = value/1e3) %>% 
                    filter(prob %in% c(0.25,0.5,0.75)) %>% 
                    pivot_wider(names_from = prob,values_from = value),
                  aes(y=0.5,ymin = `0.25`, ymax = `0.75`),
                  alpha = 0.5) +
      geom_line() + 
      geom_vline(xintercept = Emsy$f) +
      geom_vline(xintercept = Emsy_range$f, lty=2)+
      geom_hline(yintercept = Emsy$value/1e3) + 
      geom_vline(xintercept = Epa$f,col='red') +
      # geom_point(data=fit$res.by.year %>% 
      #              filter(stock == 'cdred_mat') %>% 
      #              select(value=catch,f=F),
      #            aes(y=value/1e3)) +
      labs(y="Catch ('000 tons)", x = 'F'),
    
    ssb_dat %>% 
      filter(prob==0.5) %>% 
      ggplot(aes(f, value)) + 
      geom_ribbon(data = ssb_dat %>%
                    mutate(value = value) %>% 
                    filter(prob %in% c(0.05,0.5,0.95)) %>% 
                    pivot_wider(names_from = prob,values_from = value),
                  aes(y=0.5,ymin = `0.05`, ymax = `0.95`),
                  alpha = 0.5) +
      geom_ribbon(data = ssb_dat %>%
                    mutate(value = value) %>%  
                    filter(prob %in% c(0.25,0.5,0.75)) %>% 
                    pivot_wider(names_from = prob,values_from = value),
                  aes(y=0.5,ymin = `0.25`, ymax = `0.75`),
                  alpha = 0.5) +
      geom_line() + 
      geom_vline(xintercept = Emsy$f) +
      geom_vline(xintercept = Emsy_range$f,lty=2)+
      geom_vline(xintercept = Epa$f,col='red') +
      geom_hline(yintercept = blim/1e3, col='red') + 
      geom_hline(yintercept = bpa/1e3, lty = 2) + 
      # geom_point(data=fit$res.by.year %>% 
      #              filter(stock == 'cdred_mat') %>% 
      #              select(value=total.biomass,f=F),
      #            aes(y=value/1e3)) +
      labs(y="SSB ('000 tons)", x = 'F') ,
    
    results_msy_nobtrigger %>% 
      filter(year > (max(year) -50), hr_target == 0.12) %>%
      #filter(step==1, hr_target == 0.06) %>%
      # mutate(ssb = round(ssb/1e3, 0), rec = round(rec/1e3, 0)) %>% 
      # select(ssb, rec) %>% 
      # distinct() %>% 
      # ggplot(aes(ssb, rec/1e3)) + 
      ggplot(aes(ssb/1e6, rec/1e6))  + 
      geom_point(alpha = 0.1) + 
      labs(y='Recruitment (millions)', x="SSB ('000 tons)") + 
      geom_vline(xintercept = blim/1e3, col = 'red') + 
      geom_line(data = tibble(ssb=seq(0,50,by=10)*1e6, rec = pmin(1,(ssb)/(blim*1e3))*
                                fit$res.by.year %>% 
                                filter(year >= 1985) %>% 
                                pull(recruitment) %>% 
                                mean(.,na.rm = T)),
                col = 'white',linewidth = 1.5) + 
      geom_line(data = tibble(ssb=seq(0,50,by=10)*1e6,rec = pmin(1,(ssb)/(blim*1e3))*
                                fit$res.by.year %>% 
                                filter(year >= 1985) %>% 
                                pull(recruitment) %>% 
                                mean(.,na.rm = T)),
                col = 'blue'),
    
    Pbref %>% 
      rename(Bpa = pbpa, Blim = pblim) %>% 
      pivot_longer(names_to = 'rp', cols = c('Bpa', 'Blim')) %>% 
      ggplot(aes(f,value)) + 
      geom_line(aes(col = rp)) +
      geom_hline(yintercept = 0.05) +
      labs(y = 'Prob < Ref. point', x = 'F', col = 'Ref. point') + 
      theme(legend.position = c(0.8,0.3)),
    
    ncol = 2
    
)

ggsave(plot = proj_plot, filename = file.path(outpath, 'simulationplots.png'), units = 'in', width = 6, height = 6)

## Histogram of draws fit
fithist <- 
  fit$stock.recruitment %>% 
  mutate(rec = recruitment/1e6) %>% 
  filter(year >= 2000) %>% 
  pull(rec) %>% hist(breaks = seq(0, 11, by = 1))

simhist <- 
  results_msy_nobtrigger %>% 
  filter(year > 2025, hr_target == 0.06) %>% 
  mutate(rec = rec/1e6) %>% 
  pull(rec) %>% hist(breaks = seq(0, 11, by = 1))

recounts <- data.frame(Type = 'Base_fit', c = fithist$counts/sum(fithist$counts), mids = fithist$mids) %>% 
  bind_rows(
    data.frame(Type = 'Simulation', c = simhist$counts/sum(simhist$counts), mids = simhist$mids)
  ) %>% 
  ggplot(aes(mids, c, fill = Type)) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  labs(x = "Recruitment (millions)", y = "Proportion")

ggsave(plot = recounts, filename = file.path(outpath, 'rec_counts.png'), units = 'in', width = 6, height = 6)




refPoints <- 
  tibble(blim = blim, 
         bpa = bpa,
         btrigger = bpa,
         hr_msy_tmp = Emsy$hr_target, fmsy_tmp = Emsy$f, msy = Emsy$value,
         hr_pa = Epa$hr_target, fpa = Epa$f,
         hr_lim = Elim$hr_lim, flim = Elim$flim)

refPoints$fmsy <- min(refPoints$fpa, refPoints$fmsy_tmp)
refPoints$hr_msy <- min(refPoints$hr_pa, refPoints$hr_msy_tmp)

save(refPoints,file=file.path(outpath, 'refpoint.Rdata'))

write.csv(refPoints, file=file.path(outpath, 'refpoints.csv'), row.names = FALSE)

