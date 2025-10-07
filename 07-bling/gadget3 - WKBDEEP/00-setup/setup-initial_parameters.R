## Useful constants
## Weight-Length relationship (Autumn survey)
lw.constants <- 
  list(MFm = 
         mfdb_dplyr_sample(mdb) %>% 
         filter(species == local(defaults$species),
                sampling_type == 'AUT',
                !is.na(weight),
                !is.na(length),
                weight > 0,
                length > 10) %>% 
         select(length,weight) %>% 
         collect(n=Inf) %>% 
         lm(log(weight/1e3)~log(length),.) %>% 
         broom::tidy() %>% 
         select(estimate),
       Fm = 
         mfdb_dplyr_sample(mdb) %>% 
         filter(species == local(defaults$species),
                sex == 'F',
                sampling_type == 'AUT',
                !is.na(weight),
                !is.na(length),
                weight > 0,
                length > 10) %>% 
         select(length,weight) %>% 
         collect(n=Inf) %>% 
         lm(log(weight/1e3)~log(length),.) %>% 
         broom::tidy() %>% 
         select(estimate)
         )
  
## transport back to right dimension
lw.constants$MFm$estimate[1] <- exp(lw.constants$MFm$estimate[1])
lw.constants$Fm$estimate[1] <- exp(lw.constants$Fm$estimate[1])

data.frame(l = 1:120) |> 
  mutate(w_comb = lw.constants$MFm$estimate[1]*l^lw.constants$MFm$estimate[2],
         female = lw.constants$Fm$estimate[1]*l^lw.constants$Fm$estimate[2]) |> 
  pivot_longer(cols = c('w_comb', 'female')) |> 
  ggplot(aes(l, value)) + 
  geom_line(aes(col = name))

## initial conditions sigma
init.sigma <- 
  mfdb_dplyr_sample(mdb) %>% 
  dplyr::filter(species == local(defaults$species), 
                !is.na(length),
                !is.na(age),
                year > 1950,
                length > 10)  %>% 
  dplyr::select(age,length) %>% 
  dplyr::collect(n=Inf) %>% 
  dplyr::group_by(age) %>% 
  dplyr::summarise(ml=mean(length,na.rm=TRUE),ms=sd(length,na.rm=TRUE), n=length(na.exclude(length)))

library(mar)
mar <- connect_mar()
init.cv <- 
  les_syni(mar) |>
  left_join(les_stod(mar)) |>
  #filter(ar <= max(local(year_range))) |>
  inner_join(les_aldur(mar) |> filter(tegund_nr == 7, aldur > 0)) |> 
  collect(n = Inf) |> 
  drop_na(aldur, lengd) |> 
  select(year = ar, month = man, 
         Sampling = synaflokkur_nr, sex = kyn_nr, 
         mat = kynthroski_nr, age = aldur, length = lengd, 
         lat = kastad_breidd, lon = kastad_lengd) |> 
  mutate(Sampling = case_when(Sampling %in% c(1,2,4,8) ~ 'Catches',
                              Sampling == 30 ~ 'Spring survey',
                              Sampling == 35 ~ 'Autumn survey',
                              .default = 'Other survey'),
         Sampling = fct_relevel(Sampling, c('Spring survey', 'Autumn survey',
                                            'Other survey', 'Catches'))) |> 
  filter(year > 1960, year < 2000) |> 
  group_by(age) |> 
  summarise(mean = mean(length), sd = sd(length), n = n()) |> 
  mutate(cv = round(sd/mean, 2))

DBI::dbDisconnect(mar)

## Initial coefficients for sd
init.sigma.coef <- 
  init.sigma %>% 
  filter(age >= min(unlist(lapply(stocks, gadget3::g3_stock_def, 'minage'))) & 
         age <= max(unlist(lapply(stocks, gadget3::g3_stock_def, 'minage')))) %>% 
  lm(I(ms/ml)~I(1/age) + age, data = .) %>% 
  coefficients() %>% 
  setNames(c('alpha', 'beta', 'gamma'))


## initial guess for the maturity ogive:
mat.l50 <- 
  mfdb_dplyr_sample(mdb) %>% 
  filter(species == local(defaults$species),
         sampling_type == 'AUT',
         !is.na(maturity_stage)) %>% 
  select(length,maturity_stage) %>% 
  group_by(length,maturity_stage) %>% 
  dplyr::summarise(n=n()) %>% 
  group_by(length) %>% 
  dplyr::mutate(p=n/sum(n)) %>% 
  ungroup() %>% 
  filter(maturity_stage=='2',p>0.50,length > 20) %>% 
  dplyr::summarise(l50=min(length)) %>% 
  collect(n=Inf)

## No data so taken from the Faroese stock annex 
## Magnusson and Magnusson 95 estimated and L and A50 for Icelandic males (9) and females (11)
mat.a50 <- 10

#################### GROWTH ####################################################

## Initial values for grwoth
aldat <- 
  mfdb_dplyr_sample(mdb) |> 
  dplyr::filter(species == toupper(species_name), 
                data_source == 'iceland-aldist',
                sampling_type %in% 'SEA',
                gear %in% c('BMT','NPT','DSE','PSE','PGT','SHT','GIL')) |> 
  dplyr::collect(n=Inf) |> 
  tidyr::drop_na(age) 

vonB.par <- nls(length~Linf*(1-exp(-K*(age-t0))), 
                data=aldat, 
                start=list(Linf=125, K=0.15, t0=-1))
#confint(cas.vonB.par)

plot(aldat$age, aldat$length, ylim = c(0,140), xlim = c(0,20))
predict(vonB.par, newdata = data.frame(age = seq(0,20,by = 0.1))) -> preds
lines(seq(0,20,by = 0.1), preds, col = "red")

## Default linf
growth_vals <- 
  list(linf = coef(vonB.par)[['Linf']],
       k = coef(vonB.par)[['K']],
       t0 = coef(vonB.par)[['t0']])




## OUTPUT
save(lw.constants, mat.l50, init.sigma, mat.a50, init.sigma.coef, init.cv,
     file = file.path(base_dir, 'data', 'init_param.Rdata'))

