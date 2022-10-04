# ---------------------------------------------------------------------
# Set the vendace model

lw.constants.ven <- 
  mfdb_dplyr_sample(mdb) %>% data.frame() %>% 
  filter(species == defaults.ven$species,
         data_source == 'individual_comm_vendace',
         !is.na(weight)) %>% 
  select(length,weight) %>%
  mutate(weight=weight/1000) %>% #g2kg
  collect(n=Inf) %>% 
  lm(log(weight)~log(length),.) %>% 
  broom::tidy() %>% 
  select(estimate)
## transport back to right dimension
lw.constants.ven$estimate[1] <- exp(lw.constants.ven$estimate[1])
lw.constants.ven <- data.frame(a=lw.constants.ven$estimate[1],
                               b=lw.constants.ven$estimate[2])

## VB growth params
vb.constants.ven <- 
  mfdb_dplyr_sample(mdb) %>%  data.frame() %>%
  filter(species == defaults.ven$species,
         data_source == 'individual_comm_vendace') %>% 
  select(length,age,month) %>%
  mutate(age2 = as.numeric(age) + as.numeric(month/12-1/(12*2))) %>% 
  collect(n=Inf) %>%
  nls(length ~ Linf*(1-exp(-k*(age2-t0))),. , start=list(Linf=15,k=0.5,t0=0.1)) %>%
  broom::tidy() %>% 
  select(estimate)
vb.constants.ven <- data.frame(Linf=vb.constants.ven$estimate[1],
                               k=vb.constants.ven$estimate[2],
                               t0=vb.constants.ven$estimate[3])

## initial conditions sigma
init.sigma.ven <- 
  mfdb_dplyr_sample(mdb) %>%  data.frame() %>%
  dplyr::filter(species == defaults.ven$species,
                data_source == 'individual_comm_vendace',
                ## areacell %in% defaults.spr$area[[1]],
                ## institute %in% 'SLU',
                age >0,
                !is.na(length),
                !is.na(age))  %>%
  dplyr::select(age,length) %>% 
  dplyr::collect(n=Inf) %>% 
  dplyr::group_by(age) %>% 
  dplyr::summarise(ml=mean(length,na.rm=TRUE),ms=sd(length,na.rm=TRUE))

# manually adjust sd age8+
init.sigma.ven[init.sigma.ven$age >= 8,"ms"] <- 2.00

mat.constants.ven <- 
  mfdb_dplyr_sample(mdb) %>%  data.frame() %>%
  filter(species == defaults.ven$species,
         data_source == 'maturity_aco_bench2021',
         !is.na(maturity_stage)) %>%
  group_by(length,maturity_stage) %>%
  summarise(n = sum(count)) %>%
  group_by(length) %>% 
  mutate(p=n/sum(n, na.rm=T)) %>% 
  filter(maturity_stage=='5') %>%
  collect(n=Inf) %>% 
  nls(p~1/(1+exp(-a*(length-l50))),. , start=list(a=2,l50=11)) %>%
  broom::tidy() %>% 
  select(estimate)
mat.constants.ven <- data.frame(a=mat.constants.ven$estimate[1],
                                l50=mat.constants.ven$estimate[2])

## setup the immature stock first
ven.imm <-
  gadgetstock('venimm',gd$dir,missingOkay = TRUE) %>%
  gadget_update('stock',
                minage = 0,
                maxage = 1,
                minlength = 3.5,
                maxlength = 17.5,
                dl = 0.5,
                livesonareas = 1) %>%
  gadget_update('refweight',
                data=data_frame(length=seq(.[[1]]$minlength,.[[1]]$maxlength,.[[1]]$dl),
                                mean=lw.constants.ven$a*length^lw.constants.ven$b)) %>% 
  gadget_update('doesgrow', ## note to self the order of these parameters make difference
                growthparameters=c(linf='#ven.Linf', 
                                   k=to.gadget.formulae(quote(0.01*ven.k)),
                                   alpha = '#venimm.walpha',
                                   beta = '#venimm.wbeta'),
                beta = to.gadget.formulae(quote(1e3*ven.bbin)),
                maxlengthgroupgrowth = 4) %>% 
  gadget_update('initialconditions',
                normalcond = data_frame(age = 1:.[[1]]$maxage,
                                         area = 1,
                                         age.factor = parse(text=sprintf('exp(-1*(venimm.M+ven.init.F)*%1$s)*venimm.init.%1$s',age)) %>% 
                                           map(to.gadget.formulae) %>% 
                                           unlist(),   
                                         area.factor = '#venimm.init.scalar',
                                         mean = von_b_formula(age,linf='ven.Linf',k='ven.k',recl='ven.recl'),
                                         stddev = init.sigma.ven$ms[age],
                                         relcond = 1)) %>% 
  ## does"something" updates should also allow for other names, e.g. doesrenew -> recruitment etc..
  gadget_update('iseaten',
                energycontent = 1) %>% 
  gadget_update('doesmature',
                maturityfunction = 'continuous',
                maturestocksandratios = 'venmat 1',
                coefficients = '#ven.mat1 #ven.mat2 0 0') %>% 
  gadget_update('doesmove',
                transitionstocksandratios = 'venmat 1',
                transitionstep = 4) %>% 
  gadget_update('doesrenew',
                normalparam = data_frame(year = year_range,
                                         step = 2,
                                         area = 1,
                                         age = .[[1]]$minage,
                                         number = parse(text=sprintf('ven.rec.scalar*ven.rec.%s',year)) %>% 
                                           map(to.gadget.formulae) %>% 
                                           unlist(),
                                         mean = von_b_formula(age,linf='ven.Linf',k='ven.k',recl='ven.recl'),
                                         stddev = '#ven.rec.sd',
                                         alpha = '#venimm.walpha',
                                         beta = '#venimm.wbeta'))
ven.imm$initialconditions$minage <- 1
## ven.imm$initialconditions$minlength <- 7.5
## ven.imm$initialconditions$maxlength <- 15.5

ven.mat <-
  gadgetstock('venmat',gd$dir,missingOkay = TRUE) %>%
  gadget_update('stock',
                minage = 0,
                maxage = 10,
                minlength = 3.5,
                maxlength = 20.5,
                dl = 0.5,
                livesonareas = 1) %>%
  gadget_update('doesgrow', ## note to self the order of these parameters make difference
                growthparameters=c(linf='#ven.Linf', 
                                   k=to.gadget.formulae(quote(0.01*ven.k)),
                                   alpha = '#venmat.walpha',
                                   beta = '#venmat.wbeta'),
                beta = to.gadget.formulae(quote(1e3*ven.bbin)),
                maxlengthgroupgrowth = 4) %>% 
  gadget_update('initialconditions',
                normalcond = data_frame(## age = .[[1]]$minage:.[[1]]$maxage,
                                        ## age = .[[1]]$minage:8,
                                         age = 1:8,
                                         area = 1,
                                         age.factor = parse(text=sprintf('exp(-1*(venmat.M+ven.init.F)*%1$s)*venmat.init.%1$s',age)) %>% 
                                           map(to.gadget.formulae) %>% 
                                           unlist(),
                                         area.factor = '#venmat.init.scalar',
                                         mean = von_b_formula(age,linf='ven.Linf',k='ven.k',recl='ven.recl'),
                                         stddev = init.sigma.ven$ms[age],
                                         relcond = 1)) %>% 
  ## does"something" updates should also allow for other names, e.g. doesrenew -> recruitment etc..
  gadget_update('refweight',
                data=data_frame(length=seq(.[[1]]$minlength,.[[1]]$maxlength,.[[1]]$dl),
                                mean=lw.constants.ven$a*length^lw.constants.ven$b)) %>% 
  gadget_update('iseaten',
                energycontent = 1)
ven.mat$initialconditions$minage <- 1
ven.mat$initialconditions$maxage <- 8
## ven.mat$initialconditions$minlength <- 7.5
## ven.mat$initialconditions$maxlength <- 19.5

## write to file
ven.imm %>% 
  write.gadget.file(gd$dir)

ven.mat %>% 
  write.gadget.file(gd$dir)

