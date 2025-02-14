# ---------------------------------------------------------------------
# Set the vendace model

## weight length relationship (from Mikaela 20181206)
## lw.constants.ven <- data.frame(a=2e-06,
##                                b=3.2201)

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

## tmp <- mfdb_dplyr_sample(mdb) %>% data.frame() %>%
##        filter(species == defaults.ven$species,
##               data_source == 'individual_comm_vendace',
##               !is.na(weight)) %>%
##        data.frame()
## ggplot(tmp,
##        aes(length,weight/1000)) +
##    geom_point() +
##    geom_line(data=data.frame(x=tmp$length,
##                              y=lw.constants.ven$a*tmp$length^lw.constants.ven$b),
##              aes(x,y), color=2, lwd=0.6) +
##    facet_wrap(~year)

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

## tmp <- 
##   mfdb_dplyr_sample(mdb) %>%  data.frame() %>%
##   filter(species == defaults.ven$species,
##          data_source == 'individual_comm_vendace') %>%
##   mutate(age2 = as.numeric(age) + as.numeric(month/12-1/(12*2))) %>%
##   data.frame()
## ggplot(tmp,
##        aes(age2,length)) +
##    geom_point() +
##     geom_line(data=data.frame(x=seq(min(tmp$age2,na.rm=T),max(tmp$age2,na.rm=T),length.out=100)) %>%
##                    mutate(y=vb.constants.ven$Linf*(1-exp(-vb.constants.ven$k*(x-vb.constants.ven$t0)))),
##               aes(x,y), color=2, lwd=0.6)

             
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

## initial guess for L50 of the maturity ogive:
## mat.constants.ven <- 
##   mfdb_dplyr_sample(mdb) %>%  data.frame() %>%
##   filter(species == defaults.ven$species,
##          data_source == 'individual_comm_vendace',
##          !is.na(maturity_stage)) %>% 
##   select(length,maturity_stage) %>% 
##   group_by(length,maturity_stage) %>% 
##   dplyr::summarise(n=n()) %>% 
##   group_by(length) %>% 
##   dplyr::mutate(p=n/sum(n, na.rm=T)) %>% 
##   ungroup() %>% 
##   filter(maturity_stage=='5',p>0.50) %>% 
##   dplyr::summarise(l50=min(length, na.rm=T)) %>% 
##   collect(n=Inf)
## mat.constants.ven <- data.frame(a=NA,
##                                 l50=mat.constants.ven[[1]])

## initial guess for the maturity ogive:
mat.constants.ven <- 
  mfdb_dplyr_sample(mdb) %>%  data.frame() %>%
  filter(species == defaults.ven$species,
         data_source == 'individual_comm_vendace',
         year %in% c(1998,2006:2008,2010:2018), # temporary until data get fixed***
         !is.na(maturity_stage)) %>% 
  select(length,maturity_stage) %>% 
  group_by(length,maturity_stage) %>% 
  dplyr::summarise(n=n()) %>% 
  data.frame() %>%
  complete(maturity_stage,length, fill=list(n=0)) %>%
  group_by(length) %>% 
  dplyr::mutate(p=n/sum(n, na.rm=T)) %>% 
  filter(maturity_stage=='5') %>%
  collect(n=Inf) %>% 
  nls(p~1/(1+exp(-a*(length-l50))),. , start=list(a=2,l50=11)) %>%
  broom::tidy() %>% 
  select(estimate)
mat.constants.ven <- data.frame(a=mat.constants.ven$estimate[1],
                                l50=mat.constants.ven$estimate[2])

## tmp <-   mfdb_dplyr_sample(mdb) %>%  data.frame() %>%
##   filter(species == defaults.ven$species,
##          data_source == 'individual_comm_vendace',
##          year %in% c(1998,2006:2008,2010:2018), # temporary until data get fixed***
##          !is.na(maturity_stage)) %>% 
##   select(length,maturity_stage) %>% 
##   group_by(length,maturity_stage) %>% 
##   dplyr::summarise(n=n()) %>% 
##   data.frame() %>%
##   complete(maturity_stage,length, fill=list(n=0)) %>%
##   group_by(length) %>% 
##   dplyr::mutate(p=n/sum(n, na.rm=T)) %>% 
##   filter(maturity_stage=='5') %>%
##   data.frame()
## ggplot(tmp,
##        aes(length,p)) +
##    geom_point() +
##    geom_line(data=data.frame(x=tmp$length,
##                              y=1/(1+exp(-mat.constants.ven$a*(tmp$length-mat.constants.ven$l50)))),
##              aes(x,y), color=2, lwd=0.6)

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
  ## gadget_update('initialconditions',
  ##               ## normalparam = data_frame(age = .[[1]]$minage:.[[1]]$maxage,
  ##               normalparam = data_frame(age = 1:.[[1]]$maxage,
  ##                                        area = 1,
  ##                                        age.factor = parse(text=sprintf('exp(-1*(venimm.M+ven.init.F)*%1$s)*venimm.init.%1$s',age)) %>% 
  ##                                          map(to.gadget.formulae) %>% 
  ##                                          unlist(),   
  ##                                        area.factor = '#venimm.init.scalar',
  ##                                        mean = von_b_formula(age,linf='ven.Linf',k='ven.k',recl='ven.recl'),
  ##                                        stddev = init.sigma.ven$ms[age],
  ##                                        alpha = to.gadget.formulae(quote(1e-6 * ven.walpha)),
  ##                                        beta = '#ven.wbeta')) %>% 
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
  gadget_update('iseaten',1) %>% 
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
  ## gadget_update('initialconditions',
  ##               normalparam = data_frame(## age = .[[1]]$minage:.[[1]]$maxage,
  ##                                        age = .[[1]]$minage:8,
  ##                                        area = 1,
  ##                                        age.factor = parse(text=sprintf('exp(-1*(venmat.M+ven.init.F)*%1$s)*venmat.init.%1$s',age)) %>% 
  ##                                          map(to.gadget.formulae) %>% 
  ##                                          unlist(),
  ##                                        area.factor = '#venmat.init.scalar',
  ##                                        mean = von_b_formula(age,linf='ven.Linf',k='ven.k',recl='ven.recl'),
  ##                                        stddev = init.sigma.ven$ms[age],
  ##                                        alpha = to.gadget.formulae(quote(1e-6 * ven.walpha)),
  ##                                        beta = '#ven.wbeta')) %>% 
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
  gadget_update('iseaten',1)
ven.mat$initialconditions$minage <- 1
ven.mat$initialconditions$maxage <- 8
## ven.mat$initialconditions$minlength <- 7.5
## ven.mat$initialconditions$maxlength <- 19.5

## write to file
ven.imm %>% 
  write.gadget.file(gd$dir)

ven.mat %>% 
  write.gadget.file(gd$dir)

# ---------------------------------------------------------------------
Sys.setenv(GADGET_WORKING_DIR=normalizePath(gd$dir))
callGadget(s=1,log = 'init.log') #ignore.stderr = FALSE,

## update the input parameters with sane initial guesses
read.gadget.parameters(sprintf('%s/params.out',gd$dir)) %>% 
  ## init_guess('ven.rec.[0-9]',1,0.001,1000,1) %>%
  ## init_guess('venimm.init.[0-9]',1,0.001,1000,1) %>%
  ## init_guess('venmat.init.[0-9]',1,0.001,1000,1) %>%
  init_guess('rec.[0-9]|init.[0-9]',1,0.001,100,1) %>%
  init_guess('ven.recl',9,5,15,1) %>% 
  init_guess('ven.rec.sd',1.1, 0.01, 15,1) %>% 
  init_guess('ven.Linf',vb.constants.ven$Linf, 12, 25,0) %>% 
  init_guess('ven.k$',100*vb.constants.ven$k, 0.1, 100,1) %>% 
  init_guess('ven.bbin',0.9, 0.001, 50, 1) %>% 
  init_guess('ven.com1.alpha', 0.9,  0.01, 3, 1) %>% 
  init_guess('ven.com1.l50',12,2,20,1) %>% 
  init_guess('ven.com2.alpha', 0.9,  0.01, 3, 1) %>% 
  init_guess('ven.com2.l50',12,2,20,1) %>% 
  ## init_guess('ven.ref.alpha', 0.9,  0.01, 3, 0) %>% 
  ## init_guess('ven.ref.l50',12,2,20,1) %>% 
  init_guess('ven.aco.alpha', 0.7,  0.01, 3, 1) %>% 
  init_guess('ven.aco.l50',11.5,2,20,1) %>% 
  init_guess('ven.sea.alpha', 0.7,  0.01, 3, 0) %>% 
  init_guess('ven.sea.l50',11.5,2,20,1) %>% 
  init_guess('venimm.walpha',lw.constants.ven$a, 1e-10, 1,0) %>% 
  init_guess('venimm.wbeta',lw.constants.ven$b, 2, 4,0) %>% 
  init_guess('venmat.walpha',lw.constants.ven$a, 1e-10, 1,0) %>% 
  init_guess('venmat.wbeta',lw.constants.ven$b, 2, 4,0) %>% 
  init_guess('venimm.M$',0.2,0.001,1,0) %>% 
  init_guess('venmat.M$',0.2,0.001,1,0) %>% 
  init_guess('ven.rec.scalar',1e3,1,1e8,0) %>% 
  init_guess('venimm.init.scalar',1e3,1,1e8,0) %>% 
  init_guess('venmat.init.scalar',1e3,1,1e8,0) %>% 
  init_guess('ven.mat2',mat.constants.ven$l50,1,30,1) %>% 
  init_guess('ven.mat1',mat.constants.ven$a,0.1,10,1) %>% # -0.2589 by Mikaela***
  init_guess('ven.init.F',0.3,0.1,1,0) %>%
write.gadget.parameters(.,file=sprintf('%s/params.in',gd$dir))
