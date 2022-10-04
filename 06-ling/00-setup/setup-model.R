## Useful constansts

## weight length relationship
lw.constants <- 
  mfdb_dplyr_sample(mdb) %>% 
  filter(species == local(defaults$species),
         sampling_type == 'IGFS',
         !is.na(weight)) %>% 
  select(length,weight) %>% 
  collect(n=Inf) %>% 
  lm(log(weight/1e3)~log(length),.) %>% 
  broom::tidy() %>% 
  select(estimate)
## transport back to right dimension
lw.constants$estimate[1] <- exp(lw.constants$estimate[1])

## initial conditions sigma
init.sigma <- 
  mfdb_dplyr_sample(mdb) %>% 
  dplyr::filter(species == local(defaults$species),age >0,!is.na(length))  %>% 
  dplyr::select(age,length) %>% 
  dplyr::collect(n=Inf) %>% 
  dplyr::group_by(age) %>% 
  dplyr::summarise(ml=mean(length,na.rm=TRUE),ms=sd(length,na.rm=TRUE))

## initial guess for the maturity ogive:
mat.l50 <- 
  mfdb_dplyr_sample(mdb) %>% 
  filter(species == local(defaults$species),
         sampling_type == 'IGFS',
         !is.na(maturity_stage)) %>% 
  select(length,maturity_stage) %>% 
  group_by(length,maturity_stage) %>% 
  dplyr::summarise(n=n()) %>% 
  group_by(length) %>% 
  dplyr::mutate(p=n/sum(n)) %>% 
  ungroup() %>% 
  filter(maturity_stage=='2',p>0.50,length>25) %>% 
  dplyr::summarise(l50=min(length)) %>% 
  collect(n=Inf)


## setup the immature stock first
ling.imm <- 
  gadgetstock('lingimm',gd$dir,missingOkay = TRUE) %>%
  gadget_update('stock',
                minage = 3,
                maxage = 10,
                minlength = 20,
                maxlength = 160,
                dl = 4,
                livesonareas = 1) %>%
  gadget_update('doesgrow', ## note to self the order of these parameters make difference
                growthparameters=c(linf='#ling.Linf', 
                                   k=to.gadget.formulae(quote(0.001*ling.k)),
                                   alpha = '#lingimm.walpha',
                                   beta = '#lingimm.wbeta'),
                beta = to.gadget.formulae(quote(10*ling.bbin))) %>% 
  gadget_update('initialconditions',
                normalparam = tibble(age = .[[1]]$minage:.[[1]]$maxage,
                                         area = 1,
                                         age.factor = parse(text=sprintf('exp(-1*(lingimm.M+ling.init.F)*%1$s)*lingimm.init.%1$s',age)) %>% 
                                           map(to.gadget.formulae) %>% 
                                           unlist(),   
                                         area.factor = '#lingimm.init.scalar',
                                         mean = von_b_formula(age,linf='ling.Linf',k='ling.k',recl='ling.recl'),
                                         stddev = init.sigma$ms[age],
                                         alpha = '#lingimm.walpha',
                                         beta = '#lingimm.wbeta')) %>% 
  ## does"something" updates should also allow for other names, e.g. doesrenew -> recruitment etc..
  gadget_update('refweight',
                data=data_frame(length=seq(.[[1]]$minlength,.[[1]]$maxlength,.[[1]]$dl),
                                mean=lw.constants$estimate[1]*length^lw.constants$estimate[2])) %>% 
  gadget_update('iseaten',1) %>% 
  gadget_update('doesmature', 
                maturityfunction = 'continuous',
                maturestocksandratios = 'lingmat 1',
                coefficients = '( * 0.001 #ling.mat1) #ling.mat2 0 0') %>% 
  gadget_update('doesmove',
                transitionstocksandratios = 'lingmat 1',
                transitionstep = 4) %>% 
  gadget_update('doesrenew',
                normalparam = data_frame(year = year_range,
                                         step = 1,
                                         area = 1,
                                         age = .[[1]]$minage,
                                         number = parse(text=sprintf('ling.rec.scalar*ling.rec.%s',year)) %>% 
                                           map(to.gadget.formulae) %>% 
                                           unlist(),
                                         mean = von_b_formula(age,linf='ling.Linf',k='ling.k',recl='ling.recl'),
                                         stddev = '#ling.rec.sd',
                                         alpha = '#lingimm.walpha',
                                         beta = '#lingimm.wbeta')) 




ling.mat <-
  gadgetstock('lingmat',gd$dir,missingOkay = TRUE) %>%
  gadget_update('stock',
                minage = 5,
                maxage = 15,
                minlength = 20,
                maxlength = 160,
                dl = 4,
                livesonareas = 1) %>%
  gadget_update('doesgrow', ## note to self the order of these parameters make difference
                growthparameters=c(linf='#ling.Linf', 
                                   k=to.gadget.formulae(quote(0.001*ling.k)),
                                   alpha = '#lingimm.walpha',
                                   beta = '#lingimm.wbeta'),
                beta = to.gadget.formulae(quote(10*ling.bbin))) %>% 
  gadget_update('initialconditions',
                normalparam = data_frame(age = .[[1]]$minage:.[[1]]$maxage,
                                         area = 1,
                                         age.factor = parse(text=sprintf('exp(-1*(lingmat.M+ling.init.F)*%1$s)*lingmat.init.%1$s',age)) %>% 
                                           map(to.gadget.formulae) %>% 
                                           unlist(),
                                         area.factor = '#lingmat.init.scalar',
                                         mean = von_b_formula(age,linf='ling.Linf',k='ling.k',recl='ling.recl'),
                                         stddev = init.sigma$ms[age],
                                         alpha = '#lingmat.walpha',
                                         beta = '#lingmat.wbeta')) %>% 
  ## does"something" updates should also allow for other names, e.g. doesrenew -> recruitment etc..
  gadget_update('refweight',
                data=data_frame(length=seq(.[[1]]$minlength,.[[1]]$maxlength,.[[1]]$dl),
                                mean=lw.constants$estimate[1]*length^lw.constants$estimate[2])) %>% 
  gadget_update('iseaten',1) 


## write to file
ling.imm %>% 
  write.gadget.file(gd$dir)

ling.mat %>% 
  write.gadget.file(gd$dir)



Sys.setenv(GADGET_WORKING_DIR=normalizePath(gd$dir))
callGadget(s=1,log = 'init.log') #ignore.stderr = FALSE,

## update the input parameters with sane initial guesses
read.gadget.parameters(sprintf('%s/params.out',gd$dir)) %>% 
  init_guess('rec.[0-9]|init.[0-9]',1,0.001,1000,1) %>%
  init_guess('recl',12,4,20,1) %>% 
  init_guess('rec.sd',5, 4, 20,1) %>% 
  init_guess('Linf',160, 100, 200,1) %>% 
  init_guess('k$',90, 40, 100,1) %>% 
  init_guess('bbin',6, 1e-08, 100, 1) %>% 
  init_guess('alpha', 0.5,  0.01, 3, 1) %>% 
  init_guess('l50',50,10,100,1) %>% 
  init_guess('walpha',lw.constants$estimate[1], 1e-10, 1,0) %>% 
  init_guess('wbeta',lw.constants$estimate[2], 2, 4,0) %>% 
  init_guess('M$',0.15,0.001,1,0) %>% 
  init_guess('rec.scalar',400,1,500,1) %>% 
  init_guess('init.scalar',200,1,300,1) %>% 
  init_guess('mat2',mat.l50$l50,0.75*mat.l50$l50,1.25*mat.l50$l50,1) %>% 
  init_guess('mat1',70,  10, 200, 1) %>% 
  init_guess('init.F',0.4,0.1,1,1) %>% 
  init_guess('p0',0,0,1,1) %>% 
  init_guess('p2',1,0,1,1) %>% 
  init_guess('p3',1,0.01,100,1) %>% 
  init_guess('p4',1,0.01,100,1) %>% 
  init_guess('mode',70,30,90,1) %>% 
  write.gadget.parameters(.,file=sprintf('%s/params.in',gd$dir))
