# ---------------------------------------------------------------------
# Set the ringed seal model
require(tidyverse)

tmp <- mfdb_sample_rawdata(mdb, c('age','sex','maturity_stage'),
                           list(## year=mfdb_unaggregated(),
                                year=year_range,
                                step = mfdb_unaggregated(),
                                area = mfdb_unaggregated(),
                                length = mfdb_interval("len", seq(0, 200, by = 1)),
                                age = mfdb_unaggregated(),
                                sex = mfdb_unaggregated(),
                                maturity_stage = mfdb_unaggregated(),
                                data_source='ringed_seal_individuals'))[[1]] %>%
    mutate(Length = raw_length,
           Weight = raw_weight/1000) #g2kg

## weight length relationship
lw.constants.rin <-
  tmp %>%
  select(Length,Weight) %>%
  na.omit() %>%
  mutate(Weight=Weight) %>%
  ## collect(n=Inf) %>%
  lm(log(Weight)~log(Length),.) %>% 
  broom::tidy() %>% 
  select(estimate)
## transport back to right dimension
lw.constants.rin$estimate[1] <- exp(lw.constants.rin$estimate[1])
lw.constants.rin <- data.frame(a=lw.constants.rin$estimate[1],
                               b=lw.constants.rin$estimate[2])
## initial conditions sigma
init.sigma.rin0 <- init.sigma.rin <-
    tmp %>%
    select(year,age,Length) %>%
    na.omit() %>%
    group_by(age) %>%
    summarise(ml = mean(Length),
              ms = sd(Length))

# alternative use non-linear quantile regression
# 1 sd is approx 68% corresponding to qq16 and qq84
library(quantreg)
datLenAge <-
    tmp %>%
    select(step,age,Length) %>%
    mutate(AgeX2 = age + step/12-1/24) %>%
    na.omit()
grw.qregr50.rin <-
    datLenAge %>%
    nlrq(Length~Linf*(1-exp(-k*(AgeX2-t0))),. , start=list(Linf=130,k=0.2,t0=-1), tau=0.5) %>%
    broom::tidy() %>% 
    select(estimate)
grw.qregr50.rin <- data.frame(Linf= grw.qregr50.rin$estimate[1],
                              k   = grw.qregr50.rin$estimate[2],
                              t0  = grw.qregr50.rin$estimate[3])
grw.qregr16.rin <-
    datLenAge %>%
    nlrq(Length~Linf*(1-exp(-k*(AgeX2-t0))),. , start=list(Linf=130,k=0.2,t0=-1), tau=0.16) %>%
    broom::tidy() %>% 
    select(estimate)
grw.qregr16.rin <- data.frame(Linf= grw.qregr16.rin$estimate[1],
                              k   = grw.qregr16.rin$estimate[2],
                              t0  = grw.qregr16.rin$estimate[3])
grw.qregr84.rin <-
    datLenAge %>%
    nlrq(Length~Linf*(1-exp(-k*(AgeX2-t0))),. , start=list(Linf=130,k=0.2,t0=-1), tau=0.84) %>%
    broom::tidy() %>% 
    select(estimate)
grw.qregr84.rin <- data.frame(Linf= grw.qregr84.rin$estimate[1],
                              k   = grw.qregr84.rin$estimate[2],
                              t0  = grw.qregr84.rin$estimate[3])

init.sigma.rin <- data.frame(age=1:30,
                              ml=grw.qregr50.rin$Linf*(1-exp(-grw.qregr50.rin$k*(1:30-grw.qregr50.rin$t0))),
                              ms1=grw.qregr50.rin$Linf*(1-exp(-grw.qregr50.rin$k*(1:30-grw.qregr50.rin$t0))) -
                                  grw.qregr16.rin$Linf*(1-exp(-grw.qregr16.rin$k*(1:30-grw.qregr16.rin$t0))),
                              ms2=grw.qregr84.rin$Linf*(1-exp(-grw.qregr84.rin$k*(1:30-grw.qregr84.rin$t0))) -
                                  grw.qregr50.rin$Linf*(1-exp(-grw.qregr50.rin$k*(1:30-grw.qregr50.rin$t0))))
init.sigma.rin <- init.sigma.rin %>%
                  mutate(ms = (ms1+ms2)/2) %>%
                  select(age,ml,ms)

## initial growth
datLenAge <-
    tmp %>%
    select(step,age,Length) %>%
    mutate(AgeX2 = age + step/12-1/24) %>% #***CHECK if birthday assumed 1 Jan
    na.omit()
grw.constants.rin <-
    datLenAge %>%
    nls(Length~Linf*(1-exp(-k*(AgeX2-t0))),. , start=list(Linf=190,k=1e-2,t0=0.2)) %>%
    broom::tidy() %>% 
    select(estimate)
grw.constants.rin <- data.frame(Linf= grw.constants.rin$estimate[1],
                                k   = grw.constants.rin$estimate[2],
                                t0  = grw.constants.rin$estimate[3])

## initial guess for the maturity ogive
propMat <-
    tmp %>%
    mutate(maturity_stage = ifelse(is.na(maturity_stage) & age <= 3, 1, # assume 0-3=juv and 6+=adu
                            ifelse(is.na(maturity_stage) & age >= 6, 5, maturity_stage))) %>%
    filter(!is.na(maturity_stage)) %>%
    mutate(matGrp = ifelse(maturity_stage==1, "imm", "mat"))  %>%
    mutate(lenGrp = round(Length/10)*10) %>%
    select(lenGrp,matGrp) %>%
    na.omit() %>%
    filter(matGrp != "unspec") %>%
    group_by(lenGrp,matGrp) %>% 
  dplyr::summarise(n=n()) %>%
  data.frame() %>%
  complete(matGrp,lenGrp, fill=list(n=0)) %>%
  group_by(lenGrp) %>% 
  dplyr::mutate(p=n/sum(n, na.rm=T)) %>% 
  filter(matGrp=="mat") %>%
  collect(n=Inf)
mat.constants.rin <- propMat %>%
  nls(p~1/(1+exp(-a*(lenGrp-l50))),. , start=list(a=0.1,l50=120)) %>%
  broom::tidy() %>% 
  select(estimate)
mat.constants.rin <- data.frame(a=mat.constants.rin$estimate[1],
                                l50=mat.constants.rin$estimate[2])

# proportion maturity at age
propMatAge <-
    tmp %>%
    mutate(maturity_stage = ifelse(is.na(maturity_stage) & age <= 3, 1, # assume 0-3=juv and 6+=adu
                            ifelse(is.na(maturity_stage) & age >= 6, 5, maturity_stage))) %>%
    filter(!is.na(maturity_stage)) %>%
    mutate(matGrp = ifelse(maturity_stage==1, "imm", "mat"))  %>%
    select(age,matGrp) %>%
    na.omit() %>%
    group_by(age,matGrp) %>%
  dplyr::summarise(n=n()) %>%
  data.frame() %>%
  complete(matGrp,age, fill=list(n=0)) %>%
  group_by(age) %>% 
  dplyr::mutate(p=n/sum(n, na.rm=T)) %>% 
  filter(matGrp=="mat") %>%
  collect(n=Inf)
matAge.constants.rin <- propMatAge %>%
  nls(p~1/(1+exp(-a*(age-a50))),. , start=list(a=0.1,a50=3.2)) %>%
  broom::tidy() %>% 
  select(estimate)
matAge.constants.rin <- data.frame(a=matAge.constants.rin$estimate[1],
                                   a50=matAge.constants.rin$estimate[2])

## initial population stable age structure from having run the model at equilibrium
init.pop <- read.table("~/../valerio/Share/Gadget/ringed_seal/ringed_seal_18.2/ringseal_01_equilibrium/WGTSeq/ageCompEq.txt", header=T)


## initial SR parameters for the BH model
sr.mu.rin <- 29 # from calculate_ssb_at_carrying_capacity.R 
sr.la.rin <- 7  #4
avgWgt <- 60 # assume 60 kg as avg adult weight
ssn <- seq(1000,190000,1000)
ssb <- ssn * avgWgt
y <- (1e3 * sr.mu.rin * ssb) / ((1e5 * sr.la.rin) + ssb)
## plot(ssb,y,cex=0.6)
## cbind(ssn,ssb,y)

## setup the immature stock first
rin.imm <-
  gadgetstock('rinimm',gd$dir,missingOkay = TRUE) %>%
  gadget_update('stock',
                minage = 0,
                maxage = 5,
                minlength = 40,
                maxlength = 140,
                dl = 5,
                livesonareas = 1) %>%
  gadget_update('refweight',
                data=data_frame(length=seq(.[[1]]$minlength,.[[1]]$maxlength,.[[1]]$dl),
                                mean=lw.constants.rin$a*length^lw.constants.rin$b)) %>% 
  gadget_update('doesgrow', ## note to self the order of these parameters make difference
                growthparameters=c(linf='#rin.Linf', 
                                   k=to.gadget.formulae(quote(0.01*rin.k)),
                                   alpha = '#rinimm.walpha',
                                   beta = '#rinimm.wbeta'),
                beta = to.gadget.formulae(quote(1e1*rin.bbin)),
                maxlengthgroupgrowth = 4) %>%
  gadget_update('naturalmortality',
                c(0.43, # 65-89% survival rate pup-subadults (Sundqvist et al 2012 Ambio)
                  rep('#rinimm.M',3),
                  to.gadget.formulae(quote((rinimm.M+rinmat.M)/2)), # intermediate M for those ages overlapping imm and mat
                  to.gadget.formulae(quote((rinimm.M+rinmat.M)/2)))) %>%
  gadget_update('initialconditions',
                normalcond = data_frame(age = 1:.[[1]]$maxage,
                                         area = 1,
                                         age.factor = parse(text=sprintf('rinimm.init.%1$s',age)) %>% 
                                         ## age.factor = parse(text=sprintf('exp(-1*(rinimm.M+rin.init.F)*%1$s)*rinimm.init.%1$s',age)) %>% 
                                           map(to.gadget.formulae) %>% 
                                           unlist(),   
                                         area.factor = '#rin.init.scalar',
                                         ## area.factor = '#rinimm.init.scalar',
                                         mean = von_b_formula(age,linf='rin.Linf',k='rin.k',recl='rin.recl'),
                                         stddev = init.sigma.rin$ms[age],
                                         relcond = 1)) %>% 
  ## does"something" updates should also allow for other names, e.g. doesrenew -> recruitment etc..
  gadget_update('iseaten',1) %>%
  gadget_update('doeseat',
                suitability = list(paste(prey='venimm', type='function', suit_func='andersenfleet', p0='#p0rin2ven',p1='#p1rin2ven',p2="Modelfiles/rin.ven.sel",p3='(* 1 #p3rin2ven)',p4='(* 1 #p3rin2ven)',p5='(* 10 #p5rin2ven)', sep='\t'),
				   paste(prey='venmat', type='function', suit_func='andersenfleet', p0='#p0rin2ven',p1='#p1rin2ven',p2="Modelfiles/rin.ven.sel",p3='(* 1 #p3rin2ven)',p4='(* 1 #p3rin2ven)',p5='(* 10 #p5rin2ven)', sep='\t'),
				   paste(prey='her', type='function', suit_func='constant', alpha="Modelfiles/rin.her.sel", sep = '\t'),
				   paste(prey='other', type='function', suit_func='constant', alpha="Modelfiles/rin.oth.sel", sep = '\t')),
                preference = list('\n',
                                  paste(prey='venimm', '#prefrin2ven', '\n', sep='\t'),
                                  paste(prey='venmat', '#prefrin2ven', '\n', sep='\t'),
                                  paste(prey='her',    '#prefrin2her', '\n', sep='\t'),
                                  paste(prey='other',  '#prefrin2oth', sep='\t')),
                maxconsumption = list(m0='(* 1e-4 #m0rin)',m1='#m1rin',m2='#m2rin',m3='#m3rin'),
                halffeedingvalue = '#rin.Hfeed') %>% 
  gadget_update('doesmature',
                maturityfunction = 'continuous',
                maturestocksandratios = 'rinmat 1',
                coefficients = '0 0 #rin.mat1 #rin.mat2') %>% # age-based maturity
  gadget_update('doesmove',
                transitionstocksandratios = 'rinmat 1',
                transitionstep = 4)

rin.imm$initialconditions$minage <- 1
rin.imm$initialconditions$maxage <- 5

rin.mat <-
  gadgetstock('rinmat',gd$dir,missingOkay = TRUE) %>%
  gadget_update('stock',
                minage = 4,
                maxage = 40,
                minlength = 90,
                maxlength = 160,
                dl = 5,
                livesonareas = 1) %>%
  gadget_update('refweight',
                data=data_frame(length=seq(.[[1]]$minlength,.[[1]]$maxlength,.[[1]]$dl),
                                mean=lw.constants.rin$a*length^lw.constants.rin$b)) %>% 
  gadget_update('doesgrow', ## note to self the order of these parameters make difference
                growthparameters=c(linf='#rin.Linf', 
                                   k=to.gadget.formulae(quote(0.01*rin.k)),
                                   alpha = '#rinmat.walpha',
                                   beta = '#rinmat.wbeta'),
                beta = to.gadget.formulae(quote(1e1*rin.bbin)),
                maxlengthgroupgrowth = 4) %>% 
  gadget_update('naturalmortality',
                c(to.gadget.formulae(quote((rinimm.M+rinmat.M)/2)), # intermediate M for those ages overlapping imm and mat
                  to.gadget.formulae(quote((rinimm.M+rinmat.M)/2)),
                  rep('#rinmat.M',35))) %>%
  gadget_update('initialconditions',
                normalcond = data_frame(age = 4:25,
                                        area = 1,
                                        age.factor = parse(text=sprintf('rinmat.init.%1$s',age)) %>% 
                                            map(to.gadget.formulae) %>% 
                                            unlist(),
                                        area.factor = '#rin.init.scalar',
                                        mean = von_b_formula(age,linf='rin.Linf',k='rin.k',recl='rin.recl'),
                                        stddev = init.sigma.rin$ms[age],
                                        relcond = 1)) %>% 
  ## does"something" updates should also allow for other names, e.g. doesrenew -> recruitment etc..
  gadget_update('doesspawn',
                spawnfile=gadgetfile(paste0("Modelfiles/", stock_names[2], ".spawnfile"),
                components=list(
               	list(
		    spawnsteps = 1,
		    spawnareas = 1,
		    firstspawnyear = 1991,
		    lastspawnyear = 2020,
		    spawnstocksandratios = 'rinimm  1',
		    proportionfunction = 'constant  0.5', # 50% (=females un der 50-50 ratio) will reproduce
		    mortalityfunction = 'constant	 0',
		    weightlossfunction = 'constant  0',
		    recruitment = 'fecundity	#rin.spw.p0  0  0  1  0',
                    stockparameters = '#rin.recl  #rin.rec.sd  #rinimm.walpha  #rinimm.wbeta')))) %>% 
  gadget_update('iseaten',1) %>%
  gadget_update('doeseat',
                suitability = list(paste(prey='venimm', type='function', suit_func='andersenfleet', p0='#p0rin2ven',p1='#p1rin2ven',p2="Modelfiles/rin.ven.sel",p3='(* 1 #p3rin2ven)',p4='(* 1 #p3rin2ven)',p5='(* 10 #p5rin2ven)', sep='\t'),
				   paste(prey='venmat', type='function', suit_func='andersenfleet', p0='#p0rin2ven',p1='#p1rin2ven',p2="Modelfiles/rin.ven.sel",p3='(* 1 #p3rin2ven)',p4='(* 1 #p3rin2ven)',p5='(* 10 #p5rin2ven)', sep='\t'),
				   paste(prey='her', type='function', suit_func='constant', alpha="Modelfiles/rin.her.sel", sep = '\t'),
				   paste(prey='other', type='function', suit_func='constant', alpha="Modelfiles/rin.oth.sel", sep = '\t')),
                preference = list('\n',
                                  paste(prey='venimm', '#prefrin2ven', '\n', sep='\t'),
                                  paste(prey='venmat', '#prefrin2ven', '\n', sep='\t'),
                                  paste(prey='her',    '#prefrin2her', '\n', sep='\t'),
                                  paste(prey='other',  '#prefrin2oth', sep='\t')),
                maxconsumption = list(m0='(* 1e-4 #m0rin)',m1='#m1rin',m2='#m2rin',m3='#m3rin'),
                halffeedingvalue = '#rin.Hfeed')

## write to file
rin.imm %>% 
  write.gadget.file(gd$dir)

rin.mat %>% 
  write.gadget.file(gd$dir)


## write all the timevariable files
source("setup_timevariables_rin.R")

