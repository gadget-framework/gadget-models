# ---------------------------------------------------------------------
Sys.setenv(GADGET_WORKING_DIR=normalizePath(gd$dir))
callGadget(s=1,log = 'init.log') #ignore.stderr = FALSE,

## update the input parameters of vendace and ringed seal retrieving estimates from single sp run
ven.param <- read.gadget.parameters("~/../valerio/Share/Gadget/vendace/vendace_13/vendace01/WGTS/params.final")
rin.param <- read.gadget.parameters("~/../valerio/Share/Gadget/ringed_seal/ringed_seal_18.3/ringseal_02/WGTS/params.final")

read.gadget.parameters(sprintf('%s/params.out',gd$dir)) %>%
  init_guess('ven.rec.1991',ven.param[ven.param$switch=='ven.rec.1991','value'],0.001,100,1) %>%
  init_guess('ven.rec.1992',ven.param[ven.param$switch=='ven.rec.1992','value'],0.001,100,1) %>%
  init_guess('ven.rec.1993',ven.param[ven.param$switch=='ven.rec.1993','value'],0.001,100,1) %>%
  init_guess('ven.rec.1994',ven.param[ven.param$switch=='ven.rec.1994','value'],0.001,100,1) %>%
  init_guess('ven.rec.1995',ven.param[ven.param$switch=='ven.rec.1995','value'],0.001,100,1) %>%
  init_guess('ven.rec.1996',ven.param[ven.param$switch=='ven.rec.1996','value'],0.001,100,1) %>%
  init_guess('ven.rec.1997',ven.param[ven.param$switch=='ven.rec.1997','value'],0.001,100,1) %>%
  init_guess('ven.rec.1998',ven.param[ven.param$switch=='ven.rec.1998','value'],0.001,100,1) %>%
  init_guess('ven.rec.1999',ven.param[ven.param$switch=='ven.rec.1999','value'],0.001,100,1) %>%
  init_guess('ven.rec.2000',ven.param[ven.param$switch=='ven.rec.2000','value'],0.001,100,1) %>%
  init_guess('ven.rec.2001',ven.param[ven.param$switch=='ven.rec.2001','value'],0.001,100,1) %>%
  init_guess('ven.rec.2002',ven.param[ven.param$switch=='ven.rec.2002','value'],0.001,100,1) %>%
  init_guess('ven.rec.2003',ven.param[ven.param$switch=='ven.rec.2003','value'],0.001,100,1) %>%
  init_guess('ven.rec.2004',ven.param[ven.param$switch=='ven.rec.2004','value'],0.001,100,1) %>%
  init_guess('ven.rec.2005',ven.param[ven.param$switch=='ven.rec.2005','value'],0.001,100,1) %>%
  init_guess('ven.rec.2006',ven.param[ven.param$switch=='ven.rec.2006','value'],0.001,100,1) %>%
  init_guess('ven.rec.2007',ven.param[ven.param$switch=='ven.rec.2007','value'],0.001,100,1) %>%
  init_guess('ven.rec.2008',ven.param[ven.param$switch=='ven.rec.2008','value'],0.001,100,1) %>%
  init_guess('ven.rec.2009',ven.param[ven.param$switch=='ven.rec.2009','value'],0.001,100,1) %>%
  init_guess('ven.rec.2010',ven.param[ven.param$switch=='ven.rec.2010','value'],0.001,100,1) %>%
  init_guess('ven.rec.2011',ven.param[ven.param$switch=='ven.rec.2011','value'],0.001,100,1) %>%
  init_guess('ven.rec.2012',ven.param[ven.param$switch=='ven.rec.2012','value'],0.001,100,1) %>%
  init_guess('ven.rec.2013',ven.param[ven.param$switch=='ven.rec.2013','value'],0.001,100,1) %>%
  init_guess('ven.rec.2014',ven.param[ven.param$switch=='ven.rec.2014','value'],0.001,100,1) %>%
  init_guess('ven.rec.2015',ven.param[ven.param$switch=='ven.rec.2015','value'],0.001,100,1) %>%
  init_guess('ven.rec.2016',ven.param[ven.param$switch=='ven.rec.2016','value'],0.001,100,1) %>%
  init_guess('ven.rec.2017',ven.param[ven.param$switch=='ven.rec.2017','value'],0.001,100,1) %>%
  init_guess('ven.rec.2018',ven.param[ven.param$switch=='ven.rec.2018','value'],0.001,100,1) %>%
  init_guess('ven.rec.2019',ven.param[ven.param$switch=='ven.rec.2018','value'],0.001,100,1) %>%
  init_guess('ven.rec.2020',ven.param[ven.param$switch=='ven.rec.2018','value'],0.001,100,1) %>%

  init_guess('venimm.init.1',ven.param[ven.param$switch=='venimm.init.1','value'],0.001,100,1) %>%
  init_guess('venmat.init.1',ven.param[ven.param$switch=='venmat.init.1','value'],0.001,100,1) %>%
  init_guess('venmat.init.2',ven.param[ven.param$switch=='venmat.init.2','value'],0.001,100,1) %>%
  init_guess('venmat.init.3',ven.param[ven.param$switch=='venmat.init.3','value'],0.001,100,1) %>%
  init_guess('venmat.init.4',ven.param[ven.param$switch=='venmat.init.4','value'],0.001,100,1) %>%
  init_guess('venmat.init.5',ven.param[ven.param$switch=='venmat.init.5','value'],0.001,100,1) %>%
  init_guess('venmat.init.6',ven.param[ven.param$switch=='venmat.init.6','value'],0.001,100,1) %>%
  init_guess('venmat.init.7',ven.param[ven.param$switch=='venmat.init.7','value'],0.001,100,1) %>%
  init_guess('venmat.init.8',ven.param[ven.param$switch=='venmat.init.8','value'],0.001,100,1) %>%
      
  init_guess('ven.recl',ven.param[ven.param$switch=='ven.recl','value'],5,15,0) %>%
  init_guess('ven.rec.sd',ven.param[ven.param$switch=='ven.rec.sd','value'],0.01,15,0) %>%
      
  init_guess('ven.Linf',ven.param[ven.param$switch=='ven.Linf','value'],12,25,0) %>%
  init_guess('ven.k',ven.param[ven.param$switch=='ven.k','value'],0.1,100,0) %>%
  init_guess('ven.bbin',ven.param[ven.param$switch=='ven.bbin','value'],0.001,50,1) %>%

  init_guess('ven.com1.alpha',ven.param[ven.param$switch=='ven.com1.alpha','value'],0.01,3,1) %>%
  init_guess('ven.com1.l50',ven.param[ven.param$switch=='ven.com1.l50','value'],2,20,1) %>%
  init_guess('ven.com2.alpha',ven.param[ven.param$switch=='ven.com2.alpha','value'],0.01,3,1) %>%
  init_guess('ven.com2.l50',ven.param[ven.param$switch=='ven.com2.l50','value'],2,20,1) %>%
  init_guess('ven.aco.alpha',ven.param[ven.param$switch=='ven.aco.alpha','value'],0.01,3,1) %>%
  init_guess('ven.aco.l50',ven.param[ven.param$switch=='ven.aco.l50','value'],2,20,1) %>%

  init_guess('venimm.walpha',ven.param[ven.param$switch=='venimm.walpha','value'],1e-10,1,0) %>%
  init_guess('venimm.wbeta',ven.param[ven.param$switch=='venimm.wbeta','value'],2,4,0) %>%
  init_guess('venmat.walpha',ven.param[ven.param$switch=='venmat.walpha','value'],1e-10,1,0) %>%
  init_guess('venmat.wbeta',ven.param[ven.param$switch=='venmat.wbeta','value'],2,4,0) %>%

  init_guess('venimm.M$',0.2,0.001,1,0) %>% 
  init_guess('venmat.M$',0.2,0.001,1,0) %>% 
  init_guess('ven.rec.scalar',1e3,1,1e8,0) %>% 
  init_guess('venimm.init.scalar',1e3,1,1e8,0) %>% 
  init_guess('venmat.init.scalar',1e3,1,1e8,0) %>%
  init_guess('ven.init.F',0.3,0.1,1,0) %>%
      
  init_guess('ven.mat1',ven.param[ven.param$switch=='ven.mat1','value'],0.1,10,0) %>%
  init_guess('ven.mat2',ven.param[ven.param$switch=='ven.mat2','value'],1,30,0) %>%

## update the input parameters of ringed seal with values from single sp run

## read.gadget.parameters(sprintf('%s/params.in',gd$dir)) %>% 
  ## init_guess('rin.rec.[0-9]',1,0.001,1000,1) %>%
  ## init_guess('rinimm.init.[0-9]',1,0.001,1000,1) %>%
  ## init_guess('rinmat.init.[0-9]',1,0.001,1000,1) %>%
  ## init_guess('rec.[0-9]|init.[0-9]',1,0.01,100,1) %>%
  ## init_guess('init.[0-9]',1,0.01,100,1) %>%
  init_guess('rinimm.init.1',rin.param[rin.param$switch=='rinimm.init.1','value'],0,1000,0) %>%
  init_guess('rinimm.init.2',rin.param[rin.param$switch=='rinimm.init.2','value'],0,1000,0) %>%
  init_guess('rinimm.init.3',rin.param[rin.param$switch=='rinimm.init.3','value'],0,1000,0) %>%
  init_guess('rinimm.init.4',rin.param[rin.param$switch=='rinimm.init.4','value'],0,1000,0) %>%
  init_guess('rinimm.init.5',rin.param[rin.param$switch=='rinimm.init.5','value'],0,1000,0) %>%

  init_guess('rinmat.init.4',rin.param[rin.param$switch=='rinmat.init.4','value'],0,1000,0) %>%
  init_guess('rinmat.init.5',rin.param[rin.param$switch=='rinmat.init.5','value'],0,1000,0) %>%
  init_guess('rinmat.init.6',rin.param[rin.param$switch=='rinmat.init.6','value'],0,1000,0) %>%
  init_guess('rinmat.init.7',rin.param[rin.param$switch=='rinmat.init.7','value'],0,1000,0) %>%
  init_guess('rinmat.init.8',rin.param[rin.param$switch=='rinmat.init.8','value'],0,1000,0) %>%
  init_guess('rinmat.init.9',rin.param[rin.param$switch=='rinmat.init.9','value'],0,1000,0) %>%
  init_guess('rinmat.init.10',rin.param[rin.param$switch=='rinmat.init.10','value'],0,1000,0) %>%
  init_guess('rinmat.init.11',rin.param[rin.param$switch=='rinmat.init.11','value'],0,1000,0) %>%
  init_guess('rinmat.init.12',rin.param[rin.param$switch=='rinmat.init.12','value'],0,1000,0) %>%
  init_guess('rinmat.init.13',rin.param[rin.param$switch=='rinmat.init.13','value'],0,1000,0) %>%
  init_guess('rinmat.init.14',rin.param[rin.param$switch=='rinmat.init.14','value'],0,1000,0) %>%
  init_guess('rinmat.init.15',rin.param[rin.param$switch=='rinmat.init.15','value'],0,1000,0) %>%
  init_guess('rinmat.init.16',rin.param[rin.param$switch=='rinmat.init.16','value'],0,1000,0) %>%
  init_guess('rinmat.init.17',rin.param[rin.param$switch=='rinmat.init.17','value'],0,1000,0) %>%
  init_guess('rinmat.init.18',rin.param[rin.param$switch=='rinmat.init.18','value'],0,1000,0) %>%
  init_guess('rinmat.init.19',rin.param[rin.param$switch=='rinmat.init.19','value'],0,1000,0) %>%
  init_guess('rinmat.init.20',rin.param[rin.param$switch=='rinmat.init.20','value'],0,1000,0) %>%
  init_guess('rinmat.init.21',rin.param[rin.param$switch=='rinmat.init.21','value'],0,1000,0) %>%
  init_guess('rinmat.init.22',rin.param[rin.param$switch=='rinmat.init.22','value'],0,1000,0) %>%
  init_guess('rinmat.init.23',rin.param[rin.param$switch=='rinmat.init.23','value'],0,1000,0) %>%
  init_guess('rinmat.init.24',rin.param[rin.param$switch=='rinmat.init.24','value'],0,1000,0) %>%
  init_guess('rinmat.init.25',rin.param[rin.param$switch=='rinmat.init.25','value'],0,1000,0) %>%

  init_guess('rin.recl',rin.param[rin.param$switch=='rin.recl','value'],50,90,0) %>%
  init_guess('rin.rec.sd',rin.param[rin.param$switch=='rin.rec.sd','value'],4,10,0) %>%

  init_guess('rin.Linf',rin.param[rin.param$switch=='rin.Linf','value'],120,180,0) %>%
  init_guess('rin.k',rin.param[rin.param$switch=='rin.k','value'],0.1,100,0) %>%
  init_guess('rin.bbin',rin.param[rin.param$switch=='rin.bbin','value'],0.001,50,0) %>%

  init_guess('rin.hun.alpha',rin.param[rin.param$switch=='rin.hun.alpha','value'],0.01,3,0) %>%
  ## init_guess('rin.hun.l50',rin.param[rin.param$switch=='rin.hun.l50','value'],70,120,0) %>%
  init_guess('rin.aco.alpha',rin.param[rin.param$switch=='rin.aco.alpha','value'],0.1,10,0) %>%
      
  init_guess('rinimm.walpha',rin.param[rin.param$switch=='rinimm.walpha','value'],1e-10,1,0) %>%
  init_guess('rinimm.wbeta',rin.param[rin.param$switch=='rinimm.wbeta','value'],2,4,0) %>%
  init_guess('rinmat.walpha',rin.param[rin.param$switch=='rinmat.walpha','value'],1e-10,1,0) %>%
  init_guess('rinmat.wbeta',rin.param[rin.param$switch=='rinmat.wbeta','value'],2,4,0) %>%

  ## init_guess('rin.spw.mu',rin.param[rin.param$switch=='rin.spw.mu','value'],1,100,0) %>%
  ## init_guess('rin.spw.la',rin.param[rin.param$switch=='rin.spw.la','value'],1,1000,1) %>%
  init_guess('rin.spw.p0', rin.param[rin.param$switch=='rin.spw.p0','value'], 0.1, 1, 1) %>% # pregnancy rate

  init_guess('rinimm.M',rin.param[rin.param$switch=='rinimm.M','value'],0.001,1,0) %>%
  init_guess('rinmat.M',rin.param[rin.param$switch=='rinmat.M','value'],0.001,1,0) %>%

  init_guess('rin.init.scalar',rin.param[rin.param$switch=='rin.init.scalar','value'],1e-2,1e2,1) %>%

  init_guess('rin.mat1',rin.param[rin.param$switch=='rin.mat1','value'],0.1,5,0) %>%
  init_guess('rin.mat2',rin.param[rin.param$switch=='rin.mat2','value'],1,6,0) %>%
  ## init_guess('rin.mat1',rin.param[rin.param$switch=='rin.mat1','value'],0.1,5,0) %>%
  ## init_guess('rin.mat2',rin.param[rin.param$switch=='rin.mat2','value'],90,130,0) %>%

  init_guess('over1.rinher',0.5,0,1,0) %>% # proportion GoB herring pop available to ringed seal in Q1
  init_guess('over2.rinher',0.5,0,1,0) %>% # proportion GoB herring pop available to ringed seal in Q2
  init_guess('over3.rinher',0.5,0,1,0) %>% # proportion GoB herring pop available to ringed seal in Q3
  init_guess('over4.rinher',0.5,0,1,0) %>% # proportion GoB herring pop available to ringed seal in Q4

## update stomachs params with initial guess      
  init_guess('p0rin2ven',0,0,1,0) %>%
  init_guess('p1rin2ven',1,0.1,2,0) %>%
  init_guess('p2rin2ven1',1,0.1,1,1) %>%
  init_guess('p2rin2ven2',1,0.1,1,1) %>%
  init_guess('p3rin2ven',2,0.1,10,1) %>%
  init_guess('p4rin2ven',2,0.1,10,1) %>%
  init_guess('p5rin2ven',4,1,10,1) %>%
  init_guess('p0rin2her1',0.2,0.1,1,1) %>%
  init_guess('p0rin2her2',0.2,0.1,1,1) %>%
  ## init_guess('p0rin2her',0.4,0.1,1.5,0) %>%
  ## init_guess('p1rin2her',0,0,1,0) %>%
  ## init_guess('p2rin2her',1,0.5,1.5,1) %>%
  ## init_guess('p3rin2her',0.5,0.1,1.5,1) %>%
  init_guess('p0rin2oth1',0.2,0.1,1,1) %>%
  init_guess('p0rin2oth2',0.2,0.1,1,1) %>%
  ## init_guess('p0rin2oth',0.4,0.1,1.5,0) %>%
  ## init_guess('p1rin2oth',0,0,1,0) %>%
  ## init_guess('p2rin2oth',1,0.5,1.5,1) %>%
  ## init_guess('p3rin2oth',0.5,0.1,1.5,1) %>%
  init_guess('prefrin2ven',1,0.1,10,0) %>%
  init_guess('prefrin2her',1,0.1,10,0) %>%
  init_guess('prefrin2oth',1,0.1,10,0) %>%
  init_guess('m0rin',3.97,0.1,10,0) %>%
  init_guess('m1rin',1,0,1,0) %>%
  init_guess('m2rin',0,0,1,0) %>%
  init_guess('m3rin',2.57,0.1,10,0) %>%
  init_guess('rin.Hfeed',0,0,10,0) %>%
      
write.gadget.parameters(.,file=sprintf('%s/params.in',gd$dir))
