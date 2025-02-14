library(Rgadget)
library(ggplot2)
library(grid)
library(gridExtra)

## rm(list=ls())

source("~/../valerio/Share/Gadget/Rscripts/ggdata_coverage.R")
source("~/../valerio/Share/Gadget/Rscripts/add_captionModel.R")
source("~/../valerio/Share/Gadget/Rscripts/ggplot_AgeLenDistributionStock.R")

setwd(gd$dir)

tmp <- gadget.iterative(rew.sI=TRUE,
                        grouping=list(
                            ind=c('si.ven.cpue','si.ven.aco')),
                        cv.floor=0.01,
                        params.file='params.in',
                        wgts="WGTS",
                        main='main')

fit <- gadget.fit(main="WGTS/main.final",
                  f.age.range=data.frame(stock=c("venimm","venmat"),age.min=1,age.max=3))

## setwd("vendace01")
## load("WGTS/WGTS.Rdata")
## fit <- out

# ------------------------------------------
# standard plots
dirFigs <- "out"
if(sum(match(list.files(),dirFigs), na.rm=T)==1){
    print(paste("folder",dirFigs,"exists"))
} else {system(paste("mkdir",dirFigs))}

figName <- "sidat_dir.ps"
postscript(paste(dirFigs,figName, sep="/"))
plot(fit, data = "sidat", type = "direct") +
    ## ggsave(paste(dirFigs,figName, sep="/"), device="ps") +
    ggRunName() + ggRunNameSize(6)
dev.off()

figName <- "sidat_xy.ps"
postscript(paste(dirFigs,figName, sep="/"))
plot(fit, data = "sidat", type = "x-y") +
    ## ggsave(paste(dirFigs,figName, sep="/"), device="ps") +
    ggRunName() + ggRunNameSize(6)
dev.off()

figName <- "summary.ps"
postscript(paste(dirFigs,figName, sep="/"))
plot(fit, data='summary') +
    ## ggsave(paste(dirFigs,figName, sep="/"), device="ps") +
  ggRunName() + ggRunNameSize(6)
dev.off()

figName <- "summary_wgt.ps"
postscript(paste(dirFigs,figName, sep="/"))
plot(fit, data='summary',type = 'weighted') +
  ## ggsave(paste(dirFigs,figName, sep="/"), device="ps") +
  ggRunName() + ggRunNameSize(6)
dev.off()

figName <- "summary_pie.ps"
postscript(paste(dirFigs,figName, sep="/"))
plot(fit, data='summary',type='pie') +
  ## ggsave(paste(dirFigs,figName, sep="/"), device="ps") +
  ggRunName() + ggRunNameSize(6)
dev.off()

figName <- "catch.ps"
postscript(paste(dirFigs,figName, sep="/"))
plot(fit, data="res.by.year", type="catch") + facet_wrap(~stock) +
  ## ggsave(paste(dirFigs,figName, sep="/"), device="ps") +
  ggRunName() + ggRunNameSize(6)
dev.off()

figName <- "F.ps"
postscript(paste(dirFigs,figName, sep="/"))
plot(fit, data="res.by.year", type="F") + facet_wrap(~stock) +
  ## ggsave(paste(dirFigs,figName, sep="/"), device="ps") +
  ggRunName() + ggRunNameSize(6)
dev.off()

figName <- "total.ps"
postscript(paste(dirFigs,figName, sep="/"))
plot(fit, data="res.by.year", type="total") + facet_wrap(~stock) +
  ## ggsave(paste(dirFigs,figName, sep="/"), device="ps") +
  ggRunName() + ggRunNameSize(6)
dev.off()

figName <- "rec.ps"
postscript(paste(dirFigs,figName, sep="/"))
plot(fit, data="res.by.year", type="rec") + facet_wrap(~stock) +
  ## ggsave(paste(dirFigs,figName, sep="/"), device="ps") +
  ggRunName() + ggRunNameSize(6)
dev.off()

## cairo_ps(paste(dirFigs,"stock_demo.ps", sep="/"))
##   plot(fit, data='stock.std')# + facet_wrap(~stock) +
##   ggRunName() + ggRunNameSize(6)
## dev.off()

figName <- "stock_demo_venmat.ps"
postscript(paste(dirFigs,figName, sep="/"))
ggAgeDistStk2(fit, stkName="venmat", ageVec=1:10, plusGroup=10) +
    ## ggsave(paste(dirFigs,figName, sep="/"), device="ps") +
   ggRunName() + ggRunNameSize(6)
dev.off()

## figName <- "suitability.ps"
## postscript(paste(dirFigs,figName, sep="/"))
##   plot(fit, data='suitability') +
##   ## ggsave(paste(dirFigs,figName, sep="/"), device="ps") +
##   ggRunName() + ggRunNameSize(6)
## dev.off()

figName <- "suitability_v2.ps"
postscript(paste(dirFigs,figName, sep="/"))
  tmp1 <- fit$suitability %>% filter(year==2015 & step==4 & stock=="venmat" & fleet %in% "comven2" & length<=18.5)
  tmp2 <- fit$suitability %>% filter(year==1995 & step==4 & stock=="venmat" & fleet %in% "comven1")
  tmp3 <- fit$suitability %>% filter(year==2015 & step==4 & stock=="venmat" & fleet %in% "aco" & length<=18.5)
  ## tmp4 <- fit$suitability %>% filter(year==2015 & step==1 & stock=="rinmat" & fleet %in% "aco.rin")
  tmp <- bind_rows(tmp1,tmp2,tmp3)
  ggplot(tmp) + geom_line(aes(length,suit,group=fleet,col=fleet)) + xlim(0,18) +
  ## ggsave(paste(dirFigs,figName, sep="/"), device="ps") +
  ggRunName() + ggRunNameSize(6)
dev.off()

tmp <- plot(fit,data = 'catchdist.fleets')
 names(tmp)

figName <- "ldist_ven_com.ps"
postscript(paste(dirFigs,figName, sep="/"))
  tmp$ldist.ven.com +
  ## ggsave(paste(dirFigs,figName, sep="/"), device="ps") +
  ggRunName() + ggRunNameSize(6)
dev.off()

figName <- "alk_ven_com.ps"
postscript(paste(dirFigs,figName, sep="/"))
  tmp$alk.ven.com +
  ## ggsave(paste(dirFigs,figName, sep="/"), device="ps") +
  ggRunName() + ggRunNameSize(6)
dev.off()

figName <- "ldist_ven_aco.ps"
postscript(paste(dirFigs,figName, sep="/"))
  tmp$ldist.ven.aco +
  ## ggsave(paste(dirFigs,figName, sep="/"), device="ps") +
  ggRunName() + ggRunNameSize(6)
dev.off()

figName <- "alk_ven_aco.ps"
postscript(paste(dirFigs,figName, sep="/"))
  cairo_ps(paste(dirFigs,"alk_aco.ps", sep="/"))
  tmp$alk.ven.aco +
  ## ggsave(paste(dirFigs,figName, sep="/"), device="ps") +
  ggRunName() + ggRunNameSize(6)
dev.off()

bubbles <- plot(fit,data = 'catchdist.fleets',type='bubble')
names(bubbles)

figName <- "bubble_ldist_ven_com.ps"
postscript(paste(dirFigs,figName, sep="/"))
  bubbles$ldist +
  ## ggsave(paste(dirFigs,figName, sep="/"), device="ps") +
  ggRunName() + ggRunNameSize(6)
dev.off()

figName <- "bubble_aldist_ven_com.ps"
postscript(paste(dirFigs,figName, sep="/"))
  bubbles$aldist +
  ## ggsave(paste(dirFigs,figName, sep="/"), device="ps") +
  ggRunName() + ggRunNameSize(6)
dev.off()

grplot <- plot(fit,data = 'catchdist.fleets',type='growth')
names(grplot)

figName <- "growth_ven_com.ps"
postscript(paste(dirFigs,figName, sep="/"))
  grplot$alk.ven.com +
  ## ggsave(paste(dirFigs,figName, sep="/"), device="ps") +
  ggRunName() + ggRunNameSize(6)
dev.off()

figName <- "growth_ven_aco.ps"
postscript(paste(dirFigs,figName, sep="/"))
  grplot$alk.ven.aco +
  ## ggsave(paste(dirFigs,figName, sep="/"), device="ps") +
  ggRunName() + ggRunNameSize(6)
dev.off()

tmp <- plot(fit,data = 'stockdist')
names(tmp)

figName <- "mat_ven_com.ps"
postscript(paste(dirFigs,figName, sep="/"))
  tmp$mat.ven.com + xlim(9,18) +
  ## ggsave(paste(dirFigs,figName, sep="/"), device="ps") +
  ggRunName() + ggRunNameSize(6)
dev.off()

figName <- "mat_ven_aco.ps"
postscript(paste(dirFigs,figName, sep="/"))
  tmp$mat.ven.aco + xlim(9,18) +
  ## ggsave(paste(dirFigs,figName, sep="/"), device="ps") +
  ggRunName() + ggRunNameSize(6)
dev.off()

figName <- "data_coverage.ps"
postscript(paste(dirFigs,figName, sep="/"))
  ggDataCoverage(fit) +
  ## ggsave(paste(dirFigs,figName, sep="/"), device="ps") +
  ggRunName() + ggRunNameSize(6)
dev.off()

# standard advice plot (catch, SSB, F, R)
tmp <- fit$res.by.year %>% group_by(year,area) %>% summarise(catch=sum(catch))
  catch.out <- ggplot(tmp) + geom_bar(aes(year,catch), stat="identity")
tmp <- fit$res.by.year %>% filter(stock=="venmat")
  ssb.out <- ggplot(tmp, aes(year,total.biomass)) + geom_line() + geom_point() +
        ylim(0,max(tmp$total.biomass)) + ylab("SSB")
tmp <- fit$res.by.year %>% filter(stock=="venimm")
  rec.out <- ggplot(tmp, aes(year,total.number)) + geom_line() + geom_point() +
        ylim(0,max(tmp$total.number)) + ylab("Number immature")
tmp <- fit$res.by.year %>% filter(stock=="venmat")
  fbar.out <- ggplot(tmp, aes(year,F)) + geom_line() + geom_point() +
        ylim(0,max(tmp$F)) + ggRunName() + ggRunNameSize(6)

figName <- "standard_plot.ps"
postscript(paste(dirFigs,figName, sep="/"))
  grid.arrange(catch.out,ssb.out,rec.out,fbar.out,ncol=2)
dev.off()

# comparison with SS3 output
ss3.ssb <- read.table("~/../valerio/Share/Gadget/vendace/SS3_output/SSB.csv", header=T, sep=",")
ss3.rec <- read.table("~/../valerio/Share/Gadget/vendace/SS3_output/R.csv", header=T, sep=",")
ss3.fbar <- read.table("~/../valerio/Share/Gadget/vendace/SS3_output/Fbar.csv", header=T, sep=",")
ssbcomp <- ssb.out +
           geom_point(data=ss3.ssb,aes(Years,SSB*1000),pch=2) +
           geom_line(data=ss3.ssb,aes(Years,SSB*1000),lty=2) +
           ylim(0,max(c(ss3.ssb$SSB*1000,ssb.out$data$total.biomass)))
fbarcomp <- fbar.out +
            geom_point(data=ss3.fbar %>% filter(Yr<=2017),aes(Yr,fbar),pch=2) +
            geom_line(data=ss3.fbar %>% filter(Yr<=2017),aes(Yr,fbar),lty=2) +
            ylim(0,max(c(ss3.fbar$fbar)))
reccomp <- rec.out +
           geom_point(data=ss3.rec,aes(Years,R*500),pch=2) +
           geom_line(data=ss3.rec,aes(Years,R*500),lty=2) +
           ylim(0,max(c(ss3.rec$R*500))) +
           ggRunName() + ggRunNameSize(6)

figName <- "standard_plot_comp.ps"
postscript(paste(dirFigs,figName, sep="/"))
  grid.arrange(ssbcomp,fbarcomp,reccomp,ncol=3)
dev.off()

source("~/../valerio/Share/Gadget/Rscripts/ggplot_stomach_PropPrey.R")
figName <- "stomLH1_rin_spp_v1.ps"
postscript(paste(dirFigs,figName, sep="/"))
   ggPropPreyStomach(fit, compName="stom1.rin") +
  ## ggsave(paste(dirFigs,figName, sep="/"), device="ps") +
   ggRunName() + ggRunNameSize(6)
dev.off()

pl1 <- ggplot(fit$stomachcontent %>%
       ## filter(year==2015) %>%
       filter(component=="stom1.rin")) +
    geom_bar(aes(pred.length, observed, fill=prey), stat="identity", position="stack") +
    ggtitle("Observed") +
    facet_grid(year~step)
pl2 <- ggplot(fit$stomachcontent %>%
       ## filter(year==2015) %>%
       filter(component=="stom1.rin")) +
    geom_bar(aes(pred.length, predicted, fill=prey), stat="identity", position="stack") +
    ggtitle("Predicted") +
    facet_grid(year~step) +
    ggRunName() + ggRunNameSize(6)

## figName <- "stomLH1_rin_spp_v2.ps"
## postscript(paste(dirFigs,figName, sep="/"))
##   grid.arrange(pl1,pl2, nrow=2)
## dev.off()
figName <- "stomLH1_rin_spp_obs_v2.ps"
postscript(paste(dirFigs,figName, sep="/"))
  pl1
dev.off()
figName <- "stomLH1_rin_spp_pred_v2.ps"
postscript(paste(dirFigs,figName, sep="/"))
  pl2
dev.off()

figName <- "stomLH2_rin_ven_v1.ps"
postscript(paste(dirFigs,figName, sep="/"))
ggplot(fit$stomachcontent %>% filter(component=="stom2.rin")) +
    geom_point(aes(prey.length, observed, group=pred.length,col=as.factor(pred.length))) +
    geom_line(aes(prey.length, predicted, group=pred.length,col=as.factor(pred.length))) +
    facet_grid(step~year) +
    ggRunName() + ggRunNameSize(6)
dev.off()

figName <- "stomLH2_rin_ven_v2.ps"
postscript(paste(dirFigs,figName, sep="/"))
ggplot(fit$stomachcontent %>% filter(component=="stom2.rin")) +
    geom_point(aes(prey.length, observed-predicted, group=pred.length,col=as.factor(pred.length))) +
    facet_grid(step~year) +
    ggRunName() + ggRunNameSize(6)
dev.off()

ggplot(fit$stomachcontent %>% filter(component=="stom2.rin")) +
    geom_point(aes(observed,predicted, group=pred.length,col=as.factor(pred.length))) +
    geom_abline(slope=1, intercept=0, col="red", lwd=0.5) +
    facet_grid(~year)

figName <- "stomLH2_rin_ven_v3.ps"
postscript(paste(dirFigs,figName, sep="/"), width=10, height=3)
ggplot(fit$stomachcontent %>% filter(component=="stom2.rin")) +
    geom_point(aes(observed,predicted, group=pred.length,col=as.factor(pred.length),pch=as.factor(year)),size=3) +
    geom_abline(slope=1, intercept=0, col="red", lwd=0.5) +
    facet_grid(~year) +
    ggRunName() + ggRunNameSize(6)
dev.off()

# pred-prey selectivity
source("~/../valerio/Share/Gadget/Rscripts/plot_selectivity.R")
## l <- fit$stock.full %>%
##      filter(stock %in% c("venimm","venmat")) %>%
##      .$length %>% unique()
## L <- fit$stock.full %>%
##      filter(stock %in% c("rinimm","rinmat")) %>%
##      summarise(min(length),max(length)) %>% as.numeric()
## L <- seq(L[1],L[2],length=5)
## pp <- fit$params %>% filter(switch %in% c("p0rin2ven","p1rin2ven","p2rin2ven","p3rin2ven")) %>% .$value
## pp <- c(pp,pp[length(pp)]) * c(1,1,1,1,1)
## plot(l, selectivityAndersen(L=L[1], l=l, params=pp), type="l")
## for(i in 2:length(L)){
##     lines(l, selectivityAndersen(L=L[i], l=l, params=pp), col=i)
## }

l <- fit$stock.full %>%
     filter(stock %in% c("venimm","venmat")) %>%
     .$length %>% unique()
pp <- fit$params %>% filter(switch %in% c("p0rin2ven","p1rin2ven","p2rin2ven1","p3rin2ven","p5rin2ven")) %>% .$value
pp <- c(pp[1:4],pp[4],pp[5])
pp <- c(pp,pp[length(pp)]) * c(1,1,1,1,1,10)
plot(l, selectivityAndersenFleet(l=l, params=pp), type="l")
lines(l, selectivityAndersenFleet(l=l, params=pp), col=2)
pp <- fit$params %>% filter(switch %in% c("p0rin2ven","p1rin2ven","p2rin2ven2","p3rin2ven","p5rin2ven")) %>% .$value
pp <- c(pp[1:4],pp[4],pp[5])
pp <- c(pp,pp[length(pp)]) * c(1,1,1,1,1,10)
lines(l, selectivityAndersenFleet(l=l, params=pp), col=3)

## L <- fit$stock.full %>%
##      filter(stock %in% c("rinimm","rinmat")) %>%
##      .$length %>% unique()
## pp <- fit$params %>% filter(switch %in% c("p0rin2oth","p1rin2oth","p2rin2oth","p3rin2oth")) %>% .$value
## pp <- pp * c(1,1,-0.1,1)
## plot(L, selectivityExp(l=10, L=L, params=pp), type="l")
## pp <- fit$params %>% filter(switch %in% c("p0rin2her","p1rin2her","p2rin2her","p3rin2her")) %>% .$value
## pp <- pp * c(1,1,-0.1,1)
## lines(L, selectivityExp(l=10, L=L, params=pp), col=2)

# -----------------------------
## # calculate realised predator consumption

## # make printfile ***CHECK FOR BETTER WAY THAN THIS HACK
## gadgetfile('printfile.custom',
##            file_type = 'print',
##            components = list(
##                component=list(type = 'predatorpreyprinter',
##                               predatornames = c('rinimm','rinmat'),
##                               preynames = c('venimm','venmat'),
##                               areaaggfile = 'Aggfiles/catchdistribution.ldist.ven.aco.area.agg',
##                               ageaggfile = "Aggfiles/catchdistribution.alk.ven.aco.age.agg",
##                               lenaggfile = "Aggfiles/surveyindices.si.ven.cpue.len.agg",
##                               printfile = "out/ven_m2_age",
##                               yearsandsteps = c('all','all')),
##                component=list(type = 'predatorpreyprinter',
##                               predatornames = c('comven1','comven2'),
##                               preynames = c('venimm','venmat'),
##                               areaaggfile = 'Aggfiles/catchdistribution.ldist.ven.aco.area.agg',
##                               ageaggfile = "Aggfiles/catchdistribution.alk.ven.aco.age.agg",
##                               lenaggfile = "Aggfiles/surveyindices.si.ven.cpue.len.agg",
##                               printfile = "out/ven_f_age",
##                               yearsandsteps = c('all','all')),
##                component=list(type = 'predatorprinter',
##                               predatornames = c('rinimm','rinmat'),
##                               preynames = c('venimm','venmat','her','other'),
##                               areaaggfile = 'Aggfiles/stomachcontent.stom2.rin.area.agg',
##                               predlenaggfile = 'Aggfiles/rinmat.stock.len.agg',
##                               preylenaggfile = "WGTS/print.aggfiles/venmat.alllen.agg",
##                               biomass = 1,
##                               printfile = "out/rin_consumption.out",
##                               yearsandsteps = c('all','all')))) %>%
##          write.gadget.file('.', recursive=T)

## gadgetfile('printfile.custom',
##            file_type = 'print',
##            components = list(
##                component=list(type = 'predatorprinter',
##                               predatornames = c('rinimm','rinmat'),
##                               preynames = c('venimm','venmat','her','other'),
##                               areaaggfile = 'Aggfiles/stomachcontent.stom2.rin.area.agg',
##                               predlenaggfile = 'Aggfiles/rin.stock.len.agg',
##                               preylenaggfile = "WGTS/print.aggfiles/venmat.alllen.agg",
##                               biomass = 1,
##                               printfile = "out/ven_consumed_rin.out",
##                               yearsandsteps = c('all','all')))) %>%
    ## write.gadget.file('.', recursive=T)
## header <- sprintf("; name\tlower\tupper")
## predLenAgg <- data.frame("name"=paste0("len",4:18*10), "lower"=4:18*10, "upper"=5:19*10)
## preyLenAgg <- data.frame("name"="all", "lower"=1, "upper"=30)

## # make aggfiles predator and prey
## tmp <- tmp1 <- read.gadget.file(".", "Aggfiles/rinimm.stock.len.agg", file_type="generic")
## tmp2 <- read.gadget.file(".", "Aggfiles/rinmat.stock.len.agg", file_type="generic")
## tmp[[1]] <- rbind(tmp1[[1]],tmp2[[1]])
## tmp[[1]] <- unique(tmp[[1]])
## attributes(tmp)$file_name <- "Aggfiles/rin.stock.len.agg"
## tmp %>% write.gadget.file('.')

## tmp <- read.gadget.file(".", "Aggfiles/venmat.stock.len.agg", file_type="generic")
## tmp1 <- tmp[[1]][1,2]
## tmp2 <- tmp[[1]][nrow(tmp[[1]]),3]
## tmp[[1]] <- tmp[[1]][1,]
## tmp[[1]][1,1] <- "ven"
## tmp[[1]][1,2:3] <- c(tmp1,tmp2)
## attributes(tmp)$file_name <- "Aggfiles/ven.allen.agg"
## tmp %>% write.gadget.file('.')

callGadget(s=1, i="WGTS/params.final", main="main")

fAge <- read.table("out/ven_f_age", skip=6, header=F)
colnames(fAge) <- c("year","step","area","age","length","number_consumed_com","biomass_consumed_com","f")
m2Age <- read.table("out/ven_m2_age", skip=6, header=F)
colnames(m2Age) <- c("year","step","area","age","length","number_consumed_seal","biomass_consumed_seal","m2")
m2f <- left_join(fAge,m2Age)

figName <- "Z_partition.ps"
ggplot(m2f %>%
       ## filter(step==4) %>%
       group_by(year,area,age) %>% summarise(m2=mean(m2), f=mean(f)) %>%
       filter(age %in% paste0("age",1:3)) %>%
       mutate(m1 = 0.2) %>%
       gather("source","mortality",c("f","m1","m2")) %>%
       group_by(year,area,source) %>%
       summarise(mortality=mean(mortality))) +
    geom_bar(aes(year,mortality,fill=source), position="stack", stat="identity") + ggsave(paste(dirFigs,figName, sep="/"), device="ps") +
   ggRunName() + ggRunNameSize(6)


tmp <- m2f %>%
    group_by(year,area,age) %>% summarise(m2=mean(m2), f=mean(f)) %>%
    filter(age %in% paste0("age",1:3)) %>%
    mutate(mf = m2 + f) %>%
    group_by(year,area) %>%
    summarise(mf=mean(mf))
ggplot() +
    geom_line(data=ss3.fbar, aes(Yr,fbar), col=2) +
    geom_line(data=tmp, aes(year,mf))

tmp <- m2f %>%
    group_by(year) %>%
    summarise(biomass_consumed_com = sum(biomass_consumed_com),
              biomass_consumed_seal = sum(biomass_consumed_seal)) %>%
    gather("pred","biomass",2:3)
ggplot(tmp) +
    geom_line(aes(year, biomass, col=pred)) +
    ylab("Biomass vendace consumed")


rinNum <- fit$stock.full %>%
    ## filter(stock %in% c("rinmat")) %>%
    filter(stock %in% c("rinimm","rinmat")) %>%
    group_by(year,step,length) %>%
    summarise(number=sum(number))

biomCons <- read.table("out/rin_consumption.out", skip=6, header=F)
colnames(biomCons) <- c("year","step","area","length","prey_length","biomass_consumed")
biomConsAtLen <- biomCons %>%
    select("year","step","length","biomass_consumed") %>%
    separate(length,c("x","length"),3,6) %>%
    mutate(length=as.numeric(length), x=NULL) %>%
    right_join(rinNum) %>%
    filter(year == 2010) %>% # pick one year as example. Unless of understocking they should be the same
    mutate(indCons = biomass_consumed/number) # individual consumption

ggplot(biomConsAtLen) + geom_point(aes(length,indCons)) + facet_wrap(~year)
ggplot(biomConsAtLen) + geom_point(aes(length,number)) + facet_wrap(~year)

biomCons <- biomCons %>%
    group_by(year) %>%
    summarise(biomass_consumed = sum(biomass_consumed))
ggplot(biomCons) +
    geom_line(aes(year, biomass_consumed))

# ----------------------------------------------------------
# check maturity ringed seal
rinNum <- fit$stock.full %>%
    filter(stock %in% c("rinimm","rinmat")) %>%
    select(year,step,area,stock,length,number) %>%
    spread(stock,number,fill=0) %>%
    mutate(rinall = rinimm + rinmat,
           prop = rinmat/rinall)
ggplot(rinNum) + geom_point(aes(length,prop)) + facet_wrap(~year)


rinNum <- fit$stock.full %>%
    filter(stock %in% c("rinimm","rinmat"))
rinNum <- rinNum %>%
    group_by(year,step,area,length) %>%
    summarise(numTot=sum(number)) %>%
    right_join(rinNum)



# Retrospective analysis
gadget.retro(params.file="params.in", main.file="main",
             grouping=list(ind=c('si.ven.cpue','si.ven.aco')),
             cv.floor=0.01,
             num.years=5, iterative=T)

fit1 <- gadget.fit(main="RETRO/WGTS.1/main.final",
                   f.age.range=data.frame(stock=c("venimm","venmat"),age.min=1,age.max=3),
                   fit.folder ="RETRO/WGTS.1", params.file="RETRO/WGTS.1/params.final")
fit2 <- gadget.fit(main="RETRO/WGTS.2/main.final",
                   f.age.range=data.frame(stock=c("venimm","venmat"),age.min=1,age.max=3),
                   fit.folder ="RETRO/WGTS.2", params.file="RETRO/WGTS.2/params.final")
fit3 <- gadget.fit(main="RETRO/WGTS.3/main.final",
                   f.age.range=data.frame(stock=c("venimm","venmat"),age.min=1,age.max=3),
                   fit.folder ="RETRO/WGTS.3", params.file="RETRO/WGTS.3/params.final")
fit4 <- gadget.fit(main="RETRO/WGTS.4/main.final",
                   f.age.range=data.frame(stock=c("venimm","venmat"),age.min=1,age.max=3),
                   fit.folder ="RETRO/WGTS.4", params.file="RETRO/WGTS.4/params.final")
fit5 <- gadget.fit(main="RETRO/WGTS.5/main.final",
                   f.age.range=data.frame(stock=c("venimm","venmat"),age.min=1,age.max=3),
                   fit.folder ="RETRO/WGTS.5", params.file="RETRO/WGTS.5/params.final")
fitL <- bind.gadget.fit(r1=fit1,r2=fit2,r3=fit3,r4=fit4,r5=fit5)

tmp <- fitL$res.by.year %>% filter(stock=="venmat")
ssb.retro <- ggplot(tmp, aes(year,total.biomass,col=model,group=model)) +
             geom_line() + geom_point() +
             ylim(0,max(tmp$total.biomass)) + ylab("SSB") +
             ggRunName() + ggRunNameSize(6)
tmp <- fitL$res.by.year %>% filter(stock=="venimm")
rec.retro <- ggplot(tmp, aes(year,total.number,col=model,group=model)) +
             geom_line() + geom_point() +
             ylim(0,max(tmp$total.number)) + ylab("Number immature") +
             ggRunName() + ggRunNameSize(6)
tmp <- fitL$res.by.year %>% filter(stock=="venmat")
fbar.retro <- ggplot(tmp, aes(year,F,col=model,group=model)) +
              geom_line() + geom_point() +
              ylim(0,max(tmp$F)) + ylab("F13") +
              ggRunName() + ggRunNameSize(6)
cairo_ps(paste(dirFigs,"retro_plot.ps", sep="/"))
  grid.arrange(ssb.retro,rec.retro,fbar.retro,ncol=3)
dev.off()



mohns.rho <- function(...){}


# merge into a PDF
system(paste("psmerge $(ls ",dirFigs,"/*.ps) > ",dirFigs,"/figs_all.ps ; ps2pdf ",dirFigs,"/figs_all.ps ",dirFigs,"/figs_all.pdf ; rm ",dirFigs,"/figs_all.ps",sep=""))

# ----------------------------------------
load("~/../valerio/Share/Gadget/vendace-ringedseal/ven_rin_04/ven-rin02/WGTS/WGTS.Rdata")
fit3 <- out
load("~/../valerio/Share/Gadget/vendace/vendace_13.3/vendace01/WGTS/WGTS.Rdata")
fit2 <- out
load("~/../valerio/Share/Gadget/vendace/vendace_13.2/vendace03/WGTS/WGTS.Rdata")
fit1 <- out


fitL <- bind.gadget.fit("Singlesp"=fit1, "SealAsFleet"=fit2, "Multispp"=fit3)

# Catch
tmp <- fitL$res.by.year %>% filter(model == "Singlesp") %>% group_by(year,area) %>% summarise(catch=sum(catch))
  catch.out <- ggplot(tmp) + geom_bar(aes(year,catch), stat="identity")
# SSB
tmp <- fitL$res.by.year %>% filter(stock=="venmat") %>% filter(model %in% c("Singlesp","Multispp"))
ssb.out <- ggplot(tmp, aes(year,total.biomass,col=model)) + geom_line() + geom_point() +
        ylim(0,max(tmp$total.biomass)) + ylab("SSB")
# Rec
tmp <- fitL$res.by.year %>% filter(stock=="venimm") %>% filter(model %in% c("Singlesp","Multispp"))
rec.out <- ggplot(tmp, aes(year,total.number,col=model)) + geom_line() + geom_point() +
        ylim(0,max(tmp$total.number)) + ylab("Number immature")
# F
tmp <- fitL$res.by.year %>% filter(stock=="venmat") %>% filter(model %in% c("Singlesp","Multispp"))
fbar.out <- ggplot(tmp, aes(year,F,col=model)) + geom_line() + geom_point() +
        ylim(0,max(tmp$F)) + ylab("Fishing mortality")

figName <- "standard_plot_comp_single_multi.ps"
postscript(paste(dirFigs,figName, sep="/"))
  grid.arrange(catch.out,ssb.out,rec.out,fbar.out,ncol=2)
dev.off()
