#devtools::install_github('hafro/rgadget')
library(Rgadget)
tmp<-callGadget(s=1,main='main',i='params.in',o='lik.out',ignore.stderr=FALSE, log='tmp')
lik <- read.gadget.likelihood('likelihood')
tmp<-gadget.iterative(params.file = 'params.in',
                      optinfofile = 'optfile',
                      wgts = 'WGTS',
                      rew.sI=TRUE,
                      cv.floor=0.05,
                      grouping = list(sind=lik$surveyindices$name, seine = c('ldist.seine','ldist.alkseine')))
fit<-gadget.fit(wgts = "WGTS", printfile.printatstart = 0, printfile.steps = 2)
#fit<-gadget.fit(wgts = "WGTS")
st <- Rgadget:::read.gadget.stockfiles('Modelfiles/anch')
params <- read.gadget.parameters('WGTS/params.final')
rec <- Rgadget:::get.gadget.recruitment(st,params,collapse=FALSE)
SS<-read.gadget.lik.out('WGTS/lik.final')
lik <- read.gadget.likelihood('likelihood')
lik.data<-read.gadget.data(lik)
save(params,rec,st,lik.data,SS,file="Output.Rdata")
source("gadget_fit4_2step_2019.r")

#Pasar al repositorio gadget-recovery correr en el cesga en el folder correspondiente#############################################################################33
library(icesTAF)
library(stringr)

path_results<-"/mnt/netapp2/Home_FT2/home/csic/epc/mrh/recovery-results-2020/"
#path_results<-"/run/user/1000/gvfs/sftp:host=ft3.cesga.es,user= csepcmrh/mnt/netapp2/Home_FT2/home/csic/epc/mrh/Gadget_2022/recovery-results-2020"
#sftp://csepcmrh@ft3.cesga.es/home/csic/epc/mrh/Gadget_2022
mkdir(paste(path_results,str_extract(getwd(),"\\w+$"),sep=""))
file2<-c("Output.Rdata","resbyyear.Rdata", "optfile","params.in")
path_run<-getwd()
file.copy(paste(path_run,"/WGTS/WGTS.Rdata",sep=""),paste(path_results,str_extract(getwd(),"\\w+$"),sep=""))
file.copy(paste(path_run,"/",file2[1],sep=""),paste(path_results,str_extract(getwd(),"\\w+$"),sep=""))
file.copy(paste(path_run,"/",file2[2],sep=""),paste(path_results,str_extract(getwd(),"\\w+$"),sep=""))
file.copy(paste(path_run,"/",file2[3],sep=""),paste(path_results,str_extract(getwd(),"\\w+$"),sep=""))
file.copy(paste(path_run,"/",file2[4],sep=""),paste(path_results,str_extract(getwd(),"\\w+$"),sep=""))




















#fit<-gadget.fit(wgts = "WGTS_end", printfile.printatstart = 0, printfile.steps = 2)


#setwd("WGTS")
#F12<-data.frame(stock="anch",age.min=1,age.max=2)
#F1<-data.frame(stock="anch",age.min=1,age.max=1)
#F2<-data.frame(stock="anch",age.min=2,age.max=2)
#Files2copy<-list.files()
#list.files<-Files2copy[1:(length(Files2copy)-1)]

#dir.create("../WGTS1")
#dir.create("../WGTS2")
#dir.create("../WGTS12")
#dir.create("../WGTSseine")
#dir.create("../WGTSpelago")
#dir.create("../WGTSecocadiz")

#HIce WGTS1 a mano en el 44porque necesita casi todos los archivos
#file.copy(from=list.files, to = "../WGTS1")
#file.copy(from=list.files, to = "../WGTS2")
# file.copy(from=list.files, to = "../WGTS12")
# file.copy(from=list.files, to = "../WGTSseine")
# file.copy(from=list.files, to = "../WGTSpelago")
# file.copy(from=list.files, to = "../WGTSecocadiz")

#setwd("..")
#fit1 <- gadget.fit(wgts = 'WGTS1', 
                  # f.age.range=F1)

#fit2 <- gadget.fit(wgts = 'WGTS2', 
                  # f.age.range=F2)
#F12<-data.frame(stock="anch",age.min=1,age.max=2)
#fit12 <- gadget.fit(wgts = 'WGTS12', 
                    #f.age.range=F12)





#retro<-gadget.retro(mainfile = 'WGTS/main.final', iterative=TRUE)
