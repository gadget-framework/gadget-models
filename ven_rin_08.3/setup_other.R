otherfood <- 
  gadgetfile('Modelfiles/otherfood',
             file_type = 'otherfood',
             components = list(
                 component=list(foodname = 'other',
                                livesonareas = 1,
                                lengths = list(min = 1, max = 30),
                                energycontent = 1,
                                amount = gadgetfile('Data/otherfood',
                                                    components = list(
                                                        component = area %>%
                                                                    select(year,step,area) %>%
                                                                    mutate(prey="other",
                                                                           area=1,
                                                                           amount=1e8))))))

otherfood %>% 
  write.gadget.file(gd$dir, recursive=T)

# -----------------
# load TSB (SD30-31) from GoB_Herring_2019newsurvey at www.stockassessment.org
her.biom <- read.table("herring.biomass",skip=2, header=F)
colnames(her.biom) <- c("year","step","area","prey","amount")
# add seal-herring overlapping factor (only part of the herring pop is available to ringed seal)
her.biom <- her.biom %>%
            ## mutate(amount=paste("(* #over.rinher ",amount,")",sep=""))
            mutate(amount=ifelse(step==1, paste("(* #over1.rinher ",amount,")",sep=""),
                          ifelse(step==2, paste("(* #over2.rinher ",amount,")",sep=""),
                          ifelse(step==3, paste("(* #over3.rinher ",amount,")",sep=""),
                          ifelse(step==4, paste("(* #over4.rinher ",amount,")",sep=""),NA)))))
herring <- 
  gadgetfile('Modelfiles/herring',
             file_type = 'otherfood',
             components = list(
                 component=list(foodname = 'her',
                                livesonareas = 1,
                                lengths = list(min = 1, max = 25),
                                energycontent = 1,
                                amount = gadgetfile('Data/herring',
                                                    components = list(
                                                        ## component = area %>%
                                                        ##             select(year,step,area) %>%
                                                        ##             mutate(prey="her",
                                                        ##                    area=1,
                                                        ##                    amount=1e4))))))
                                                        component = her.biom)))))

herring %>% 
  write.gadget.file(gd$dir)
