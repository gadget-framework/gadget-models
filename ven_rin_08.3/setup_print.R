# ---------------------------------------------------------------------
# Set custom printfile for predation mortality

gadgetfile('printfile.custom',
           file_type = 'print',
           components = list(
               component=list(type = 'predatorpreyprinter',
                              predatornames = c('rinimm','rinmat'),
                              preynames = c('venimm','venmat'),
                              areaaggfile = 'Aggfiles/catchdistribution.ldist.ven.aco.area.agg',
                              ageaggfile = "Aggfiles/catchdistribution.alk.ven.aco.age.agg",
                              lenaggfile = "Aggfiles/surveyindices.si.ven.cpue.len.agg",
                              printfile = "out/ven_m2_age",
                              yearsandsteps = c('all','all')),
               component=list(type = 'predatorpreyprinter',
                              predatornames = c('comven1','comven2'),
                              preynames = c('venimm','venmat'),
                              areaaggfile = 'Aggfiles/catchdistribution.ldist.ven.aco.area.agg',
                              ageaggfile = "Aggfiles/catchdistribution.alk.ven.aco.age.agg",
                              lenaggfile = "Aggfiles/surveyindices.si.ven.cpue.len.agg",
                              printfile = "out/ven_f_age",
                              yearsandsteps = c('all','all')),
               component=list(type = 'predatorprinter',
                              predatornames = c('rinimm','rinmat'),
                              preynames = c('venimm','venmat','her','other'),
                              areaaggfile = 'Aggfiles/stomachcontent.stom2.rin.area.agg',
                              predlenaggfile = 'Aggfiles/rin.stock.len.agg',
                              preylenaggfile = "WGTS/print.aggfiles/venmat.alllen.agg",
                              biomass = 1,
                              printfile = "out/rin_consumption.out",
                              yearsandsteps = c('all','all')))) %>%
         write.gadget.file(gd$dir, recursive=T)

# make some extra aggfiles predator and prey
tmp <- tmp1 <- read.gadget.file(gd$dir, "Aggfiles/rinimm.stock.len.agg", file_type="generic")
tmp2 <- read.gadget.file(gd$dir, "Aggfiles/rinmat.stock.len.agg", file_type="generic")
tmp[[1]] <- rbind(tmp1[[1]],tmp2[[1]])
tmp[[1]] <- unique(tmp[[1]])
attributes(tmp)$file_name <- "Aggfiles/rin.stock.len.agg"
tmp %>% write.gadget.file(gd$dir)
