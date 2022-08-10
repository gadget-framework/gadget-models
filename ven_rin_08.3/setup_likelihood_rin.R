
gadgetlikelihood('likelihood.rin',gd$dir,missingOkay = TRUE) %>% 
  gadget_update("catchdistribution",
                name = "alk.rin",
                weight = 1,
                data = alk.rin,
                aggregationlevel = 0,
                fleetnames = "hunt",
                stocknames = stock_names) %>%
  gadget_update("surveyindices",
                name = "si.rin",
                weight = 1,
                data = si.rin,
                sitype = "acoustic",
                biomass = 0,
                ## fittype = 'fixedlinearfit',
                fittype = 'fixedloglinearfit',
                slope = 1,
                intercept = 0,
                surveynames = "aco.rin",
                stocknames = stock_names) %>%
  gadget_update("stockdistribution",
                name = "mat.rin",
                weight = 1,
                data = mat.rin,
                fleetnames = c("hunt"),
                stocknames = stock_names) %>%

  gadget_update("stomachcontent",
                name = "stom1.rin",
                weight = 1,
                data_function = "scsimple",
                data = stom1.rin,
                predator_names = c("rinimm","rinmat"),
                prey_labels = list("ven3.5"=c("venimm","venmat"),
                                   "her1"="her",
                                   "other1"="other"),
                prey_digestion_coefficients = list(c(1,0,0))) %>%

  gadget_update("stomachcontent",
                name = "stom2.rin",
                weight = 1,
                data_function = "scsimple",
                data = stom2.rin,
                predator_names = c("rinimm","rinmat"),
                prey_labels = list(len8.5=c("venimm","venmat"),
                                   len9.5=c("venimm","venmat"),
                                   len10.5=c("venimm","venmat"),
                                   len11.5=c("venimm","venmat"),
                                   len12.5=c("venimm","venmat"),
                                   len13.5=c("venimm","venmat"),
                                   len14.5=c("venimm","venmat"),
                                   len15.5=c("venimm","venmat"),
                                   len16.5="venmat",
                                   len17.5="venmat",
                                   len18.5="venmat"),
              prey_digestion_coefficients = list(c(1,0,0))) %>%

  write.gadget.file(gd$dir)
