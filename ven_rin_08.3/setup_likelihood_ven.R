
gadgetlikelihood('likelihood.ven',gd$dir,missingOkay = TRUE) %>% 
  ## Write a penalty component to the likelihood file
  gadget_update("penalty",
                name = "bounds",
                weight = "10",
                data = data.frame(
                  switch = c("default"),
                  power = c(2),
                  upperW=10000,
                  lowerW=10000,
                  stringsAsFactors = FALSE)) %>%
  gadget_update("understocking",
                name = "understocking",
                weight = "10") %>% #
  gadget_update("catchdistribution",
                name = "ldist.ven.com",
                weight = 1,
                data = ldist.ven.com,
                fleetnames = c("comven1","comven2"),
                stocknames = stock_names) %>%
  gadget_update("catchdistribution",
                name = "alk.ven.com",
                weight = 1,
                data = alk.ven.com,
                fleetnames = c("comven1","comven2"),
                stocknames = stock_names) %>%
  gadget_update("catchdistribution",
                name = "ldist.ven.aco",
                weight = 1,
                data = ldist.ven.aco,
                fleetnames = "aco",
                stocknames = stock_names) %>%
  gadget_update("catchdistribution",
                name = "alk.ven.aco",
                weight = 1,
                data = alk.ven.aco,
                fleetnames = "aco",
                stocknames = stock_names) %>%
                    
  gadget_update("surveyindices",
                name = "si.ven.cpue",
                weight = 1,
                data = si.ven.cpue,
                sitype = "fleets", # to account for selectivity
                biomass = 1,
                fittype = 'fixedslopeloglinearfit',
                slope = 1,
                fleetnames = c("comven1","comven2"),
                stocknames = stock_names) %>%
  gadget_update("surveyindices",
                name = "si.ven.aco",
                weight = 1,
                data = si.ven.aco,
                sitype = "ages",
                biomass = 0,
                fittype = 'fixedslopeloglinearfit',
                slope = 1,
                stocknames = stock_names) %>%
  gadget_update("stockdistribution",
                name = "mat.ven.aco",
                weight = 1,
                data = mat.ven.aco,
                fleetnames = "aco",
                stocknames = stock_names) %>%
                    
  write.gadget.file(gd$dir)
