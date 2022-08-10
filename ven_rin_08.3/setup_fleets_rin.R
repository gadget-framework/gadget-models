# ---------------------------------------------------------------------
# make faked hunting, culling, gillnet catch files
hunt.catch.rin <-
  structure(rbind(expand.grid(year=1991:2020,step=1:4,area=1,fleet="hunt",number=1)),
            area_group = mfdb_group(`1` = 1))

# make faked acoustic survey (for acoustic index LH)
aco.catch.rin <-
  structure(rbind(expand.grid(year=1991:2020,step=1,area=1,fleet="aco.rin",number=1)),
            area_group = mfdb_group(`1` = 1))

# ---------------------------------------------------------------------
## write to file
tmp <- gadgetfleet('Modelfiles/fleet',gd$dir,missingOkay = TRUE) %>%
# hunting
  gadget_update('numberfleet',
                name = 'hunt',
                suitability =
                paste0('\n',
                         paste(c('rinimm','rinmat'),
                               ## 'function','exponentiall50',
                               ## '#rin.hun.alpha','#rin.hun.l50',
                               'function','constant',
                               '#rin.hun.alpha',
                               collapse='\n')),
                data = hunt.catch.rin) %>%
# faked acoustic
  gadget_update('numberfleet',
                name = 'aco.rin',
                suitability =
                paste0('\n',
                         paste(c('rinimm','rinmat'),
                               'function','constant',
                               '#rin.aco.alpha',
                               collapse='\n')),
                data = aco.catch.rin) ## %>%

write.gadget.file(tmp, gd$dir)
