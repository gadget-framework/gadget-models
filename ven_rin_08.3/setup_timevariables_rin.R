# set timevariable files for ringed seal

## manipulate the area file to include time variable seal consumption
# - spatial overlap with vendace 60-80%? or 50-70%
# - seasonal pattern in consumption from Kunnasranta et al 1999 (fig 1) 
over.rin2ven <- 0.7
## Cseas <- c(1,0.55,0.66,1)
Cseas <- rep(1,4)
area <- NULL
for(i in 1:4){
    area <- rbind(area,
                 expand.grid(year=year_range,
                             step=i,
                             area="area1",
                             mean=log(over.rin2ven*Cseas[i])))
}
area <- arrange(area,year,step)

gadget_areafile2(
  size = mfdb_area_size(mdb, defaults.ven)[[1]],
  ## temperature = mfdb_temperature(mdb, defaults.ven)[[1]]) %>% 
  temperature = area) %>% 
gadget_dir_write(gd,.)


# seal seasonal preference vendace
tmp <- expand.grid("year"=year_range, "step"=1:4) %>%
       mutate("value"=ifelse(step %in% 1:2, '(* 1 #p2rin2ven1)', '(* 1 #p2rin2ven2)')) %>%
       arrange(year, step) %>% rename(";year"=year)
tmp <- gadgetfile(file_name="Modelfiles/rin.ven.sel",
                  file_type="timevariable",
                  components=list(
                      list(
                          "p2rin2ven",
                                  data = tmp)))
tmp %>% 
    write.gadget.file(gd$dir)
# seal seasonal preference herring
tmp <- expand.grid("year"=year_range, "step"=1:4) %>%
       mutate("value"=ifelse(step %in% 1:2, '(* 1 #p0rin2her1)', '(* 1 #p0rin2her2)')) %>%
       arrange(year, step) %>% rename(";year"=year)
tmp <- gadgetfile(file_name="Modelfiles/rin.her.sel",
                  file_type="timevariable",
                  components=list(
                      list(
                          "p0rin2her",
                                  data = tmp)))
tmp %>% 
  write.gadget.file(gd$dir)
# seal seasonal preference otherfood
tmp <- expand.grid("year"=year_range, "step"=1:4) %>%
       mutate("value"=ifelse(step %in% 1:2, '(* 1 #p0rin2oth1)', '(* 1 #p0rin2oth2)')) %>%
       arrange(year, step) %>% rename(";year"=year)
tmp <- gadgetfile(file_name="Modelfiles/rin.oth.sel",
                  file_type="timevariable",
                  components=list(
                             list("p0rin2oth",
                                  data = tmp)))
tmp %>% 
  write.gadget.file(gd$dir)
