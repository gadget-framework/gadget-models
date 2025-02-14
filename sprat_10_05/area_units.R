# load ICES rect and subd from file as those imported in mfdb
squaresAll <- read.table("~/../valerio/Share/PROGETTI/MareFRAME_FP7/WP3/MFDB/ICES_RECT.csv", header=T, sep=";")

# retain only the Baltic SD22-32
squares <- squaresAll %>%
           select(SD,ICES_Rectangle,Area) %>%
           filter(SD %in% 22:32) %>%
           group_by(SD,ICES_Rectangle) %>%
           summarise(Area=sum(Area)) %>%
           arrange(desc(Area)) %>% # order by decreasing area
           data.frame()

# retain only the first occurrence (with the largest area)
squares <- squares[!duplicated(squares[,"ICES_Rectangle"]),]

# separate SD28.1 and SD28.2
squares <- squares %>%
           select(-Area) %>%
           mutate(SD=as.character(SD),
                  SD=ifelse(ICES_Rectangle %in% c("43H3","43H4","44H2","44H3","44H4","45H2","45H3","45H4"), "28.1", SD),
                  SD=ifelse(SD==28,"28.2",SD))

print("...ICES rectangles and subdivisions loaded")
