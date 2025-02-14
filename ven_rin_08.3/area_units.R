# load ICES rect and subd from file as those imported in mfdb
squares <- read.table("~/../valerio/Share/PROGETTI/MareFRAME_FP7/WP3/MFDB/ICES_RECT.csv", header=T, sep=";")
squares <- squares[,c("SD","ICES_Rectangle","Area")]
tmp <- aggregate(squares$Area, list("rect"=squares$ICES_Rectangle), sum)
squares <- squares[order(squares[,"ICES_Rectangle"], squares[,"Area"], decreasing=T),]
squares <- squares[!duplicated(squares[,"ICES_Rectangle"]),]
squares$Area <- tmp$x[match(squares$ICES_Rectangle, tmp$rect)]
squares <- squares[squares$SD>=22,]

subdiv <- aggregate(squares$Area, list(sub=squares$SD), sum)

print("...ICES rectangles and subdivisions loaded")
