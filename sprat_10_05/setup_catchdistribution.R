minage <- spr.imm[[1]]$minage
maxage <- spr.mat[[1]]$maxage
maxlength <- spr.mat[[1]]$maxlength 
minlength <- spr.imm[[1]]$minlength
dl <- 1

# ---------------------------------------------------------------------
# Query commercial catch at age (8+) CANUM:
ll <- mfdb_interval("all",c(minlength,maxlength),
                    open_ended = c("upper","lower"))
names(ll) <- "all"

canumANDwecaSpr1 <- mfdb_sample_count(mdb, c('age','length'), list(
    area            = "SD2232",
    timestep        = mfdb_timestep_quarterly,
    year            = 1974:1994,
    sampling_type = 'LND',
    data_source   = 'canumANDweca_spr_byQ_1974-2010',
    length        = ll,
    age           = mfdb_group('age1'=1,'age2'=2,'age3'=3,'age4'=4,'age5'=5,'age6'=6,'age7'=7,'age8'=8:10)))[[1]]

canumANDwecaSpr2 <- mfdb_sample_count(mdb, c('age','length'), list(
    area            = "SD2232",
    timestep        = mfdb_timestep_quarterly,
    year            = 1995:2018,
    sampling_type = 'LND',
    data_source   = 'canumANDweca_spr_byQ_1995-2018',
    length        = ll,
    age           = mfdb_group('age1'=1,'age2'=2,'age3'=3,'age4'=4,'age5'=5,'age6'=6,'age7'=7,'age8'=8:10)))[[1]]

adist.spr.com <- rbind(canumANDwecaSpr1,canumANDwecaSpr2)
adist.spr.com[1:3,]

adist.spr.com$area <- "area1"
attributes(adist.spr.com)$area <- "area1"
names(attributes(adist.spr.com)$area) <- "area1"

## ggplot(filter(adist.spr.com,year %in% 1993:2002)) +
##     geom_bar(aes(age,number), stat="identity") +
##     facet_grid(step~year)

# ---------------------------------------------------------------------
# length distribution BIAS:
agg <- list(year=c(1992,1994:1996,1998:2018),
            ## year            = c(1995,1996,1998:2001,2003:2018), # more restrictive
            area=mfdb_group('area1'= as.character(squares[squares$SD %in% c("24","25","26","27","28.2","29"),"ICES_Rectangle"])),
            length=mfdb_interval("len", seq(4, 18, by = 1)))

aa <- mfdb_interval("all",c(minage,maxage), open_ended = c("upper","lower"))
names(aa) <- "all"

ldist.spr.bias <- mfdb_sample_count(mdb, c('age','length'), list(
    area            = agg$area,
    timestep        = mfdb_timestep_quarterly,
    year            = agg$year,
    species         = mfdb_group(species='SPR'), 
    length          = agg$length,
    age             = aa,
    sampling_type   = 'RES',
    data_source     = 'pelagic_biasHL_spr'), scale_index="acoustic_bias_rect_spr")[[1]]

## ggplot(ldist.spr.bias) +
##      geom_line(aes(as.numeric(substring(length,4,5)),number)) +
##      facet_wrap(~year)

# ---------------------------------------------------------------------
# Query ALK pelagic survey
agg <- list(year=c(1992,1994:1996,1998:2011,2014:2018),
            ## year            = c(1998:2001,2003,2004,2006:2011,2014:2018), # more restrictive
            area=mfdb_group('area1'= as.character(squares[squares$SD %in% c("24","25","26","27","28.2","29"),"ICES_Rectangle"])),
            length=mfdb_interval("len", seq(4, 18, by = 1)),
            age=mfdb_group('age0'=0,'age1'=1,'age2'=2,'age3'=3,'age4'=4,'age5'=5,'age6'=6,'age7'=7,'age8'=8,'age9'=9,'age10'=10))

alk.spr.bias <- mfdb_sample_count(mdb, c('age','length'), list(
    area            = agg$area,
    timestep        = mfdb_timestep_quarterly,
    year            = agg$year,
    species         = 'SPR', 
    length          = agg$length,
    age             = agg$age,
    sampling_type   = 'RES',
    data_source     = 'pelagic_biasCA_spr'), scale_index="acoustic_bias_rect_spr")[[1]]

## ggplot(alk.spr.bias) +
##     geom_point(aes(age,len,size=number)) +
##     facet_wrap(~year)

attributes(alk.spr.bias)$area$area1 <- "area1"

# ---------------------------------------------------------------------
