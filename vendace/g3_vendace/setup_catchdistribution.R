minage <-  ven_imm$iterate %>% environment() %>% .$minage
maxage <-  ven_mat$iterate %>% environment() %>% .$maxage
maxlength <-  ven_mat$iterate %>% environment() %>% .$lengthgroups %>% max()
minlength <-  ven_imm$iterate %>% environment() %>% .$lengthgroups %>% min()
## dl <- ven_imm$iterate %>% environment() %>% .$stock__dl %>% min()
dl <- 1

# -------------------------------------------
# Query commercial catch at age (8+) CANUM:
ll <- mfdb_interval("all",c(minlength,maxlength),
                    open_ended = c("upper","lower"))
names(ll) <- "all"
adist.ven.com <- mfdb_sample_count(mdb, c('age','length'), list(
    area          = "SD3031",
    ## area          = mfdb_group('area1'="SD3031"),
    timestep      = mfdb_timestep_quarterly,
    year          = year_range,
    sampling_type = 'LND',
    data_source   = 'canum_vendace',
    length        = ll,
    age           = mfdb_group('age0'=0,'age1'=1,'age2'=2,'age3'=3,'age4'=4,'age5'=5,'age6'=6,'age7'=7,'age8'=8:10)))[[1]]

adist.ven.com$area <- '1'
names(attributes(adist.ven.com)$area) <- '1'
attributes(adist.ven.com)$area$'1' <- '1'

# -------------------------------------------
# Query seal consumption of vendace by age:
adist.ven.seal <- mfdb_sample_count(mdb, c('age','length'), list(
    area          = "SD3031",
    ## area          = mfdb_group('area1'="SD3031"),
    timestep      = mfdb_timestep_quarterly,
    year          = year_range,
    sampling_type = 'RES',
    data_source   = 'seal_high_agedistribution_vendace',
    length        = ll,
    age           = mfdb_group('age0'=0,'age1'=1,'age2'=2,'age3'=3,'age4'=4,'age5'=5,'age6'=6,'age7'=7,'age8'=8:10)))[[1]]

adist.ven.seal$area <- '1'
names(attributes(adist.ven.seal)$area) <- '1'
attributes(adist.ven.seal)$area$'1' <- '1'

# -------------------------------------------
# Query acoustic number at age (8+):
adist.ven.aco <- mfdb_sample_count(mdb, c('age','length'), list(
    area          = "SD3031",
    ## area          = mfdb_group('area1'="SD3031"),
    timestep      = mfdb_timestep_quarterly,
    year          = 2009:2018,
    sampling_type = 'RES',
    data_source   = 'acoustic_agedistribution_vendace',
    length        = ll,
    age           = mfdb_group('age0'=0,'age1'=1,'age2'=2,'age3'=3,'age4'=4,'age5'=5,'age6'=6,'age7'=7,'age8'=8:10)))[[1]]

adist.ven.aco$area <- '1'
names(attributes(adist.ven.aco)$area) <- '1'
attributes(adist.ven.aco)$area$'1' <- '1'

# -------------------------------------------
# Query commercial length distribution:
ll <- mfdb_interval("len", seq(minlength, maxlength, by = dl))
names(ll) <- paste("len",4:21, sep="")
aa <- mfdb_interval("all",c(minage,maxage), open_ended = c("upper","lower"))
names(aa) <- "all"

ldist.ven.com <- mfdb_sample_count(mdb, c('age','length'), list(
    area          = "SD3031",
    ## area          = mfdb_group('area1'="SD3031"),
    timestep      = mfdb_timestep_quarterly,
    year          = year_range,
    sampling_type = 'LND',
    data_source   = 'lengthdist_comm_vendace',
    length        = ll,
    age           = aa))[[1]]

ldist.ven.com$area <- '1'
names(attributes(ldist.ven.com)$area) <- '1'
attributes(ldist.ven.com)$area$'1' <- '1'

## tmp <- ldist.ven.com
## tmp <- tmp %>%
##        group_by(year) %>%
##        summarise(numberYr=sum(number)) %>%
##        right_join(tmp)
## ggplot(tmp, aes(as.numeric(substring(length,4,6)),number/numberYr)) +
##     geom_line() +
##     facet_wrap(~year) +
##     xlab("Length (cm)")    

# -------------------------------------------
# Query acoustic/pelagic length distribution:
ll <- mfdb_interval("len", seq(minlength, maxlength, by = dl))
names(ll) <- paste("len",4:21, sep="")
aa <- mfdb_interval("all",c(minage,maxage), open_ended = c("upper","lower"))
names(aa) <- "all"

ldist.ven.aco <- mfdb_sample_count(mdb, c('age','length'), list(
    area          = "SD3031",
    ## area          = mfdb_group('area1'="SD3031"),
    timestep      = mfdb_timestep_quarterly,
    year          = c(2009:2012,2014:2018),  #*** what about 2013???
    sampling_type = 'RES',
    data_source   = 'lengthdist_aco_vendace',
    length        = ll,
    age           = aa))[[1]]

ldist.ven.aco$area <- '1'
names(attributes(ldist.ven.aco)$area) <- '1'
attributes(ldist.ven.aco)$area$'1' <- '1'

## tmp <- ldist.ven.aco
## tmp <- tmp %>%
##        group_by(year) %>%
##        summarise(numberYr=sum(number)) %>%
##        right_join(tmp)
## ggplot(tmp, aes(as.numeric(substring(length,4,6)),number/numberYr)) +
##     geom_line() +
##     facet_wrap(~year) +
##     xlab("Length (cm)")    

# -------------------------------------------
# Query ALK commercial fleet
ll <- mfdb_interval("len", seq(minlength, maxlength, by = dl))
names(ll) <- paste("len",4:21, sep="")

alk.ven.com <- mfdb_sample_count(mdb, c('age','length'), list(
    area            = "SD3031",
    timestep        = mfdb_timestep_quarterly,
    year            = year_range,
    sampling_type   = 'LND',
    species         = 'FVE',
    length          = ll,
    age             = mfdb_group('age0'=0,'age1'=1,'age2'=2,'age3'=3,'age4'=4,'age5'=5,'age6'=6,'age7'=7,'age8'=8:10),
    data_source     = 'individual_comm_vendace'))[[1]]

alk.ven.com$area <- '1'
names(attributes(alk.ven.com)$area) <- '1'
attributes(alk.ven.com)$area$'1' <- '1'

## tmp <- alk.ven.com
## tmp$age2 <- as.numeric(substring(tmp$age,4,5))
## tmp$length2 <- as.numeric(substring(tmp$length,4,6))
## ggplot(tmp, aes(age2,length2)) +
##     geom_point(aes(size=number)) +
##     facet_wrap(~year) +
##     xlab("Age") +
##     ylab("Length (cm)")

# -------------------------------------------
# Query ALK pelagic survey
ll <- mfdb_interval("len", seq(minlength, maxlength, by = dl))
names(ll) <- paste("len",4:21, sep="")

alk.ven.aco <- mfdb_sample_count(mdb, c('age','length'), list(
    area            = "SD3031",
    timestep        = mfdb_timestep_quarterly,
    year            = year_range,
    sampling_type   = 'RES',
    species         = 'FVE',
    length          = ll,
    age             = mfdb_group('age0'=0,'age1'=1,'age2'=2,'age3'=3,'age4'=4,'age5'=5,'age6'=6,'age7'=7,'age8'=8:10),
    data_source     = 'individual_aco_vendace'))[[1]]

alk.ven.aco$area <- '1'
names(attributes(alk.ven.aco)$area) <- '1'
attributes(alk.ven.aco)$area$'1' <- '1'

## tmp <- alk.ven.aco
## tmp$age2 <- as.numeric(substring(tmp$age,4,5))
## tmp$length2 <- as.numeric(substring(tmp$length,4,6))
## ggplot(tmp, aes(age2,length2)) +
##     geom_point(aes(size=number)) +
##     facet_wrap(~year) +
##     xlab("Age") +
##     ylab("Length (cm)")

# -------------------------------------------
# Query maturity commercial fleet
ll <- mfdb_interval("len", seq(minlength, maxlength, by = dl))
names(ll) <- paste("len",4:21, sep="")
aa <- mfdb_interval("all",c(minage,maxage), open_ended = c("upper","lower"))
names(aa) <- "all"

mat.ven.com <- mfdb_sample_count(mdb, c('maturity_stage','age','length'), list(
    area            = "SD3031",
    timestep        = mfdb_timestep_quarterly,
    ## year            = 1991:2017,
    year            = c(1998,2006:2008,2010:2018),
    sampling_type   = 'LND',
    species         = 'FVE',
    length          = ll,
    age             = aa,
    maturity_stage  = mfdb_group('ven_imm'=1,'ven_mat'=5),
    data_source     = 'individual_comm_vendace'))[[1]]

mat.ven.com$area <- '1'
names(attributes(mat.ven.com)$area) <- '1'
attributes(mat.ven.com)$area$'1' <- '1'


# fill in and adjust *** needed?
# assume fish smaller than min obs are all immature and fish >15 cm are all mature
lenRange <- range(as.numeric(unique(substring(mat.ven.com$length,4,6))))
tmp <- mat.ven.com %>%
   unite(ii, year,step,area,sep=",") %>%
   select(ii) %>%
   unique()
tmp <- expand.grid(ii=tmp[,1],
                   maturity_stage="ven_imm",
                   age="all",
                   length=paste("len",lenRange[1]-1:3,sep=""),
                   number=1)
tmp <- separate(tmp,ii,c("year","step","area")) %>%
       mutate(year=as.integer(year),
              step=as.character(step),
              area=as.character(area),
              maturity_stage=as.character(maturity_stage),
              age=as.character(age))
mat.ven.com <- rbind(mat.ven.com,tmp)
mat.ven.com <- mat.ven.com %>%
               filter(!(length=="len16" & maturity_stage=="ven_imm")) %>%
               filter(!(length=="len17" & maturity_stage=="ven_imm")) %>%
               filter(!(length=="len18" & maturity_stage=="ven_imm")) %>%
               filter(!(length=="len19" & maturity_stage=="ven_imm")) %>%
               filter(!(length=="len20" & maturity_stage=="ven_imm"))
# fill with zeros
tmp <-  mat.ven.com %>%
        complete(year,step,area,maturity_stage, age, length, fill=list(number=0)) %>%
        filter(number == 0)
tmp <- tmp %>% unite(ii, year,step,area,sep=",")
i1 <- mat.ven.com %>%
      unite(ii, year,step,area,sep=",") %>%
      select(ii)
tmp <- semi_join(tmp,i1,by="ii") %>%
       separate(ii,c("year","step","area")) %>%
       mutate(year=as.integer(year),
              step=as.character(step),
              area=as.character(area),
              maturity_stage=as.character(maturity_stage),
              age=as.character(age))
mat.ven.com <- rbind(mat.ven.com,tmp)

## mat.ven.com[1:4,]
## tmp <- mat.ven.com %>%
##        ## filter(year == 2016) %>%
##        select(year,maturity_stage,length,number) %>%
##        complete(year,maturity_stage,length, fill=list(number=0)) %>%
##        group_by(year,length) %>%
##        summarise(n=sum(number))
## tmp <- mat.ven.com %>%
##        ## filter(year == 2016) %>%
##        select(year,maturity_stage,length,number) %>%
##        complete(year,maturity_stage,length, fill=list(number=0)) %>%
##        left_join(tmp) %>%
##        mutate(p=number/n) %>%
##        filter(maturity_stage == "ven_mat")
## ggplot(tmp) +
##     geom_point(aes(as.numeric(substring(length,4,5)), p)) +
##     geom_vline(xintercept=as.numeric(mat.constants.ven[2]), lty=2) +
##     facet_wrap(~year)

# -------------------------------------------
# Query maturity acoustic/pelagic survey
ll <- mfdb_interval("len", seq(minlength, maxlength, by = dl))
names(ll) <- paste("len",4:21, sep="")
aa <- mfdb_interval("all",c(minage,maxage), open_ended = c("upper","lower"))
names(aa) <- "all"

mat.ven.aco <- mfdb_sample_count(mdb, c('maturity_stage','age','length'), list(
    area            = "SD3031",
    timestep        = mfdb_timestep_quarterly,
    year            = 2009:2018,
    ## year            = c(2010:2014),
    sampling_type   = 'RES',
    species         = 'FVE',
    length          = ll,
    age             = aa,
    maturity_stage  = mfdb_group('ven_imm'=1,'ven_mat'=5),
    data_source     = 'individual_aco_vendace'))[[1]]

mat.ven.aco$area <- '1'
names(attributes(mat.ven.aco)$area) <- '1'
attributes(mat.ven.aco)$area$'1' <- '1'

# fill in and adjust *** needed?
# assume fish smaller than min obs are all immature and fish >15 cm are all mature
lenRange <- range(as.numeric(unique(substring(mat.ven.aco$length,4,6))))
tmp <- mat.ven.aco %>%
   unite(ii, year,step,area,sep=",") %>%
   select(ii) %>%
   unique()
tmp <- expand.grid(ii=tmp[,1],
                   maturity_stage="ven_imm",
                   age="all",
                   length=paste("len",lenRange[1]-1:3,sep=""),
                   number=1)
tmp <- separate(tmp,ii,c("year","step","area")) %>%
       mutate(year=as.integer(year),
              step=as.character(step),
              area=as.character(area),
              maturity_stage=as.character(maturity_stage),
              age=as.character(age))
mat.ven.aco <- rbind(mat.ven.aco,tmp)
mat.ven.aco <- mat.ven.aco %>%
               filter(!(length=="len16" & maturity_stage=="ven_imm")) %>%
               filter(!(length=="len17" & maturity_stage=="ven_imm")) %>%
               filter(!(length=="len18" & maturity_stage=="ven_imm")) %>%
               filter(!(length=="len19" & maturity_stage=="ven_imm")) %>%
               filter(!(length=="len20" & maturity_stage=="ven_imm"))
# fill with zeros
tmp <-  mat.ven.aco %>%
        complete(year,step,area,maturity_stage, age, length, fill=list(number=0)) %>%
        filter(number == 0)
tmp <- tmp %>% unite(ii, year,step,area,sep=",")
i1 <- mat.ven.aco %>%
      unite(ii, year,step,area,sep=",") %>%
      select(ii)
tmp <- semi_join(tmp,i1,by="ii") %>%
       separate(ii,c("year","step","area")) %>%
       mutate(year=as.integer(year),
              step=as.character(step),
              area=as.character(area),
              maturity_stage=as.character(maturity_stage),
              age=as.character(age))
mat.ven.aco <- rbind(mat.ven.aco,tmp)

## mat.ven.aco[1:4,]
## tmp <- mat.ven.aco %>%
##        ## filter(year == 2016) %>%
##        select(year,maturity_stage,length,number) %>%
##        complete(year,maturity_stage,length, fill=list(number=0)) %>%
##        group_by(year,length) %>%
##        summarise(n=sum(number))
## tmp <- mat.ven.aco %>%
##        ## filter(year == 2016) %>%
##        select(year,maturity_stage,length,number) %>%
##        complete(year,maturity_stage,length, fill=list(number=0)) %>%
##        left_join(tmp) %>%
##        mutate(p=number/n) %>%
##        filter(maturity_stage == "ven_mat")
## ggplot(tmp) +
##     geom_point(aes(as.numeric(substring(length,4,5)), p)) +
##     geom_vline(xintercept=as.numeric(mat.constants.ven$l50), lty=2) +
##     facet_wrap(~year)
