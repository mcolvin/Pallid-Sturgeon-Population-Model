###########################################################
#
#  THIS SCRIPT CREATES THE DISCRETE LENGTH DISTRIBUTIONS
#   USED TO INITIALIZE THE POPULATION MODEL
# 
#  LAST UPDATED: 12/14/2018
#
##########################################################
library(RODBC)
library(plyr)
library(reshape2)
library(distr)

db_location<-"C:/Users/sreynolds/Desktop/Age_Fish/dat/"
pspapcom<- odbcConnectAccess2007(paste0(db_location, "pspap/20181002-pspa_2018.mdb"))
### PULL LENGTH DATA
dat<-sqlFetch(pspapcom,"FISH_CONDITION")
dat<-subset(dat,SPECIES=="PDSG")
dat$BASIN<-ifelse(dat$SEGMENT_ID %in% 2:4, "upper",
                  ifelse(dat$SEGMENT_ID %in% c(7:10,13,14), "lower",
                         "other"))
#DO WE WANT TO INCLUDE TRIBUTARIES?:
  #21 (Milk), 22 (Yellowstone), 52 (Sakakawea) in Upper
  #11 (Kansas), 28 (Osage), 27 (Grand) in Lower
  #5,6,23(Niobrara),24(James) in Inter-Reservoir
  #1 in ????
dat$SOURCE<-"PSPAP"
odbcClose(pspapcom)
rm(pspapcom)
## HAMP DATA
hampcom<- odbcConnectAccess2007(paste0(db_location, "hamp/hamp_2018.mdb"))
### PULL LENGTH DATA
hampdat<-sqlFetch(hampcom,"FISH")
hampdat<-subset(hampdat,SPECIES=="PDSG")
names(hampdat)[15:17]<-c("LENGTH", "WEIGHT", "FTNUM") 
hampdat$BASIN<-ifelse(hampdat$SEGMENT_ID %in% 2:4, "upper",
                  ifelse(hampdat$SEGMENT_ID %in% c(7:10,13,14), "lower",
                         "other"))
hampdat$SOURCE<-"HAMP"
### ADD HAMP DATA TO PSPAP LENGTH DATA
# intersect(hampdat$F_ID, pspapdat$F_ID) #NO F_ID OVERLAP
# intersect(hampdat$MR_ID, pspapdat$UNIQUEID) #NO EFFORT ID OVERLAPS
names(dat)[3]<-"MR_ID"
dat<-rbind.fill(dat,hampdat)
odbcClose(hampcom)  
rm(hampdat,hampcom)
## REMOVE DATA WITHOUT KNOWN LENGTH OR KNOWN ORIGIN
dat<-dat[which(!is.na(dat$LENGTH)),]
dat<-dat[-which(dat$LENGTH==0),]
dat[which(is.na(dat$HATCHERY_ORIGIN)),]$HATCHERY_ORIGIN<-"U"
dat<-dat[-which(dat$HATCHERY_ORIGIN=="U"),]
## USE RECENT DATA 2016-2018 
#### USING LENGTHS ACROSS THE YEARS WOULD ASSUME A STABLE 
#### SIZE DISTRIBUTION
dat<-dat[which(dat$YEAR %in% c(2016, 2017, 2018)),]

## LOOK AT BASIN SAMPLE SIZES
dat$freq<-1
tblB<-aggregate(freq~BASIN+HATCHERY_ORIGIN, dat, sum)
tblB<-tblB[which(tblB$BASIN %in% c("lower", "upper")),]
tblB<-tblB[order(tblB$HATCHERY_ORIGIN),]
tblB
## BY BASIN
quantiles<- seq(0,100,by=1)/100
### UPPER
#### HATCHERY
q<-quantile(dat[which(dat$BASIN=="upper" & 
                        dat$HATCHERY_ORIGIN=="H"),]$LENGTH, 
              quantiles,
              na.rm = TRUE)
out_H_upp_nospace<- data.frame(basin="upper", origin="H",
                               quantile=quantiles,val=q)
out_H_upp_nospace<-out_H_upp_nospace[order(out_H_upp_nospace$quantile),]
len_ini_upp_hatchery_nospace<-approxfun(out_H_upp_nospace$quantile,
                                        out_H_upp_nospace$val,
                                        rule=2)
## INVERSE CUMULATIVE DISTRIBUTION
#### NATURAL
q<-quantile(dat[which(dat$BASIN=="upper" & 
                        dat$HATCHERY_ORIGIN=="W"),]$LENGTH, 
            quantiles,
            na.rm = TRUE)
out_N_upp_nospace<- data.frame(basin="upper", origin="W",
                               quantile=quantiles,val=q)
out_N_upp_nospace<-out_N_upp_nospace[order(out_N_upp_nospace$quantile),]
len_ini_upp_natural_nospace<-approxfun(out_N_upp_nospace$quantile,
                                       out_N_upp_nospace$val,
                                       rule=2)
### LOWER
#### HATCHERY
q<-quantile(dat[which(dat$BASIN=="lower" & 
                        dat$HATCHERY_ORIGIN=="H"),]$LENGTH, 
            quantiles,
            na.rm = TRUE)
out_H_low_nospace<- data.frame(basin="lower", origin="H",
                               quantile=quantiles,val=q)
out_H_low_nospace<-out_H_low_nospace[order(out_H_low_nospace$quantile),]
len_ini_low_hatchery_nospace<-approxfun(out_H_low_nospace$quantile,
                                        out_H_low_nospace$val,
                                        rule=2)
#### NATURAL
q<-quantile(dat[which(dat$BASIN=="lower" & 
                        dat$HATCHERY_ORIGIN=="W"),]$LENGTH, 
            quantiles,
            na.rm = TRUE)
out_N_low_nospace<- data.frame(basin="lower", origin="W",
                               quantile=quantiles,val=q)
out_N_low_nospace<-out_N_low_nospace[order(out_N_low_nospace$quantile),]
len_ini_low_natural_nospace<-approxfun(out_N_low_nospace$quantile,
                                       out_N_low_nospace$val,
                                       rule=2)

# ## LOOK AT SEGMENT SAMPLE SIZES
# tbl<-aggregate(freq~SEGMENT_ID+HATCHERY_ORIGIN, dat, sum)
# tbl<-tbl[which(tbl$SEGMENT_ID %in% c(2:4,7:10,13,14)),]
# tbl<-tbl[order(tbl$HATCHERY_ORIGIN),]
# tbl
# ### NATURAL FISH BY SEGMENT WITH 10, 13, & 14 COMBINED
# dat$seg_id<-ifelse(dat$SEGMENT_ID %in% c(10,13,14), 10, dat$SEGMENT_ID)
# out_H_spatial<-data.frame()
# quantiles<- seq(0,100,by=1)/100
# for(seg in c(2:4,7:10))
# {
#   q<-quantile(dat[which(dat$seg_id==seg & 
#                           dat$HATCHERY_ORIGIN=="H"),]$LENGTH, 
#               quantiles,
#               na.rm = TRUE)
#   app<- data.frame(segment=seg,quantile=quantiles,val=q)
#   out_H_spatial<- rbind(out_H_spatial,app)
# }
# out_H_spatial<-out_H_spatial[order(out_H_spatial$segment, 
#                                    out_H_spatial$quantile),]
# 
# 
# ### FOR WILD FISH:
# #### UPPER BASIN: 2-4
# #### LOWER BASIN: ENITRE BASIN OR 3 GROUPS?:
# ####    # 7 & 8 (17)
# ####    # 9 (71)
# ####    # 10, 13, & 14 (8)
# dat[dat$SEGMENT_ID==8,]$seg_id <- 7
# dat[dat$SEGMENT_ID %in% 2:4,]$seg_id <- 4
# 
# out_N_spatial<-data.frame()
# quantiles<- seq(0,100,by=1)/100
# for(seg in c(4,7,9,10))
# {
#   q<-quantile(dat[which(dat$seg_id==seg & 
#                           dat$HATCHERY_ORIGIN=="W"),]$LENGTH, 
#               quantiles,
#               na.rm = TRUE)
#   app<- data.frame(segment=seg,quantile=quantiles,val=q)
#   out_N_spatial<- rbind(out_N_spatial,app)
# }
# out_N_spatial<-out_N_spatial[order(out_N_spatial$segment, 
#                                    out_N_spatial$quantile),]

rm(dat, out_H_low_nospace, out_H_upp_nospace, out_N_low_nospace,
   out_N_upp_nospace, tblB, q, quantiles, db_location)


