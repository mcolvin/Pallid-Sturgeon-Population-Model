###########################################################
#
#  THIS SCRIPT CREATES THE DISCRETE LENGTH DISTRIBUTIONS
#   USED TO INITIALIZE THE POPULATION MODEL
# 
#  LAST UPDATED: 11/13/2018
#
##########################################################
library(RODBC)
library(plyr)

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
## REMOVE DATA WITHOUT LENGTH OR KNOWN ORIGIN
dat<-dat[which(!is.na(dat$LENGTH)),]
dat<-dat[-which(dat$LENGTH==0),]
dat<-dat[which(!is.na(dat$HATCHERY_ORIGIN)),]
## WE HAVE MORE FISH INITIALLY IN THE BASIN THAN WE DO LENGTH DATA FOR 
## THAT BASIN; PLUS THESE LENGTHS ARE ACROSS THE YEARS AND HENCE WOULD 
## ASSUME A STABLE SIZE DISTRIBUTION
## USE MOST RECENT YEAR ONLY???

#BY BASIN
dat$freq<-1
LBH<-dat[which(dat$BASIN=="lower" & dat$HATCHERY_ORIGIN=="H" & dat$YEAR==2017),]
LBH<-aggregate(freq~LENGTH, data=LBH, FUN=sum)
LBH$prob<-LBH$freq/sum(LBH$freq)
len_ini_low_hatchery_nospace2<-DiscreteDistribution(LBH$LENGTH, LBH$prob)
r(len_ini_low_hatchery_nospace2)(5)
  #THIS IS COOL, BUT COULD JUST DO THIS BY SAMPLING THE VECTOR OF
  #LENGTHS... SETTING dat$YEAR==2017 GIVES SIMILAR RESULTS TO ORIGINAL
  #BUT THERE IS SOME DATA MISSING

LBW<-dat[which(dat$BASIN=="lower" & dat$HATCHERY_ORIGIN=="W"),]
LBW<-aggregate(freq~LENGTH, data=LBW, FUN=sum)
LBW$prob<-LBW$freq/sum(LBW$freq)
len_ini_low_natural_nospace2<-DiscreteDistribution(LBW$LENGTH, LBW$prob)

len_ini_low_hatchery_nospace<-ecdf(dat[which(dat$BASIN=="lower" & 
                                             dat$HATCHERY_ORIGIN=="H"),
                                       ]$LENGTH)
ecdf(dat[which(dat$BASIN=="lower" & dat$HATCHERY_ORIGIN=="W"),]$LENGTH)
ecdf(dat[which(dat$BASIN=="upper" & dat$HATCHERY_ORIGIN=="H"),]$LENGTH)
ecdf(dat[which(dat$BASIN=="upper" & dat$HATCHERY_ORIGIN=="W"),]$LENGTH)

#BY SEGMENT
hatch<-lapply(c(2:4,7:10,13,14), function(s)
{
  out<-ecdf(dat[which(dat$SEGMENT_ID==s & dat$HATCHERY_ORIGIN=="H"),
                  ]$LENGTH)
  return(out)
})
names(hatch)<-as.character(c(2:4,7:10,13,14))

nat<-lapply(c(3:4,7:10,13,14), function(s) 
  # 2 HAS NO WILD PS AND 3 ONLY HAS 3... WHAT TO DO?
{
  out<-ecdf(dat[which(dat$SEGMENT_ID==s & dat$HATCHERY_ORIGIN=="W"),
                ]$LENGTH)
  return(out)
})
names(nat)<-as.character(c(3:4,7:10,13,14))
