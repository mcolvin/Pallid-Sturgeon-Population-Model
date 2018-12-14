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
## PSPAP DATA
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
### PULL SAMPLING DATA
effort<-sqlFetch(pspapcom,"EFFORT")
effort<-subset(effort, PDSG_COUNT>0)
### COMBINE LENGTH DATA WITH SAMPLING DATA
dat<-merge(dat, effort, by.x=c("YEAR", "SEGMENT_ID", "UNIQUEID"),
           by.y=c("YEAR", "SEGMENT_ID", "UNIQUEIDENTIFIER"), 
           all=TRUE)
# ### MISSING SAMPLING DATA!!!
# length(setdiff(dat$UNIQUEID, effort$UNIQUEIDENTIFIER)) #25
# nrow(dat[which(dat$UNIQUEID %in% 
#                  setdiff(dat$UNIQUEID, effort$UNIQUEIDENTIFIER)),]) #48
dat$SOURCE<-"PSPAP"
odbcClose(pspapcom)
rm(pspapcom, effort)
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
    
## REMOVE FISH W/O PIT TAGS
indx<-which(is.na(dat$TAGNUMBER) | 
              dat$TAGNUMBER %in% c(" ", "CAUDAL CL", "N PIT", 
                                   "nofishscan", "xxxxxxxxxx",
                                   "XXXXXXXXXX"))
dat<-dat[-indx,]
rm(indx)
## STOCKING DATA
datS<- read.csv(paste0(db_location, "stocking/_All Stocked Juveniles.csv"))
### REMOVE FISH WITH MISSING SPAWNING DATE
datS<-datS[-which(datS$SPAWN.DATE==""),]
### CONVERT DATES TO POSIXct FORMAT
indx1<-grep("/",datS$SPAWN.DATE)
dates<-data.frame(ID=datS$ID[indx1], 
                  SPAWN_DATE=strptime(datS$SPAWN.DATE[indx1], 
                                      format="%m/%d/%Y"),
                  STOCK_DATE=strptime(datS$STOCK.DATE[indx1], 
                                      format="%m/%d/%Y"))
datS<-merge(datS, dates, by=c("ID"), all=TRUE)
rm(indx1, dates)
# ### CALCULATE AGE AT TIME OF STOCKING
# datS$AGE<-as.numeric(difftime(datS$STOCK_DATE, 
#                               datS$SPAWN_DATE, 
#                               units = "days"))/365.25
# ## GATHER STOCKING LENGTH AGE DATA
# stockLA<-datS[,c(1,2,30,10,11,14,28,29,15:17,23:26,20)]
# ### REMOVE UNKNOWN LENGTH OR AGE FISH AT TIME OF STOCKING
# stockLA<-stockLA[-which(is.na(stockLA$AGE)),]
# stockLA<-stockLA[-which(is.na(stockLA$LENGTH)),]
# names(stockLA)[2]<-"TAGNUMBER"

## COMBINE RECAPTURE DATA WITH STOCKING DATA
names(datS)[10:11]<-c("STOCK_LENGTH", "STOCK_WEIGHT")
### FIND PIT TAGS THAT WERE RECAPTURED
indx<-which(datS$PIT.TAG %in% dat$TAGNUMBER)
datS<-datS[indx,]
rm(indx)

### MERGE STOCK AND SPAWN DATA WITH RECAPTURED PIT TAGS
dat<-merge(dat, datS, by.x=c("TAGNUMBER"), by.y=c("PIT.TAG"), 
           all=TRUE)
if(any(duplicated(dat))){dat<-dat[-which(duplicated(dat)),]}
### REMOVE FISH FOR WHICH SPAWN DATA IS NOT AVAILABLE
dat<-dat[-which(is.na(dat$SPAWN.DATE)),] 
rm(datS, db_location)
### REMOVE FISH FOR WHICH GEAR SET DATA IS NOT AVAILABLE 
### (ALL MR_IDs IN MISSING IDs)
dat<-dat[-which(is.na(dat$SETDATE)),]
### CALCULATE AGE AT TIME OF RECAPTURE
dat$AGE<-as.numeric(difftime(dat$SETDATE, 
                             dat$SPAWN_DATE, 
                             units = "days"))/365.255

### REMOVE FISH MISSING LENGTH DATA
dat<-dat[-which(is.na(dat$LENGTH) | dat$LENGTH==0),]
    ### CLEAN DATA
    recapLA<-dat[,c(2,31,28,3,33,44,45,1,91,10,9,11,
                    26,32,50:56,49,57,88,
                    89,90,70,71,85,75,74,6,4,62,5)]
    names(recapLA)[c(29,30,34)]<-c("FIN_CURL", "STOCK_SITE", "STOCK_ID")
    UnknownAge<-UnknownAge[,c(2,31,28,3,33,44,45,1,10,9,11,26,
                              32,50:56,49,57,88,89,
                              90,70,71,85,75,74,6,4,62,5)]
    names(UnknownAge)[c(28,29,33)]<-c("FIN_CURL", "STOCK_SITE", 
                                      "STOCK_ID")
    ### ADD RIVER SECTION & RPMA
    recapLA$RPMA<-ifelse(recapLA$SEGMENT_ID %in% c(1:4,21,22,52), 2,
                         ifelse(recapLA$SEGMENT_ID %in% c(7:11,13,14,28),
                                4, 3))
    recapLA$RIVER_SECTION<-ifelse(recapLA$RPMA==2, "Upper", "Lower")
    UnknownAge$RPMA<-ifelse(UnknownAge$SEGMENT_ID %in% c(1:4,21,22,52), 
                            2,
                            ifelse(UnknownAge$SEGMENT_ID %in% c(7:11,13,14,27,28),
                                   4, 3))
    UnknownAge$RIVER_SECTION<-ifelse(UnknownAge$RPMA==2, "Upper", 
                                     "Lower")
    
    ### FIND MOVERS
    dat<-rbind.fill(recapLA,UnknownAge)
    dat$freq<-1
    Movers<-dcast(dat, TAGNUMBER~SEGMENT_ID, sum, value.var="freq")
    Movers$Num_Caps<-rowSums(Movers[,2:21])
    Movers<-subset(Movers, Num_Caps>1)
    Movers$Maxs<-NA
    for(i in 1:nrow(Movers))
    {
      Movers$Maxs[i]<-max(Movers[i,2:21])
    }
    Movers<-subset(Movers, Num_Caps>Maxs)
    indx<-which(colSums(Movers[,2:21])==0)+1
    Movers<-Movers[,-indx]
    rm(i,indx,dat)
    ### OUTLIERS
    ## REMOVE OUTLIER THAT SHRANK AFTER STOCKING
    recapLA<-recapLA[-which(recapLA$AGE>5 & recapLA$LENGTH<200),]
    # REMOVE 0 LENGTH FISH AND FISH WHERE 999 IS A PLACEHOLDER FOR LENGTH
    stockLA<-stockLA[-which(stockLA$LENGTH==0),]
    stockLA<-stockLA[-which(stockLA$LENGTH==999),]
    # REMOVE OUTLIER (LIKELY TYPO)
    stockLA<-stockLA[-which(stockLA$AGE<2 & stockLA$LENGTH>2000),]
    # A SECOND OUTLIER?
    stockLA<-stockLA[-which(stockLA$AGE<1.2 & stockLA$LENGTH>800),]
    
    
    ## SAVE NEW FILES
    if(rewrite)
    {
      saveRDS(recapLA, "./_dat/Field_Length-Age_Data.rds")
      saveRDS(stockLA, "./_dat/Stock_Length-Age_Data.rds")
      saveRDS(UnknownAge, "./_dat/Field_Unknown-Age_Length_Data.rds")
      saveRDS(NoPIT, "./_dat/Field_NoPIT_Unknown-Age_Length_Data.rds")
      saveRDS(Movers, "./_dat/Field_Segment_Movements.rds")
      saveRDS(NoRecaps, "./_dat/Non-recaptured_Stocked_Fish.rds")
    }
  }
  out<-list(KnownField=recapLA, KnownStock=stockLA, 
            UnknownField=UnknownAge, UnknownField_NoPIT=NoPIT, 
            Movers=Movers, NoRecaps=NoRecaps)
  return(out)
}