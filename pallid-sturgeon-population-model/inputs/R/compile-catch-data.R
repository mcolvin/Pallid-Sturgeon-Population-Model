#####################################################
#                                                   #
#   THIS SCRIPT COMPILES PAST PSPAP CATCH DATA TO   # 
#   PROVIDE THE FIRST AND LAST ENCOUNTER OF KNOWN   # 
#   (TAGGED) NATURAL ORIGIN FISH                    #
#                                                   #
#####################################################
library(plyr)
library(RODBC)

# PULL, CLEAN, & ORGANIZE PSPAP DATA
pspapcom<- odbcConnectAccess2007("C:/Users/sreynolds/Desktop/Age_Fish/dat/pspap/20181002-pspa_2018.mdb")
### PULL INDIVIDUAL DATA
dat<-sqlFetch(pspapcom,"FISH_CONDITION")
dat<-subset(dat,SPECIES=="PDSG" & HATCHERY_ORIGIN=="W")
### PULL SAMPLING DATA
effort<-sqlFetch(pspapcom,"EFFORT")
effort<-subset(effort, PDSG_COUNT>0)
### COMBINE INDIVIDUAL DATA WITH SAMPLING DATA
dat<-merge(dat, effort, by.x=c("YEAR", "SEGMENT_ID", "UNIQUEID"),
                by.y=c("YEAR", "SEGMENT_ID", "UNIQUEIDENTIFIER"), 
                all.x=TRUE)
odbcClose(pspapcom)
rm(effort, pspapcom)
catchHist<-ddply(dat, .(TAGNUMBER, BASIN), summarize,
                 first_encount=min(SETDATE),
                 last_encount=max(SETDATE),
                 length=LENGTH[which.max(SETDATE)])
names(catchHist)[1]<-"individual"

### SEPARATE BASINS
tmp<-list()
tmp$upper<-catchHist[which(catchHist$BASIN=="UB"),c(1,3:5)]
tmp$lower<-catchHist[which(catchHist$BASIN=="LB"),c(1,3:5)]
catchHist<-tmp

# SAVE DATA
# setwd("C:/Users/sreynolds/Documents/GitHub/Pallid-Sturgeon-Population-Model/pallid-sturgeon-population-model")
saveRDS(catchHist, "./inputs/CatchHistory.rds")


