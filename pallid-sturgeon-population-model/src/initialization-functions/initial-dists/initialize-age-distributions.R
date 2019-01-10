###########################################################
#
#  THIS SCRIPT CREATES THE DISCRETE LENGTH DISTRIBUTIONS
#   USED TO INITIALIZE THE POPULATION MODEL
# 
#  LAST UPDATED: 1/9/2019
#
##########################################################
library(RODBC)
library(plyr)
library(reshape2)
library(distr)

db_location<-"C:/Users/sreynolds/Desktop/Age_Fish/dat/"
## PSPAP DATA
pspapcom<- odbcConnectAccess2007(paste0(db_location, "pspap/20181002-pspa_2018.mdb"))
### PULL RECENT PS CAPTURE DATA
y<- 2016:2018
dat<-sqlFetch(pspapcom,"FISH_CONDITION")
dat<-subset(dat,SPECIES=="PDSG" & YEAR %in% y)
#DO WE WANT TO INCLUDE TRIBUTARIES?:
#21 (Milk), 22 (Yellowstone), 52 (Sakakawea) in Upper
#11 (Kansas), 28 (Osage), 27 (Grand) in Lower
#5,6,23(Niobrara),24(James) in Inter-Reservoir
#1 in ????
### PULL SAMPLING DATA
effort<-sqlFetch(pspapcom,"EFFORT")
effort<-subset(effort, PDSG_COUNT>0 & YEAR %in% y)
### COMBINE FISH DATA WITH SAMPLING DATA
dat<-merge(dat, effort, by.x=c("YEAR", "SEGMENT_ID", "UNIQUEID"),
           by.y=c("YEAR", "SEGMENT_ID", "UNIQUEIDENTIFIER"), 
           all.x=TRUE)
dat<-dat[,c("TAGNUMBER", "YEAR", "BASIN", "SEGMENT_ID", "SETDATE", 
            "HATCHERY_ORIGIN", "BENDRN", "SUBSAMPLEN")]
# ### MISSING SAMPLING DATA!!!
# length(setdiff(dat$UNIQUEID, effort$UNIQUEIDENTIFIER)) #25
# nrow(dat[which(dat$UNIQUEID %in% 
#                  setdiff(dat$UNIQUEID, effort$UNIQUEIDENTIFIER)),]) #48
dat$SOURCE<-"PSPAP"
odbcClose(pspapcom)
rm(pspapcom, effort)
## HAMP DATA
hampcom<- odbcConnectAccess2007(paste0(db_location, "hamp/hamp_2018.mdb"))
### PULL RECENT PS DATA
hampdat<-sqlFetch(hampcom,"FISH")
hampdat<-subset(hampdat,SPECIES=="PDSG" & YEAR %in% y)
### DEFINE BASIN AND SOURCE
hampdat$BASIN<-"LB"
hampdat$SOURCE<-"HAMP"
hampdat<-hampdat[,intersect(c("TAGNUMBER", "YEAR", "BASIN", 
                              "SEGMENT_ID", "SETDATE", 
                              "HATCHERY_ORIGIN", "BENDRN", "SUBSAMPLEN", 
                              "SOURCE"), names(hampdat))]
dat<-rbind.fill(dat,hampdat)
odbcClose(hampcom)  
rm(hampdat,hampcom)

## REMOVE DATA WITHOUT KNOWN ORIGIN
dat[which(is.na(dat$HATCHERY_ORIGIN)),]$HATCHERY_ORIGIN<-"U"
dat<-dat[-which(dat$HATCHERY_ORIGIN=="U"),]
    
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
if(any(datS$SPAWN.DATE=="")){datS<-datS[-which(datS$SPAWN.DATE==""),]}
### CONVERT DATES TO POSIXct FORMAT
indx1<-grep("/",datS$SPAWN.DATE)
dates<-data.frame(ID=datS$ID[indx1], 
                  SPAWN_DATE=strptime(datS$SPAWN.DATE[indx1], 
                                      format="%m/%d/%Y"),
                  STOCK_DATE=strptime(datS$STOCK.DATE[indx1], 
                                      format="%m/%d/%Y"))
datS<-merge(datS, dates, by=c("ID"), all=TRUE)
rm(indx1, dates)
### DEFINE BASIN
datS$BASIN<-ifelse(datS$RPMA==2, "UB",
                   ifelse(datS$RPMA==4, "LB",
                          "other"))
### LIMIT COLUMNS
datS<- datS[,c("PIT.TAG", "YEAR.STOCKED", "BASIN", "SPAWN_DATE",
               "STOCK_DATE")]

## ADD SPAWN_DATE TO RECAPTURED FISH
### PIT TAGS THAT WERE RECAPTURED
indx<-which(datS$PIT.TAG %in% dat$TAGNUMBER)
### KEEP RECAPTURED SPAWN DATA
datS_recap<-datS[indx, c("PIT.TAG", "YEAR.STOCKED", "SPAWN_DATE", 
                         "STOCK_DATE")]
rm(indx)
### MERGE STOCK AND SPAWN DATA WITH RECAPTURED PIT TAGS
dat<-merge(dat, datS_recap, by.x=c("TAGNUMBER"), by.y=c("PIT.TAG"), 
           all=TRUE)
if(any(duplicated(dat))){dat<-dat[-which(duplicated(dat)),]}
### REMOVE FISH FOR WHICH SPAWN DATA IS NOT AVAILABLE
if(any(is.na(dat$SPAWN_DATE))){dat<-dat[-which(is.na(dat$SPAWN_DATE)),]} 
rm(datS_recap, db_location)
### REMOVE FISH FOR WHICH GEAR SET DATA IS NOT AVAILABLE 
### (ALL MR_IDs IN MISSING IDs)
if(any(is.na(dat$SETDATE))){dat<-dat[-which(is.na(dat$SETDATE)),]}
### CALCULATE AGE AT TIME OF RECAPTURE
dat$AGE<-as.numeric(difftime(dat$SETDATE, 
                             dat$SPAWN_DATE, 
                             units = "days"))/365.255

## RECENTLY STOCKED FISH 
datS<- subset(datS, YEAR.STOCKED %in% y & BASIN %in% c("UB", "LB"))
### CALCULATE AGE AT TIME OF STOCKING
datS$AGE<-as.numeric(difftime(datS$STOCK_DATE, 
                             datS$SPAWN_DATE, 
                             units = "days"))/365.255
datS$YEAR<-datS$YEAR.STOCKED
datS$HATCHERY_ORIGIN<-"H"
datS$SOURCE<-"STOCK"
names(datS)[1]<-"TAGNUMBER"

## MERGE RECAPTURE DATA AND STOCK DATA
dat<-rbind.fill(dat, datS)
rm(datS)

## AGE DATA BY YEAR, BASIN, AND ORIGIN
dat$AGE_CLASS<- floor(dat$AGE)
a<-max(dat$AGE_CLASS)
dat$AGE_CLASS<- factor(dat$AGE_CLASS,
                       levels=0:(a+2),
                       ordered=TRUE)
rm(a)
dat$freq<- 1
tbl<- dcast(dat, BASIN+YEAR+HATCHERY_ORIGIN~AGE_CLASS, sum, 
            value.var="freq", drop=FALSE)
N<-ncol(tbl)
source("./inputs/default-inputs2.R")
### UPPER BASIN
#### 2-YEARS AGO
tmp<- subset(tbl, BASIN=="UB" & YEAR==y[1])
tmp[,8:N]<- tmp[,6:(N-2)]*input$upper$phi_age2_mean^2
tmp[,7]<-tmp[,5]*input$upper$phi_age1_mean*input$upper$phi_age2_mean
tmp[,6]<-tmp[,4]*input$upper$phi_age0_mean*input$upper$phi_age1_mean
tmp[,4:5]<-0
tbl[which(tbl$BASIN=="UB" & tbl$YEAR==y[1]),]<-tmp
### 1-YEAR AGO
tmp<- subset(tbl, BASIN=="UB" & YEAR==y[2])
tmp[,7:(N-1)]<- tmp[,6:(N-2)]*input$upper$phi_age2_mean
tmp[,6]<- tmp[,5]*input$upper$phi_age1_mean
tmp[,5]<- tmp[,4]*input$upper$phi_age0_mean
tmp[,4]<-0
tbl[which(tbl$BASIN=="UB" & tbl$YEAR==y[2]),]<-tmp
### LOWER BASIN
#### 2-YEARS AGO
tmp<- subset(tbl, BASIN=="LB" & YEAR==y[1])
tmp[,8:N]<- tmp[,6:(N-2)]*input$lower$phi_age2_mean^2
tmp[,7]<-tmp[,5]*input$lower$phi_age1_mean*input$lower$phi_age2_mean
tmp[,6]<-tmp[,4]*input$lower$phi_age0_mean*input$lower$phi_age1_mean
tmp[,4:5]<-0
tbl[which(tbl$BASIN=="LB" & tbl$YEAR==y[1]),]<-tmp
### 1-YEAR AGO
tmp<- subset(tbl, BASIN=="LB" & YEAR==y[2])
tmp[,7:(N-1)]<- tmp[,6:(N-2)]*input$lower$phi_age2_mean
tmp[,6]<- tmp[,5]*input$lower$phi_age1_mean
tmp[,5]<- tmp[,4]*input$lower$phi_age0_mean
tmp[,4]<-0
tbl[which(tbl$BASIN=="LB" & tbl$YEAR==y[2]),]<-tmp
## FINALIZE, AND IGNORE AGE-0's
tblU<- subset(tbl, BASIN=="UB" & HATCHERY_ORIGIN=="H")
tblU<-colSums(tblU[,5:N])
tblL<- subset(tbl, BASIN=="LB" & HATCHERY_ORIGIN=="H")
tblL<-colSums(tblL[,5:N])
tbl<-expand.grid(BASIN=c("upper", "lower"), ORIGIN="H",
                AGE=as.numeric(names(tblU)))
tbl$BASIN<-factor(tbl$BASIN,
                  levels=c("upper", "lower"),
                  ordered=TRUE)
tbl<-tbl[order(tbl$BASIN, tbl$AGE),]
tbl$p<-c(tblU, tblL)
plot(tbl[tbl$BASIN=="upper",]$AGE, tbl[tbl$BASIN=="upper",]$p)
