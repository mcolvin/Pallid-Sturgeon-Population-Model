###########################################################
#                                                         #
#   THIS SCRIPT COMPILES PAST STOCKING DATA TO PROVIDE    #
#   THE STOCKING  HISTORY AND BROODSTOCK HISTORY INPUTS   #
#                                                         #
###########################################################
library(plyr)

setwd("./GitHub/Pallid-Sturgeon-Population-Model/pallid-sturgeon-population-model")
dat<- read.csv("inputs/_All Stocked Juveniles.csv")
## LOOK AT RPMA 2, 3, & 4 STOCKING ONLY
dat <- subset(dat, RPMA %in% 2:4)
## CONVERT DATES TO POSIXct FORMAT
dat$SPAWN_DATE <- strptime(dat$SPAWN.DATE, format="%m/%d/%Y")
dat$STOCK_DATE <- strptime(dat$STOCK.DATE, format="%m/%d/%Y")
## ADD IN BIRTH YEAR
dat$YEAR_BORN <- dat$SPAWN_DATE$year+1900
## ADD IN BIRTH MONTH
dat$MONTH_BORN <- dat$SPAWN_DATE$mon+1
## ADD IN STOCKING MONTH
dat$MONTH_STOCKED <- dat$STOCK_DATE$mon+1
## CALCULATE AGE IN MONTHS AT STOCKING
dat$AGE <- (dat$YEAR.STOCKED-dat$YEAR_BORN)*12 + 
  dat$MONTH_STOCKED - dat$MONTH_BORN
## FIX ONE NEGATIVE AGE AND ONE MISSING AGE THAT 
## APPEAR TO BE THE RESULT OF A TYPO
  ## dat[76562:76572,]
dat[which(dat$AGE<0),]$YEAR_BORN <- 1992
  # dat[137200:137210,]
dat[which(is.na(dat$AGE)),c("FEMALE", "MALE", "SPAWN.DATE", 
                            "SPAWN_DATE", "YEAR_BORN", "MONTH_BORN")] <-
  dat[which(is.na(dat$AGE))-1,c("FEMALE", "MALE", "SPAWN.DATE", 
                              "SPAWN_DATE", "YEAR_BORN", "MONTH_BORN")]
dat$AGE <- (dat$YEAR.STOCKED-dat$YEAR_BORN)*12 + 
  dat$MONTH_STOCKED - dat$MONTH_BORN
## DEAL W/ NA's IN NUMBER STOCKED
  # test<-dat[which(is.na(dat$NUMBERS.STOCKED)),]
  # all(!is.na(test$PIT.TAG)) #TRUE
  # test<-dat[which(is.na(dat$NUMBERS.STOCKED) & dat$PIT.TAG==""),]
  # all(!is.na(test$RADIO.TAG)) #TRUE
  # length(which(test$RADIO.TAG=="")) #4494
### ASSUME ONLY ONE FISH WAS STOCKED WHEN NOT SPECIFIED
dat[which(is.na(dat$NUMBERS.STOCKED)),]$NUMBERS.STOCKED <- 1

## OUTLIERS
# CHANGE 0 LENGTH FISH AND FISH WHERE 999 IS A PLACEHOLDER FOR LENGTH TO NA
dat[which(dat$LENGTH==0),]$LENGTH<-NA
dat[which(dat$LENGTH==999),]$LENGTH<-NA
# CHANGE MATJOR OUTLIER LENGTH (LIKELY TYPO) TO NA
dat[which(dat$AGE<24 & dat$LENGTH>2000),]$LENGTH<-NA
# # A SECOND OUTLIER?
# dat[which(dat$AGE<15 & dat$LENGTH>800),]$LENGTH<-NA


## GENERATE STOCKING HISTORY
### CONVERT DATES TO CHARACTERS TO USE DDPLY
dat$SPAWN_DATE <- as.character(dat$SPAWN_DATE)
dat$STOCK_DATE <- as.character(dat$STOCK_DATE)

stockHist <- ddply(dat, .(YEAR.STOCKED, MONTH_STOCKED, RPMA, FEMALE, MALE,
                     HATCHERY, AGE), 
              summarize,
              number=sum(NUMBERS.STOCKED),
              length_mn=mean(LENGTH, na.rm = TRUE),
              length_sd=sd(LENGTH, na.rm=TRUE))
names(stockHist)[c(1:2, 4:5)]<- c("year", "month", "mother", "father")
names(stockHist)<- tolower(names(stockHist))
stockHist<- stockHist[,c(3,1:2,4:10)]
stockHist <- stockHist[order(stockHist$rpma, stockHist$year, 
                             stockHist$month),]

## LOOK AT STOCKING LENGTHS (MEAN AND SD) BY AGE AND USE TO FILL IN NAs
stockHist$age_cat<-ifelse(stockHist$age<=3, stockHist$age,
                          ifelse(stockHist$age<=6, 4.5,
                                 ifelse(stockHist$age<12, 9,
                                        ifelse(stockHist$age<18, 15,
                                            ifelse(stockHist$age<24, 21, 
                                               24)))))
#dat$age_cat<-ifelse(dat$AGE<=3, dat$AGE,
#                    ifelse(dat$AGE<=6, 4.5,
#                           ifelse(dat$AGE<12, 9,
#                                  ifelse(dat$AGE<18, 15,
#                                         ifelse(dat$AGE<24, 21, 24)))))
#tmp2<- ddply(dat, .(age_cat), summarize, 
#             length_mn=mean(LENGTH, na.rm=TRUE),
#             length_sd=sd(LENGTH, na.rm=TRUE))
tmp<- aggregate(length_mn~age_cat, stockHist, mean, na.rm=TRUE)
tmp$length_sd<- c(seq(25,50,5), 100)

tmp<- rbind(data.frame(age_cat=c(0,1),
                       length_mn=c(25, 50),
                       length_sd=c(5, 10)), 
            tmp)
### MEAN
NA_indx<-lapply(tmp$age_cat, function(x)
{
  out1<-which(stockHist$age_cat==x & is.na(stockHist$length_mn))
  out2<-which(tmp$age_cat==x)
  return(data.frame(indx=out1, age_indx=out2))
})
NA_indx<-do.call(rbind,NA_indx)
stockHist[NA_indx$indx,]$length_mn<-tmp$length_mn[NA_indx$age_indx]

### SD
NA_indx<-lapply(tmp$age_cat, function(x)
{
  out1<-which(stockHist$age_cat==x & is.na(stockHist$length_sd))
  out2<-which(tmp$age_cat==x)
  return(data.frame(indx=out1, age_indx=out2))
})
NA_indx<-do.call(rbind,NA_indx)
stockHist[NA_indx$indx,]$length_sd<-tmp$length_sd[NA_indx$age_indx]
rm(NA_indx)
# MAY INSTEAD WANT TO DRAW FAMILY LOT SPECIFIC LENGTH MEANS FROM A DIST 
# AND KEEP SD REASONABLE FOR AGE (OR THE MEAN DRAWN)

## ADD IN SURVIVAL ESTIMATES
### ALL BY AGE AND YEARS SINCE STOCKING:
yr<-2018
month<-12
stockHist$current_age<- (yr-stockHist$year)*12 + month-stockHist$month +
  stockHist$age
stockHist$phi00<-ifelse(stockHist$age<3, 
                        sapply(stockHist$current_age, min, 3)-
                          stockHist$age, 0)
stockHist$phi0<-ifelse(stockHist$age<12 & stockHist$current_age>3, 
                 sapply(stockHist$current_age, min, 12) - 
                   sapply(stockHist$age, max, 3), 0)
stockHist$phi1<-ifelse(stockHist$age<24 & stockHist$current_age>12, 
                 sapply(stockHist$current_age, min, 24) - 
                   sapply(stockHist$age, max, 12), 0)
stockHist$phi2<-ifelse(stockHist$current_age>24, 
                       stockHist$current_age-24, 0)

### SEPARATE BASINS
tmp<-list()
tmp$upper<-stockHist[which(stockHist$rpma==2),]
tmp$middle<-stockHist[which(stockHist$rpma==3),]
tmp$lower<-stockHist[which(stockHist$rpma==4),]
stockHist<-tmp
rm(tmp)
  #WHAT TO DO ABOUT MIGRATION FROM RPMA 3?

### BASIN-SPECIFIC SURVIVALS
#### LOWER
##### WILDHABER +
# stockHist$survival_est<- 0.0021^(stockHist$lower$phi00/3)*
#   0.0510^(stockHist$lower$phi0/9)*0.3674^(stockHist$lower$phi1/12)*
#   0.9220^(stockHist$lower$phi2/12)
##### STEFFENSEN ET AL. 2019 -- MAY ALSO WANT TO TRY MODELING WITH 
#####  DECREASING AGE-1 SURVIVAL BY AGE CLASS... HOW MUCH OF A DECREASE IN
#####  SURVIVAL WOULD BE NEEDED TO MAKE CREATE THE ESTIMATES SEEN IN 
#####  FIG. 7?  
stockHist$lower$survival_est<- 0.075^(stockHist$lower$phi00/12)*
  0.075^(stockHist$lower$phi0/12)*0.279^(stockHist$lower$phi1/12)*
  0.945^(stockHist$lower$phi2/12)
# sum(stockHist$lower$number*stockHist$lower$survival_est)
##### LOWER AT 21,790 WHICH IS GREATER THAN THE PREDICTED 12,134 USED 
##### PREVIOUSLY
##### COMPARE WITH STEFFENSEN ET AL. 2018 https://doi.org/10.1111/jai.13646
# aggregate(NUMBERS.STOCKED~YEAR_BORN, dat[which(dat$RPMA==4),], sum)
##### 1992 & 1997 EXCLUDED...LOOK INTO IF SHOULD EXCLUDE TOO
##### 2004: KIRK HAS ONE MORE FISH THAN US
##### 2008: WE HAVE 558 MORE
##### 2010: WE HAVE 37 MORE
##### 2011: WE HAVE 116 MORE
##### 2012: WE HAVE 99 MORE
##### 2013: WE HAVE 812 MORE
##### 2015: WE HAVE 500 MORE

#### UPPER
##### UPPER AT ANYTHING BETWEEN ~11,000 AND 39,600 WITH ABOVE RATES; 
##### NEED SURVIVAL RATE ESTIMATES -- ROTELLA SEEMS TO USE SOME AGE/COHORT
##### SPECIFIC VALUES
##### USING ROTELLA THE BEST WE CAN QUICKLY
check<-dat[which(dat$RPMA==2),]
MO_frac<-length(which(check$RIVER=="MO" & check$AGE<12))/length(which(check$AGE<12 & (check$RIVER=="MO" | check$RIVER=="YE")))
MO_fing<- c(0.61, 0.66, mean(c(0.7, 0.75, 0.76, 0.81, 0.77, 0.90, 0.89, 0.88)))
YE_fing<- c(0.5, 0.56, mean(c(0.61, 0.65, 0.73, 0.75, 0.67, 0.75, 1, 0.67)))
phi_fing<-MO_frac*MO_fing+(1-MO_frac)*YE_fing
rm(MO_fing, YE_fing)

# NOT SURE WHAT CATEGORIZES AS SPRING VS SUMMER YEARLING SINCE
# length(which(check$AGE>=12 & check$AGE<24 & check$MONTH_STOCKED %in% 3:5))
# length(which(check$AGE>=12 & check$AGE<24 & check$MONTH_BORN %in% 3:5))
# ARE BOTH ZERO
MO_frac<- length(which(check$RIVER=="MO" & check$AGE>=12 & check$AGE<24))/length(which(check$AGE>=12 & check$AGE<24 & (check$RIVER=="MO" | check$RIVER=="YE")))
mo_sp_yr<- c(0.78, mean(c(0.78, 0.82, 0.7, 0.71, 0.76, 0.74, 0.79, 0.73, 0.75, 0.83)))
mo_sm_yr<- c(0.64, mean(c(0.69, 0.82, 0.78, 0.82, 0.83, 0.89, 0.88, 0.87, 0.92, 1, 0.92, 0.91, 1)))
ye_sp_yr<- c(0.65, mean(c(0.68, 0.7, 0.61, 0.63, 0.67, 0.63, 0.60, 0.67, 1, 0.5)))
ye_sm_yr<- c(0.51, mean(c(0.55, 0.75, 0.67, 0.71, 0.8, 1, 1, 0.75, 1, 1)))
# SINCE CATEGORIZATION WAS UNCLEAR AMONG SPRING AND SUMMER, 
# USE EVEN WEIGHTS
phi_year<- 0.5*(MO_frac*(mo_sp_yr+mo_sm_yr)+(1-MO_frac)*(ye_sp_yr+ye_sm_yr)) 
rm(mo_sp_yr, mo_sm_yr, ye_sm_yr, ye_sp_yr, MO_frac)

stockHist$upper$survival_est<- phi_fing[1]^(stockHist$upper$phi00/12)*
  phi_fing[1]^(stockHist$upper$phi0/12)*phi_fing[2]^(stockHist$upper$phi1/12)*
  phi_fing[3]^(stockHist$upper$phi2/12)
stockHist$upper$survival_est<- ifelse(stockHist$upper$age>=12,
                                      phi_year[1]^(stockHist$upper$phi1/12)*
                                        phi_year[2]^(stockHist$upper$phi2/12),
                                      stockHist$upper$survival_est)
# sum(stockHist$upper$number*stockHist$upper$survival_est)
##### INDICATES 241,120 HATCHERY FISH VS. ESTIMATED 16,444 IN 2016
##### BUT ALSO DATA SHOWS 1,666,804 STOCKINGS (WITH 1,663,460 WITH "" 
##### FOR THE FIN CURL ENTRY) IN RPMA 2 BETWEEN 1998 AND 2016, NOT  
##### 245,249 AS IN ROTELLA 2017.  WHY THE DISCREPANCY??? -- HOW TO TELL  
##### IF HAD IRIDOVIRUS?  DOES NOT SEEM TO BE INDICATED IN ANY COMMENTS 
##### (COUNTS IN ROTELLA ARE FOR DISEASE & FIN CURL FREE)

### ADD HERE TO MODIFY BY FAMILY, HATCHERY, OR YEAR-CLASS

#BROODSTOCK DATA
## MOTHER NON-PIT CATEGORIES: "", "MIX", "UKNOWN", "GAVINS"
## FATHER NON-PIT CATEGORIES: "", "MIX", "MIX2", "MIX*", "MIX**", "UKNOWN", "GAVINS"

broodstock<-list()
## INCLUSION OF RPMA 3 BREEDERS IN RPMA 4?
tags<- as.character(unique(stockHist$lower$mother))
tags<- tags[-which(tags %in% c("", "MIX", "UKNOWN", "GAVINS"))]
broodstock$lower$bankF<- lapply(tags, function(x)
{
  yr<-max(dat[which(dat$FEMALE==x),]$YEAR_BORN, na.rm = TRUE)
  out<-data.frame(tag=tags, year=yr)
  return(out)
})
broodstock$lower$bankF<- do.call(rbind, broodstock$lower$bankF)

tags<- as.character(unique(stockHist$lower$father))
tags<- tags[-which(tags %in% c("", "MIX", "MIX2", "MIX*", "MIX**", 
                               "UKNOWN", "GAVINS"))]
broodstock$lower$bankM<- lapply(tags, function(x)
{
  yr<-max(dat[which(dat$MALE==x),]$YEAR_BORN, na.rm = TRUE)
  out<-data.frame(tag=tags, year=yr)
  return(out)
})
broodstock$lower$bankM<- do.call(rbind, broodstock$lower$bankM)



tags<- as.character(unique(stockHist$upper$mother))
tags<- tags[-which(tags %in% c("", "MIX", "UKNOWN", "GAVINS"))]
broodstock$upper$bankF<- lapply(tags, function(x)
{
  yr<-max(dat[which(dat$FEMALE==x),]$YEAR_BORN, na.rm = TRUE)
  out<-data.frame(tag=tags, year=yr)
  return(out)
})
broodstock$upper$bankF<- do.call(rbind, broodstock$upper$bankF)

tags<- as.character(unique(stockHist$upper$father))
tags<- tags[-which(tags %in% c("", "MIX", "MIX2", "MIX*", "MIX**", 
                               "UKNOWN", "GAVINS"))]
broodstock$upper$bankM<- lapply(tags, function(x)
{
  yr<-max(dat[which(dat$MALE==x),]$YEAR_BORN, na.rm = TRUE)
  out<-data.frame(tag=tags, year=yr)
  return(out)
})
broodstock$upper$bankM<- do.call(rbind, broodstock$upper$bankM)

broodstock$upper$BROOD_1<- 
  stockHist$upper[which(stockHist$upper$year==yr & stockHist$upper$age<12),]
# IF HAVE DATA ON NUMBER OF EMBRYOS PRODUCED WOULD BE GREAT
# DRAW FECUNDITIES FOR EACH FEMALE
length(unique(broodstock$upper$BROOD_1$mother))
# DETERMINE HOW MANY SURVIVE TO AGE OF STOCKING
# DETERMINE HOW MANY ARE LEFT AFTER STOCKING
# DETERMINE HOW MANY OF  THESE SURVIVE TO AGE 15

## USE TO LOOK AT RESULTING t0 DISTRIBUTION
# t0<- age + Log[1-length/Linf]/k
# COMPARE W/ length2 

broodstock$lower$BROOD_1<- 
  stockHist$lower[which(stockHist$lower$year==yr & stockHist$lower$age<12),]
tmp<- data.frame(mother=c("46263A6E1B", "462704502D", "462704502D",
                          "4627693763",
                          "4627545945", "4627545945", "4626641923",
                          "4626641923", "01358", "46270E6C3C", 
                          "46270E6C3C", "4626711111", "4626711111"),
                 father=c("4625761D21", "4716281E21", "4715591B05",
                          "47187B4132",
                          "6C00107471", "46276D476B", "462711443D", 
                          "F7F39", "A3B7D", "47041F697D", "D6C1E", 
                          "4626773563", "462457120D"),
                 hatchery=rep("GAVINS POINT", 13),
                 eggs=c(37400, 0.5*25200, 0.5*25200, 1500, 28860, 7280,
                        18560, 6032, 19458, 17064, 7344, 6244, 6021))
broodstock$lower$BROOD_1<-merge(broodstock$lower$BROOD_1[,c("mother", "father", "hatchery", "number")], 
                                tmp,by=c("mother", "father", "hatchery"), all=TRUE)
broodstock$lower$BROOD_1[is.na(broodstock$lower$BROOD_1$number),]$number<-0
broodstock$lower$BROOD_1[is.na(broodstock$lower$BROOD_1$eggs),]$eggs
