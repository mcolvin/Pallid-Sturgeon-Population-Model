
library(xlsx)
library(plyr)
library(reshape2)
setwd("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Pallid-Sturgeon-Population-Model/4-03-appendix-broodstock")

com45<- odbcConnectAccess2007("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Data/pallids.accdb")

dat<- sqlFetch(com45, "Stocking Data")
dat$stock_year<- factor(as.numeric(format(dat$stock_date,"%Y")),levels=c(1994:2014))
dat$age_at_stocking<- as.character(tolower(dat$age_at_stocking))
dat$age_at_stocking<- factor(  dat$age_at_stocking,levels=c( "fry","fingerling" , "adv. fing.","yearling",
	"2 yr old","3 yr old","4 yr old","5 yr old","6 yr old","7 yr old" ))
dat<- subset(dat, !(is.na(dat$number_stocked))) 


# WHEN ARE FISH SPAWNED
tmp<-aggregate(year_class~mother+spawn_month+RPMA,dat, length)
tmp$n<- 1

tmp<- dcast(tmp, spawn_month~RPMA,value.var="n",sum, subset=.(RPMA %in% c(2,4)))
tmp$p2<- round(tmp$"2"/sum(tmp$"2"),2)
tmp$p4<- round(tmp$"4"/sum(tmp$"4"),2)

tmp<- tmp[,c(1,2,4,3,5)]
write.csv(tmp,"./output/spawning_month.csv")

# WHAT HATCHERY ARE FISH FROM?
tble2<-dcast( dat, hatchery~RPMA, value.var="number_stocked", 
	subset=.(RPMA %in% c(2,4)),sum,drop=TRUE)


# HOW MANY FISH ARE STOCKED
tble3<-dcast( dat, RPMA+stock_year~age_at_stocking, value.var="number_stocked", 
	subset=.(RPMA %in% c(2,4)),sum,drop=FALSE)
write.csv(tble3,"./output/tble3.csv")


# ages of fish when stocked
dat$age
dat<- subset(dat, !(is.na(dat$days_post_spawn))) 
tble4<- ddply(dat, .(RPMA,age_at_stocking), summarize, 
	mean=round(mean(days_post_spawn),0),
	min=min(days_post_spawn),
	max=max(days_post_spawn))
tble4<- subset(tble4, RPMA %in% c(2,4))
write.csv(tble4,"./output/tble4.csv")	


