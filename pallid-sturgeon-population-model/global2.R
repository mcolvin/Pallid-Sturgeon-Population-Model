#library(shiny)
library(markdown)
library(MASS)
library(utils)
library(knitr)
library(pander)
library(distr)
library(compiler)
library(plyr)

setwd("C:/Users/sreynolds/Documents/GitHub/Pallid-Sturgeon-Population-Model/pallid-sturgeon-population-model")


# LOAD BEND METADATA 
bend_meta<-readRDS("./inputs/bend_data.rds")
bend_meta$lower$RIVER<-"MO"
bend_meta$upper$RIVER<-"MO"
YR <- data.frame(B_SEGMENT=22, BEND_NUM=1, 
                 UPPER_RIVER_MILE=round(113*0.621371, 1),
                 LOWER_RIVER_MILE=0, STATE="MT", Length.RKM=113,
                 basin="upper", id=999,RIVER="YR") 
  #Intake at 113 rkm from UBPSRWG 2004 Annual Report
YR$Length.RM<-YR$UPPER_RIVER_MILE-YR$LOWER_RIVER_MILE
YR<-rbind.fill(bend_meta$upper, YR)
#YR<-rbind.fill(bend_meta$upper[1:max(which(bend_meta$upper$B_SEGMENT==4)),],
#                YR)
#YR<-rbind(YR, bend_meta$upper[min(which(bend_meta$upper$B_SEGMENT==3)):nrow(bend_meta$upper),])
bend_meta$upper<-YR
rm(YR)
bend_meta$lower$irc<- runif(nrow(bend_meta$lower),0,3)
bend_meta$upper$irc<- runif(nrow(bend_meta$upper),0,3)
a=-3
b=0.3
bend_meta$upper$irc_p<- plogis(a+b*bend_meta$upper$irc)
bend_meta$lower$irc_p<- plogis(a+b*bend_meta$lower$irc)
rm(a,b)
source("./inputs/default-inputs2.R")


## LOAD FUNCTION TO PROCESS INPUTS
source("./src/initialization-functions/process-inputs2.R")

## LOAD INITIALIZATION PLUGINS
source("./src/initialization-functions/initialization-plugins2.R")

## LOAD DYNAMIC PLUGINS
source("./src/dynamics-functions/dynamics-plugins2.R")

## LOAD INITIALIZATON FUNCTION
source("./src/initialize-model2.R")	

## LOAD SIMULATION FUNCTION
#source("./src/simulation.R")
source("./src/simulation2.R")

## LOAD SIMULATION MODULES
#source("./src/modules/recruitment-module.R")
#source("./src/modules/stocking-module.R")

## LOAD FIGURES AND TABLES FUNCTIONS
#source("./src/tables.R")
source("./src/figures.R")
