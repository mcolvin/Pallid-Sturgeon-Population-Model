#library(shiny)
library(markdown)
library(MASS)
library(utils)
library(knitr)
library(pander)
library(distr)
library(compiler)

setwd("C:/Users/sreynolds/Documents/GitHub/Pallid-Sturgeon-Population-Model/pallid-sturgeon-population-model")


# LOAD BEND METADATA 
bend_meta<-readRDS("./inputs/bend_data.rds")
bend_meta$lower$irc<- runif(nrow(bend_meta$lower),0,3)
bend_meta$upper$irc<- runif(nrow(bend_meta$upper),0,3)
a=-3
b=0.3
bend_meta$upper$irc_p<- plogis(a+b*bend_meta$upper$irc)
bend_meta$lower$irc_p<- plogis(a+b*bend_meta$lower$irc)
source("./inputs/default-inputs2.R")


## LOAD FUNCTION TO PROCESS INPUTS
source("./src/initialization-functions/process-inputs2.R")

## LOAD INITIALIZATION PLUGINS
source("./src/initialization-functions/initialization-plugins2.R")
#source("./src/initialization-functions/ini_growth.R")


### FUNCTION TO INIITIALIZE LENGTH DISTRIBUTION
#load("./src/initialization-functions/initialize-length-functions.Rdata")
#source("./src/initialization-functions/initialize-spatial-location.R")

## LOAD DYNAMIC PLUGINS
source("./src/dynamics-functions/dynamics-plugins2.R")
#source("./src/dynamics-functions/001-adult-survival.R")
#source("./src/dynamics-functions/003-length-weight.R")
#source("./src/dynamics-functions/008-adult-movement.R")

## LOAD INITIALIZATON FUNCTION
source("./src/initialize-model2.R")	

## LOAD SIMULATION FUNCTION
source("./src/simulation.R")

## LOAD SIMULATION MODULES
source("./src/modules/recruitment-module.R")
source("./src/modules/stocking-module.R")

## LOAD FIGURES AND TABLES FUNCTIONS
source("./src/tables.R")
source("./src/figures.R")
