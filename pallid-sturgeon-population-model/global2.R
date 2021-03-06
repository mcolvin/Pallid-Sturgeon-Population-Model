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
bend_meta$lower$irc<- runif(nrow(bend_meta$lower),0,3)
bend_meta$upper$irc<- runif(nrow(bend_meta$upper),0,3)
a=-3
b=0.3
bend_meta$upper$irc_p<- plogis(a+b*bend_meta$upper$irc)
bend_meta$lower$irc_p<- plogis(a+b*bend_meta$lower$irc)
rm(a,b)
source("./inputs/default-inputs.R")


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
