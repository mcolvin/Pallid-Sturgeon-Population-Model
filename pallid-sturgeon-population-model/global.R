library(shiny)
library(markdown)
library(MASS)
library(utils)
library(knitr)
library(pander)
library(distr)
library(compiler)
library(data.table)
source("figures.R")
source("tables.R")

# LOAD BEND METADATA 
bend_meta<-readRDS("./inputs/metadata/bend_data.rds")
x$lower$irc<- runif(nrow(l),0,3)
x$upper$irc<- runif(nrow(u),0,3)
a=-3
b=0.3
x$upper$irc_p<- plogis(a+b*x$upper$irc)
x$lower$irc_p<- plogis(a+b*x$lower$irc)



## LOAD FUNCTION TO PROCESS INPUTS
source("./inputs/initialization-functions/process-inputs.R")

## LOAD INITIALIZATION PLUGINS
source("./inputs/initialization-functions/initialization-plugins.R")
source("./inputs/initialization-functions/ini_growth.R")


### FUNCTION TO INIITIALIZE LENGTH DISTRIBUTION
load("./inputs/initialization-functions/initialize-length-functions.Rdata")
source("./inputs/initialization-functions/initialize-spatial-location.R")
source("./inputs/metadata/default-inputs.R")
## LOAD DYNAMIC PLUGINS
source("./inputs/dynamics-functions/dynamics-plugins.R")
source("./inputs/dynamics-functions/001-adult-survival.R")
source("./inputs/dynamics-functions/003-length-weight.R")
source("./inputs/dynamics-functions/008-adult-movement.R")
## LOAD INITIALIZATON FUNCTION
source("./core/initialize-model.R")	
## LOAD SIMULATION FUNCTION
source("./core/simulation.R")

## LOAD SIMULATION MODULES
source("./inputs/modules/recruitment-module.R")
source("./inputs/modules/stocking-module.R")

## LOAD FIGURES AND TABLES FUNCTIONS
source("./tables.R")
source("./figures.R")
