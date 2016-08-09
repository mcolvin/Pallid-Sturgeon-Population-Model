library(shiny)
library(markdown)
library(MASS)
library(utils)
library(knitr)
library(pander)
library(distr)
library(data.table)


source("figures.R")
source("tables.R")

# LOAD BEND METADATA 
bend_meta<-readRDS("./inputs/metadata/bend_data.rds")


## LOAD INITIALIZATION PLUGINS
source("./inputs/initialization-functions/initialization-plugins.R")
source("./inputs/initialization-functions/ini_growth.R")
### FUNCTION TO INIITIALIZE LENGTH DISTRIBUTION
load("./inputs/initialization-functions/initialize_length_functions.Rdata")
source("./inputs/metadata/default-inputs.R")
## LOAD DYNAMIC PLUGINS
source("./inputs/dynamics-functions/dynamics-plugins.R")
source("./inputs/dynamics-functions/001-adult-survival.R")
source("./inputs/dynamics-functions/003-length-weight.R")
## LOAD INITIALIZATON FUNCTION
source("./core/initialization.R")	
## LOAD SIMULATION FUNCTION
source("./core/simulation.R")

## LOAD SIMULATION MODULES
source("./inputs/modules/recruitment-module.R")
source("./inputs/modules/stocking-module.R")


