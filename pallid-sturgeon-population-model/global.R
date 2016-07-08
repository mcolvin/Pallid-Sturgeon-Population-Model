library(shiny)
library(markdown)
library(MASS)
library(utils)
library(knitr)
library(pander)
library(distr)





# LOAD BEND METADATA 
load("./inputs/metadata/bend_data.Rdata")
## LOAD INITIALIZATION PLUGINS
source("./inputs/initialization-functions/initialization-plugins.R")
### FUNCTION TO INIITIALIZE LENGTH DISTRIBUTION
load("./inputs/initialization-functions/initialize_length_functions.Rdata")
source("./inputs/metadata/default-inputs.R")
## LOAD DYNAMIC PLUGINS
source("./inputs/dynamics-functions/dynamics-plugins.R")
## LOAD INITIALIZATON FUNCTION
source("./core/initialization.R")	
## LOAD SIMULATION FUNCTION
source("./core/simulation.R")



