
setwd("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Pallid-Sturgeon-Stocking-Assessment")

source("./src/01_global.R")
source("./src/02_load.R")
source("./src/03_clean.R")
source("./src/04_tables.R")
source("./src/05_figures.R")
source("./src/06_analysis.R")



figures(1)# age-0 stocking spawning = 1
figures(2)# age-1 stocking spawning = 1

figures(3)# age-0 stocking spawning = 2
figures(4)# age-1 stocking spawning = 2

figures(5)# age-0 stocking spawning = 5
figures(6)# age-1 stocking spawning = 5

figures(7)# age-0 stocking spawning = 10
figures(8)# age-1 stocking spawning = 10

# EXTINCTION PROBABILITY PLOTS

figures(9)