# TO DO:
## [1] MOVE FROM BENDS TO RKMS 
## [2] MAKE EASY WAY OF MAPPINT RKMS TO BENDS
## [3] ADD RKM STARTS AND BEND AS A STRCTURE FILE IN LOAD..


## NOTES
### TO COMPLILE TO APP-SINK GLOBAL.R, LOAD.R, PLUGINS.R AND FUNCTIONS.R TO A FILE-> GLOBAL.R
### SINK TABLES AND FIGURES FUNCTIONS TOO?  MIGHT WORK...
### sink(file="./output/test.R")
###	source("./src/02_load.R", echo = TRUE)
###	sink()


setwd("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Pallid-Sturgeon-Population-Model/")
	
	source("./src/01_global.R")
	source("./src/02_load.R")
	source("./src/03_clean.R")
	source("./src/03_plugins.R")	
	source("./src/04_functions.R")
	source("./src/05_tables.R")
	source("./src/06_figures.R")
	#source("./src/07_analysis.R")
	

	# Figure 1: TOTAL POPULATION DYNAMICS
	figures(1)
	# Figure 2: NATURAL ORIGIN POPULATION DYNAMICS
	figures(2)
	# Figure 3: HATCHERY ORIGIN POPULATION DYNAMICS
	figures(3)





