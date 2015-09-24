
	setwd("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Pallid-Sturgeon-Population-Model/")
	source("./src/01_global.R")
	source("./src/02_load.R")
	source("./src/03_clean.R")
	source("./src/04_functions.R")
	source("./src/05_tables.R")
	source("./src/06_figures.R")
	#source("./src/07_analysis.R")
	
	
	# COMPILE SIMULATIONS FOR EVALAUATE IN OTHER ROOT DIRECTORY
	tmp<- tables(1)# compile inputs
	write.csv(tmp,
		file="C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Pallid-Sturgeon-Stocking-Assessment/dat/inputs.csv")
	tmp<- tables(2)
	write.csv(tmp,
		file="C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Pallid-Sturgeon-Stocking-Assessment/dat/sims.csv")
		
	
	# Figure 1: TOTAL POPULATION DYNAMICS
	figures(1)
	# Figure 2: NATURAL ORIGIN POPULATION DYNAMICS
	figures(2)
	# Figure 3: HATCHERY ORIGIN POPULATION DYNAMICS
	figures(3)







