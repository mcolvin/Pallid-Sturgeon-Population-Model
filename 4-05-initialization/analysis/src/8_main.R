

setwd("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Pallid-Sturgeon-Population-Model/4-05-initialization/analysis")
	source("./src/1_global.R")  	
	source("./src/2_functions.R")         
	source("./src/3_load.R")     
  	source("./src/4_clean.R" )
 	source("./src/5_tables.R") 
	source("./src/6_figures.R") 	
	source("./src/7_analysis.R")
	
	# SPATIAL EXTENT OF SAMPLING BY YEAR
	## LOWER BASIN
	figures(1)
	savePlot("./figures/figure-01.wmf",type="wmf")
	## UPPER BASIN	
	figures(2)
	savePlot("./figures/figure-02.wmf",type="wmf")
	
	
	# PLOT OF LENGTH DISTRIBUTIONS OVER TIME	
	## LOWER BASIN
	figures(3)
	savePlot("./figures/figure-03.wmf",type="wmf")	
	## UPPER BASIN
	figures(4)
	savePlot("./figures/figure-04.wmf",type="wmf")
	
	# PLOT OF LENGTH DISTRIBUTIONS OVER TIME	
	## LOWER BASIN	NATURAL ORIGIN
	figures(5)
	savePlot("./figures/figure-05.wmf",type="wmf")
	## LOWER BASIN	HATCHERY ORIGIN	
	figures(6)
	savePlot("./figures/figure-06.wmf",type="wmf")
	
	# PLOT OF LENGTH DISTRIBUTIONS OVER TIME	
	## UPPER BASIN	NATURAL ORIGIN
	figures(7)
	savePlot("./figures/figure-07.wmf",type="wmf")
	## UPPER BASIN	HATCHERY ORIGIN	
	figures(8)	
	savePlot("./figures/figure-08.wmf",type="wmf")
	
	# PLOT OF INITIALIZATION DENSITY FOR ORIGIN*BASIN
	figures(9)
	savePlot("./figures/figure-09.wmf",type="wmf")		
	
	
	# TABL 1. 
	write.csv(tables(1),"./tables/table-01.csv")
		