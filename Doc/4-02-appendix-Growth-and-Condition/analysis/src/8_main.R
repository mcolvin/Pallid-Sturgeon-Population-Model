

setwd("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Pallid-Sturgeon-Population-Model/Doc/4-02-appendix-Growth-and-Condition/analysis/")
	source("./src/1_global.R")  	
	source("./src/2_functions.R")         
	source("./src/3_load.R")     
  	source("./src/4_clean.R" )
 	source("./src/5_tables.R") 
	source("./src/6_figures.R")  
	  
	source("./src/7_analysis.R")


figures(1) # study area
savePlot("./figures/figure-01.png",type="png")

figures(2) # length weight plot for upper and lower basin
savePlot("./figures/figure-02.png",type="png")

figures(3) # Change in length (y-axis) versus time at large (x-axis) plot for upper and lower basin
savePlot("./figures/figure-03.png",type="png")

figures(5)# PLOT TIME AND GROWTH
savePlot("./figures/figure-05.png",type="png")


tbl1<- tables("tbl1") # length weight relationship by basin
write.csv(tbl1, "./tables/table-01.csv")
	
tbl2<- tables("tbl2") # growth model selection
write.csv(tbl2, "./tables/table-02.csv")

tbl3<- tables("tbl3") # growth model parm estimates
write.csv(tbl3, "./tables/table-03.csv")	
	
tbl5<- tables("tbl5")# growth model parm estimates-correlated linf and k
write.csv(tbl5, "./tables/table-05.csv")		






