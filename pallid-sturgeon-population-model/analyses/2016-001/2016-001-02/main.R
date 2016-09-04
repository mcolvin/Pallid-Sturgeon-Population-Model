# Date: 8/8/2016
# Analysis: 2016-001
# Version: 02
# Analysis: Biomass estimates for Steve Keretz (USFWS)

setwd("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Pallid-Sturgeon-Population-Model/Pallid-Sturgeon-Population-Model/")

source("global.R")
input$output_name<- "2016-001" 
input$version<- "02"
input$commit<- "f6a8565"
	
# CREATE DIRECTORY IF NOT ALREADY THERE
dir.create(file.path(paste0(getwd(),"/output/",input$output_name,"-",input$version)), showWarnings = FALSE) 
input$basin<-"upper"
inputs<- modelInputs(input,
	spatial=FALSE,
	recruitmentFreq=0,
	sizeStructure=TRUE)
	
inputs$nreps		= 100
inputs$daug_H		= 43000	
inputs$daug_N		= 12
inputs$natural 		= 12
inputs$hatchery		= 43000	
inputs$natural_age0	= 0	
inputs$nyears		= 50
dyn<- initialize(inputs=inputs) # INITIALIZE OBJECTS NEEDED FOR SIMUALTION
out<- sim(inputs=inputs,dyn=dyn) #




figures(1)
figures(2)
figures(3)
figures(4)
figures(6)
figures(7)
	
