setwd("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Pallid-Sturgeon-Population-Model/Pallid-Sturgeon-Population-Model/")

source("global.R")
input$output_name<- "2016-001" #AFS-KC ANALYSIS
input$commit<- "0df8618"
	
# CREATE DIRECTORY IF NOT ALREADY THERE
dir.create(file.path(paste0(getwd(),"/output/",input$output_name)), showWarnings = FALSE) 
input$basin<-"upper"
inputs<- modelInputs(input,spatial=FALSE)
inputs$nreps=10
inputs$daug_H=43000	
inputs$daug_N=12
inputs$natural = 12
inputs$hatchery= 43000	
inputs$nyears<- 50
dyn<- initialize(inputs=inputs) # INITIALIZE OBJECTS NEEDED FOR SIMUALTION
out<- sim(inputs=inputs,dyn=dyn) #


	
