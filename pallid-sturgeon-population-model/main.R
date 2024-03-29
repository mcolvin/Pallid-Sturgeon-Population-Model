
	setwd("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Pallid-Sturgeon-Population-Model/Pallid-Sturgeon-Population-Model/")
	source("global.R")
	input$version<- "01"
	input$output_name<- "2016-002" #AFS-KC ANALYSIS
 	input$commit<- "0df8618"   
	input$output_name<- "2018-001" #AFS-KC ANALYSIS
	input$commit<- "05e80b7"
	print(shell("git rev-parse --short HEAD", intern = TRUE))
    
    library(git2r)

repo <- repository(".")
print(head(repo))
    
	# CREATE DIRECTORY IF NOT ALREADY THERE
	dir.create(file.path(paste0(getwd(),"/output/",input$output_name)), showWarnings = FALSE) 
	input$basin<-"upper"
	
	# PREPARE INPTUS FOR MODEL
	inputs<- modelInputs(input,spatial=FALSE)
	inputs$nreps=5
	inputs$daug_H=60000	
	inputs$daug_N=20000	
	inputs$nyears<- 10
	dyn<- initialize(inputs=inputs) # INITIALIZE OBJECTS NEEDED FOR SIMUALTION
	dyn2<-dyn
	

	ptm <- proc.time()
	out<- sim(inputs=inputs,
		dyn=dyn,
		recruitmentFreq=0) #
	proc.time() - ptm 
	saveSimulationOutput(out)
	
	
	
	# WORKING ON SPACE
	input$basin<-"upper"
	inputs<- modelInputs(input,spatial=TRUE)
	inputs$version<- "01"
	inputs$nreps=5
	inputs$daug_H=60000	
	inputs$daug_N=20000	
	inputs$nyears<- 10
	
	# NEW AGE-0 STOCKING
	inputs$fingerling<- data.frame(month=10,bend=c(100,110),number=c(300,350))
	inputs$yearling<- data.frame(month=10,bend=c(120,114),number=c(300,350))
	
	dyn<- initialize(inputs=inputs) # INITIALIZE OBJECTS NEEDED FOR SIMUALTION
	dyn2<-dyn
	
	out<- sim(inputs=inputs,
		dyn=dyn,
		recruitmentFreq=0,
		sizeStructure=FALSE) #

	figures(1) # plot of abundance 
	figures(2) # plot of mean weight and length
	figures(3) # plot of biomass
	figures(4) # plot of incremental psd values
	figures(5) # plot of linf and k
	figures(6) # plot of initial length distribution
	

	
	
	
	
	
	
	
	