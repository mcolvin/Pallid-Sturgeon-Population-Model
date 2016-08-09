
	setwd("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Pallid-Sturgeon-Population-Model/Pallid-Sturgeon-Population-Model/")
	source("global.R")
	input$output_name<- "2016-002" #AFS-KC ANALYSIS
	input$commit<- "0df8618"
	
	# CREATE DIRECTORY IF NOT ALREADY THERE
	dir.create(file.path(paste0(getwd(),"/output/",input$output_name)), showWarnings = FALSE) 
	input$basin<-"upper"
	inputs<- modelInputs(input,spatial=FALSE)
	inputs$nreps=10
	inputs$daug_H=60000	
	inputs$daug_N=20000	
	inputs$nyears<- 20
	dyn<- initialize(inputs=inputs) # INITIALIZE OBJECTS NEEDED FOR SIMUALTION
	dyn2<-dyn
	

	ptm <- proc.time()
	out<- sim(inputs=inputs,dyn=dyn,recruitmentFreq=0) #
	proc.time() - ptm 

	


	

	source("./tables.R")
	source("./figures.R")
	setwd("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Pallid-Sturgeon-Population-Model/output/2016-001")
	fn<-list.files(path='.',pattern="[.]Rdata")
	load(file=fn)
	figures(1)
	figures(2)
	figures(3)
	figures(4)
	figures(5) # plot of linf and k
	figures(6) # plot of initial length distributation

	# COMPILE *.RMD TO DOCX
	setwd("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Pallid-Sturgeon-Population-Model/output/2016-001")		
	knitr::knit(list.files(path='.',pattern="[.]Rmd"))	
	knitr::pandoc(list.files(path='.',pattern="[.]md"), format='docx')	
 
	
	
	
	
	