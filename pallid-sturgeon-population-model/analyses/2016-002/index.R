
	setwd("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Pallid-Sturgeon-Population-Model/Pallid-Sturgeon-Population-Model/")
	source("global.R")
	input$output_name<- "2016-002" #AFS-KC ANALYSIS
	input$commit<- "0df8618"
	# CREATE DIRECTORY IF NOT ALREADY THERE
	dir.create(file.path(paste0(getwd(),"/output/",input$output_name)), showWarnings = FALSE) 
	

	
	# OBJECTIVES
	
	## HOW MANY FISH TO STOCK TO GET LAMBDA > 1
	## IF RECRUITMENT OCCURS EVERY 1, 5, 10 YEARS
	## EFFECTS OF INCLUDING SPACE
	

	sims<- expand.grid(basin=c("Lower","Upper"),fingerlings=c(100,500,1000),yearlings=c(0,100,500,1000))
	
	
	
	input$basin<-"upper"
	inputs<- modelInputs(input, 
		spatial=FALSE) #
		
		
	inputs$nreps=10
	inputs$daug_H=45000	
	inputs$daug_N=100	
	inputs$nyears<- 50
	# INITIALIZE OBJECTS NEEDED FOR SIMUALTION
	dyn<- initialize(inputs=inputs) 
	
	inputs$yearling<-0
	inputs$fingerling<-0
	ptm <- proc.time()
	out<- sim(inputs=inputs,dyn=dyn, recruitmentFreq=0) #
	proc.time() - ptm 

	matplot(out$years,out$mean_weight/1000,type='l')
	matplot(out$years,out$mean_length,type='l')
	matplot(out$years,(out$biomass_n+out$biomass_h)/1000,type='l')
	matplot(out$years,out$total_N+out$total_H,type='l')


	
	
	hist(out$init_summary$len)
	hist(out$init_summary$linf)
	
	
	
	
	
	
	
	

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
 
	
	
	
	
	