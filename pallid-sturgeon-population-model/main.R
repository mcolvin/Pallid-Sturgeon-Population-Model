
	setwd("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Pallid-Sturgeon-Population-Model/")
	source("./src/01_global.R")# need to get run_sourcefunction
	input$output_name<- "tmp"
	input$commit<- "0df8618"
	dir.create(file.path(paste0(getwd(),"/output/",input$output_name)), showWarnings = FALSE) # CREATE DIRECTORY IF NOT ALREADY THERE

	input$basin<-"Upper"
	input$nreps=10
	out<- sim(inputs=input)
  
	
	
setwd("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Pallid-Sturgeon-Population-Model/")
source("./src/01_global.R")
source("./src/05_tables.R")
source("./src/06_figures.R")
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
 
	
	
	
	
	