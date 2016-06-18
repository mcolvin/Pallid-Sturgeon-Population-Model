
	setwd("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Pallid-Sturgeon-Population-Model/")
	source("./src/01_global.R")# need to get run_sourcefunction
	output_name<- "2016-001"
	current_commit<- "e2d502d"
	dir.create(file.path(paste0(getwd(),"/output/",output_name)), showWarnings = FALSE) # CREATE DIRECTORY IF NOT ALREADY THERE
	
	
	# RUN MODEL
	inputs<-run_source(commit=current_commit,output_name=output_name)
	inputs$nreps=100
	out<- sim(inputs=inputs)

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

	dir(all.files=TRUE)
	
	dir("./.git")
	
	ss<- readLines(con = "./.git/logs")

	
	system('git log --since=5.days --author="$(git config user.name)"')