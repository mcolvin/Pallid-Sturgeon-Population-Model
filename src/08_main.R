
	setwd("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Pallid-Sturgeon-Population-Model/")
	source("./src/01_global.R")# need to get run_sourcefunction
	
	# RUN MODEL
	inputs<-run_source(commit="Missing",output_name="2016-001",run_report=FALSE)
	out<- sim(inputs=inputs)




	# COMPILE *.RMD TO DOCX
	setwd("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Pallid-Sturgeon-Population-Model/output/2016-001")		
	knitr::knit(list.files(path='.',pattern="[.]Rmd"))	
	knitr::pandoc(list.files(path='.',pattern="[.]md"), format='docx')	
