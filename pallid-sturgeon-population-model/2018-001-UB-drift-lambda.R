
    repo <- git2r::repository("..")## REPO IS 1 LEVEL UP; USE ".." FOR 1 UP

    ## SOURCE THE GLOBAL SCRIPT FOR THE WEB APPLICATION
    source("global.R")
	input$output_name<- "2018-001" 
 	input$commit<- capture.output(git2r::summary(repo))[3] ## GET THE GIT COMMIT ID FROM REPO FOR REPRODUCABILITY
	input$basin<-"upper"  
    
	# CREATE DIRECTORY IF NOT ALREADY THERE
	dir.create(file.path(paste0(getwd(),"/output/",input$output_name)), showWarnings = FALSE) 
	
	# PREPARE INPUTS FOR MODEL
	inputs<- modelInputs(input,spatial=FALSE)
	inputs$nreps=5
	inputs$daug_H=60000	
	inputs$daug_N=20000	
	inputs$nyears<- 20
    # INITIALIZE OBJECTS NEEDED FOR SIMUALTION
	dyn<- initialize(inputs=inputs) 
	dyn2<-dyn
	
    
    
    dyn$AGE_0_N_BND[]<-0 ## ZERO OUT ANY NATURAL AGE-0 IN THE SYSTEMS
    ## AGE-0 SURVIVAL [EGGS--> AGE-1]
    p<-0.8
    inputs$pr_embryo<-p/4   
    inputs$phi_embryo<-p/4 
    inputs$phi_free_embryo<-p/4
    inputs$p_retained<-1 ## proportion retained in basin
    inputs$phi0 <-p/4   

	ptm <- proc.time()
	out<- sim(inputs=inputs,
		dyn=dyn,
		recruitmentFreq=1,
        sizeStructure=FALSE) # 0 IS FIXED RECRUITMENT
	proc.time() - ptm 
	#saveSimulationOutput(out)

    NN<-out$total_N+out$total_H
    matplot(NN/1000,type='l',ylim=c(0,max(NN)/1000),las=1,
        ylab="Abundance (x1000)",xlab="Time (months)",cex.lab=1.3)
    
    
    
	
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
	

	
	
	
	
	
	
	
	