
run_analysis<- function(
	subdir="lower",
	ncpus=3,
	nreps=500,
	inputs=input)	
	{
	# MAKE A SUB DIRECTORY IF 
	# ONE DOES NOT EXIST ALREADY
	fp<- paste("./output/",subdir,sep="")
	if(subdir %in% dir("./output")==FALSE) 
		{
		dir.create(fp) 
		}
	# CHECK TO SEE WHAT REPLICATES HAVE BEEN DONE
	indx<- c(1:nreps)
	if(exists("fn")){rm(list=c("fn"))}
	fnames<-dir(fp)
	fnames<-fnames[grep(".csv",fnames)]
	if(length(fnames)>0){
	fn<- unlist(lapply(c(1:length(fnames)), function(x) 
		{unlist(strsplit(unlist(strsplit(fnames[x],"_"))[2],"[.]"))[1]}))
		fn<-unique(as.numeric(na.omit(fn)))}
	if(exists("fn")){indx<- indx[!(indx%in%fn)]}

	# STOCKING
	stocking<- rbind(data.frame(age0=seq(0,40,2)*1000,age1= 0),
		data.frame(age0=0,age1= seq(2,14,2)*1000))
	# SET UP FOR MULTICORE
	sfInit(parallel=T, cpus=ncpus)
	sfExportAll()
	sfLibrary(triangle)
	sfLibrary(data.table)
	sfClusterSetupRNG()
	# RUN THE FUNCTION run_mc on 4 cores
	result <- sfLapply(indx, 
		function(j){
			tmp<- sample(1:nrow(stocking),1)
			inputs$age0_stock<- stocking[tmp,1] 
			inputs$age1_stock<- stocking[tmp,2]
			inputs$spawn_frequency<-sample(c(1,2,5,10,20),1,replace=TRUE)
			output<-xx_ind(input=inputs)
			write.csv(output$xx, 
				paste(fp,"/output_",j,".csv",sep=""))
			write.csv(output$out_vals, 
				paste(fp,"/inputs_",j,".csv",sep=""))
			rm(list=c("output"))
		})
	sfStop()
	}

	
	
subdir<-"wh_lower"	
yy<- run_analysis(subdir=subdir,
	ncpus=4,
	nreps=500,
	inputs=input_low_wh)
fp<- paste("./output/",subdir,sep="")
tmp<- tables(1)# compile inputs
write.csv(tmp,
	file="C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Pallid-Sturgeon-Stocking-Assessment/dat/lower_wf_inputs.csv")
	tmp<- tables(2)
	write.csv(tmp,
		file="C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Pallid-Sturgeon-Stocking-Assessment/dat/lower_wf_sims.csv")

		
subdir<-"wh_upper"
yy<- run_analysis(subdir=subdir,
	ncpus=4,
	nreps=500,
	inputs=input_low_wh)
fp<- paste("./output/",subdir,sep="")
tmp<- tables(1)# compile inputs
write.csv(tmp,
	file="C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Pallid-Sturgeon-Stocking-Assessment/dat/upper_wf_inputs.csv")
tmp<- tables(2)
write.csv(tmp,
	file="C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Pallid-Sturgeon-Stocking-Assessment/dat/upper_wf_sims.csv")
#########################################################	
	 
	
	


# DELETE SIMULATION REPOSITORY?
	delete_all_output("N") # N=NO, Y=YES
	if(length(fnames)>0){
	fn<- unlist(lapply(c(1:length(fnames)), function(x) 
		{unlist(strsplit(unlist(strsplit(fnames[x],"_"))[2],"[.]"))[1]}))
	fn<-unique(as.numeric(fn))}



	# CHANGE VARIABLES FOR ANALYSIS
	nreps<- 9000
	ncpus<-4
	
	 
	# AGE-0 STOCKING LOWER BASIN	
	indx<- c(1:nreps)
	if(exists("fn")){indx<- indx[!(indx%in%fn)]}
	# SET UP FOR MULTICORE
	sfInit(parallel=T, cpus=ncpus)
	sfExportAll()
	sfLibrary(triangle)
	sfLibrary(data.table)
	sfClusterSetupRNG()
	# RUN THE FUNCTION run_mc on 4 cores
	result <- sfLapply(indx, 
		function(j){
			input$age0_stock<- sample(seq(0,40,10)*1000,1) # range of age-0 is 0 to 40k from Steffansons model
			input$age1_stock<- 0 #round(runif(1,0,14000))# range of age-0 is 0 to 40k from Steffansons model
			input$spawn_frequency<-sample(c(1,2,5,10,20),1,replace=TRUE)
			output<-xx_ind(input=input)
			write.csv(output$xx, paste("./output/output_",j,".csv",sep=""))
			write.csv(output$out_vals, paste("./output/inputs_",j,".csv",sep=""))
			rm(list=c("output"))
		})
	sfStop()

	
# AGE-1 STOCKING LOWER BASIN
	indx<- c(10000:(10000+nreps))
	if(exists("fn")){indx<- indx[!(indx%in%fn)]}
	# SET UP FOR MULTICORE
	sfInit(parallel=T, cpus=ncpus)
	sfExportAll()
	sfLibrary(triangle)
	sfLibrary(data.table)
	sfClusterSetupRNG()
	# RUN THE FUNCTION run_mc on 4 cores
	result <- sfLapply(indx, 
		function(j){
			input$age0_stock<- 0 # range of age-0 is 0 to 40k from Steffansons model
			input$age1_stock<- sample(seq(2,14,2)*1000,1)# range of age-0 is 0 to 14k from Steffansons model
			input$spawn_frequency<-sample(c(1,2,5,10,20),1,replace=TRUE)
			output<-xx_ind(input=input)
			write.csv(output$xx, paste("./output/output_",j,".csv",sep=""))
			write.csv(output$out_vals, paste("./output/inputs_",j,".csv",sep=""))
			rm(list=c("output"))
		})
	sfStop()
	

	
##### UPPER BASIN
# whats been done?
	fnames_up<-dir("./output/upper")
	if(length(fnames_up)>0){
	fn_up<- unlist(lapply(c(1:length(fnames_up)), function(x) 
		{unlist(strsplit(unlist(strsplit(fnames_up[x],"_"))[2],"[.]"))[1]}))
	fn_up<-unique(as.numeric(fn_up))}



	# CHANGE VARIABLES FOR ANALYSIS
	nreps<- 5000
	ncpus<-4
	
	 
	# AGE-0 STOCKING LOWER BASIN	
	indx<- c(1:nreps)
	if(exists("fn_up")){indx<- indx[!(indx%in%fn_up)]}
	# SET UP FOR MULTICORE
	sfInit(parallel=T, cpus=ncpus)
	sfExportAll()
	sfLibrary(triangle)
	sfLibrary(data.table)
	sfClusterSetupRNG()
	# RUN THE FUNCTION run_mc on 4 cores
	result <- sfLapply(indx, 
		function(j){
			input$age0_stock<- sample(seq(0,40,10)*1000,1) # range of age-0 is 0 to 40k from Steffansons model
			input$age1_stock<- 0 #round(runif(1,0,14000))# range of age-0 is 0 to 40k from Steffansons model
			input$spawn_frequency<-sample(c(1,2,5,10,20),1,replace=TRUE)
			output<-xx_ind(input=input)
			write.csv(output$xx, paste("./output/upper/output_",j,".csv",sep=""))
			write.csv(output$out_vals, paste("./output/upper/inputs_",j,".csv",sep=""))
			rm(list=c("output"))
		})
	sfStop()

	
# AGE-1 STOCKING LOWER BASIN
	indx<- c(10000:(10000+nreps))
	if(exists("fn_up")){indx<- indx[!(indx%in%fn_up)]}
	# SET UP FOR MULTICORE
	sfInit(parallel=T, cpus=ncpus)
	sfExportAll()
	sfLibrary(triangle)
	sfLibrary(data.table)
	sfClusterSetupRNG()
	# RUN THE FUNCTION run_mc on 4 cores
	result <- sfLapply(indx, 
		function(j){
			input$age0_stock<- 0 # range of age-0 is 0 to 40k from Steffansons model
			input$age1_stock<- sample(seq(2,14,2)*1000,1)# range of age-0 is 0 to 14k from Steffansons model
			input$spawn_frequency<-sample(c(1,2,5,10,20),1,replace=TRUE)
			output<-xx_ind(input=input)
			write.csv(output$xx, paste("./output/upper/output_",j,".csv",sep=""))
			write.csv(output$out_vals, paste("./output/upper/inputs_",j,".csv",sep=""))
			rm(list=c("output"))
		})
	sfStop()
		
	
	
	
	
	
	
	
	
	
# dont delete this	
lambda<-dcast(out,year~rep,value.var="total",mean)
vals<-(lambda[-1,]+1)/(lambda[-nrow(lambda),]+1)
g_mn<- data.frame(scenario="Median stocking", 
mn_lambda=apply(vals,2,function(x) exp(mean(log(x)))))	
	
	





	# VISUALIZING UNCERTAINTY
	mn<-log(-input$S0/(input$S0-1))
	x <- seq(0.000000001,0.2,length=10000)
	x_l<- log(-x/(x-1))
	hx <- dnorm(x_l,mn,abs(input$S0_cv*mn))# uncertainty as CV
	plot(x, hx, type="l")


	
	# VISUALIZING UNCERTAINTY
	mn<-log(-input$S1/(input$S1-1))
	x <- seq(0.000000001,0.999999,length=10000)
	x_l<- log(-x/(x-1))
	hx <- dnorm(x_l,mn,abs(input$S1_cv*mn))# uncertainty as CV
	plot(x, hx, type="l")	
	
	# VISUALIZING UNCERTAINTY
	mn<-log(-input$S2/(input$S2-1))
	x <- seq(0.000000001,0.999999,length=10000)
	x_l<- log(-x/(x-1))
	hx <- dnorm(x_l,mn,abs(input$S2_cv*mn))# uncertainty as CV
	plot(x, hx, type="l")		

	# VISUALIZING UNCERTAINTY
	mn<-log(-input$S3plus/(input$S3plus-1))
	x <- seq(0.000000001,0.999999,length=10000)
	x_l<- log(-x/(x-1))
	hx <- dnorm(x_l,mn,abs(input$S3plus_cv*mn))# uncertainty as CV
	plot(x, hx, type="l")


	# VISUALIZING UNCERTAINTY
	mn<-log(-input$viableGam/(input$viableGam-1))
	x <- seq(0.000000001,0.999999,length=10000)
	x_l<- log(-x/(x-1))
	hx <- dnorm(x_l,mn,abs(input$viableGam_cv*mn))# uncertainty as CV
	plot(x, hx, type="l")
	
	
	
	
	
	
	
	
	
	
	
	
	