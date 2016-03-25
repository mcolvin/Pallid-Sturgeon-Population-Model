
# to do
# 1. add stocking component
#    figure out how to assign bend and rkm
#    need to add a hatchery age 0 module, with link for size, weight, hatchery...
#    maybe as a vector of each, but with a vector for reps???
# RUN A MODEL
out<- sim_pop(inputs=input)
 matplot(out$hatchery,type='l')
 matplot(out$natural,type='l')








### VERSON 2

# SAMPLING INPUTS
sampling_inputs<- expand.grid(p=c(0.1,0.4),
	nsec=c(2,4,6,8,10),
	distribution_type=c("uniform","heterogeneous"),
	n_reaches=c(5,10,15,20,25,30,40, 50,100,150,200),
	nprim=5)


# SIMULATE CAPTURE HISTORIES
#ppp<- sim_ch(input)
#ch<- ppp$ch_out	
 
## ESTIMATE POPULATION 
	
# SET UP A HETERGENEOUS POPULATION
# AND CLOSURE WITHIN REACH
# ASSUMES REACH IS CLOSED TO EMIGRATION AND MORTALITY
# ASSUMES CONSTANT CAPTURE PROBABILITY
dat<-data.frame()
pb <- txtProgressBar(min = 1, 
	max = nrow(input), style = 3)
for(s in 1:nrow(input))
	{
	for(r in 1:50)
		{
		N<- input$N[s]
		n_reaches<-input$n_reaches[s]
		x<- seq(input$seg_start[s],input$seg_end[s],input$reach_length[s])
		xx<- c(1:length(x))# index reaches
		n_occ<- input$n_occ[s]
		# reach_locations<- sort(sample(x,n_reaches, replace=FALSE))
		p_capture<- matrix(input$p[s],length(xx),n_occ)
		# ASSIGN FISH TO A REACH
		p_loc<- runif(length(xx))
		p_loc<- p_loc/sum(p_loc)
		loc<- cbind(xx,rmultinom(1,N,p_loc))

		
		loc<- loc[loc[,2]>0,]
		loc<- rep(loc[,1],loc[,2]) # what reach the fish is in
		
		tmmpp<- as.data.frame(table(loc))
		#plot(lo
		ch<- matrix(0,N,n_occ)
		for(k in 1:n_occ)
			{
			ch[,k]<-rbinom(N,1,p_capture[loc,k])		
			}
		flag<- apply(ch,1,sum)
		# SAMPLE REACHES
		sample_reaches<- sample(xx,input$n_reaches[s],replace=FALSE)
		# KEEP REACH INDEX
		INDX<- which(loc %in% sample_reaches & flag>0)
		loc_sampled<- loc[INDX]
		ch_sampled<- ch[INDX,]
		app<- out<-data.frame()
		if(class(ch_sampled)=="matrix" & length(INDX)>10)
			{
			out<-closedp.t(ch_sampled)
			app<- as.data.frame(out$results)
			app$model<- rownames(out$results)
			app$r<- r
			app$s<-s
			dat<- rbind.fill(dat,app)		
			}
		}
	setTxtProgressBar(pb, s)
	}


out<- merge(input,dat, by="s",all.x=TRUE)
out_dt<- data.table(out)
write.csv(out,"./PSPAP_sim.csv")

out<- fread("./PSPAP_sim.csv")
# SUBSET OUT Mo
#test<- out_dt[,.SD[which.min(AIC)],by=list(s,r)]
test<- subset(out, model=="M0")
test$tmp<-1
#dput(as.data.frame(test),"./sims_out.txt")
#yyy<-data.table(dget("./sims_out.txt"))

yyy<-test
plotdat<- yyy[,list(mn_abund=mean(abundance),pr_success=(sum(tmp)/50)),
	by=list(s,p,n_occ,n_reaches,N,reach_length,model)]
plotdat$mn_abund<-ifelse(plotdat$mn_abund>8000,8000,plotdat$mn_abund)
plotdat$n_reaches_per<- plotdat$n_reaches/200*100
presPlot()
xyplot(pr_success~n_reaches_per|as.factor(p),plotdat,type='b',group=n_occ,
	xlab="Percent of river sampled",ylab="More than 10 fish captured")	
savePlot("./figure-3.1.wmf",type="wmf")	
dev.off()
presPlot()
xyplot(mn_abund~n_reaches_per|as.factor(p),plotdat,type='b',group=n_occ,
	ylab="Expected abundance",xlab="Percent of river sampled")	
savePlot("./figure-3.2.wmf",type="wmf")
dev.off()

# WEIGHT ESTIMATES BY THE INVERSE OF THE PROPORTION OF STREAM SAMPLED
presPlot()
plotdat$n_hat<- plotdat$mn_abund/(plotdat$n_reaches_per/100)
xyplot(n_hat~n_reaches_per|as.factor(p),plotdat,type='b',group=n_occ,
	ylab="Expected abundance",xlab="Percent of river sampled",ylim=c(0,4000))	
savePlot("./figure-3.2a.wmf",type="wmf")
dev.off()


	
### END VERSION 2
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
	
	
	
	
	
	
	
	
	
	
	
	
	