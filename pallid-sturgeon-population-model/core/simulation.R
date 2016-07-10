

### CORE FUNCTION TO DO SIMULATIONS	
sim<- function(inputs)
	{
	# SUMMARIES
	## ABUNDANCE (AGE-1+)
	N_SUM<-colSums(dyn$Z)

	## AGE-1 RECRUITS; NATURAL ORIGIN
	indx<- lapply(1:inputs$nreps,function(x){out<- which(dyn$AGE[,x]>0 & dyn$AGE[,x]<24 & 
		dyn$Z[,x]==1 & dyn$ORIGIN[,x]==0)}) 
	
	recruits<- matrix(sapply(1:inputs$nreps,function(x) length(indx[[x]])),nrow=1)
	sq<-qp<-pm<-mt<-tr<-matrix(0,ncol=input$nreps,nrow=length(m))# MATRIX
	mn_wght<-biomass<- matrix(0,nrow=length(dyn$m),ncol=inputs$nreps)
	init_summary<- data.frame(len=dyn$LEN[,1],
		linf=dyn$Linf[,1],
		k=dyn$k[,1],
		age=dyn$AGE[,1])
	
	# PROGRESS BAR
	pb<-txtProgressBar(min=1,max=length(dyn$m),initial=0,char="*",style=3)

	
#Rprof("out.out") # for profiling in for loop

	# SIMULATE POPULATION DYNAMICS GIVEN INITIAL STATES
	for(i in 1:length(dyn$m)) # M IS A VECTOR OF MONTHS 1:12, REPEATED FOR NYEARS
		{
		
		setTxtProgressBar(pb, i)

		indx<- lapply(1:inputs$nreps,function(x){which(dyn$Z[,x]==1)}) 
		tmp<- unlist(lapply(1:inputs$nreps,function(x) length(indx[[x]])))
		indx<- cbind(unlist(indx),sort(rep(1:inputs$nreps,tmp)))
	
		### UPDATE WHETHER FISH SURVIVED PRIOR TO UPDATING OTHER STUFF  
		dyn$Z[indx]<- dSurvival(phi_age=inputs$phi,age=dyn$AGE[indx])   
		# USING WHICH WAS FASTER TO GET INDEX AND APPLY TO INDEX.
		
		### UPDATE TOTAL LENGTH 
		dyn$LEN[indx]<-dLength(k=dyn$k[indx],
			linf=dyn$Linf[indx],
			dT=1/12,
			length1=dyn$LEN[indx])		
		### UPDATE WEIGHT # slow
		dyn$WGT[indx]<-dWeight(len=dyn$LEN[indx],
			a=inputs$a,
			b=inputs$b,
			er=inputs$lw_er)
		### RECRUIT AGE-0 FISH TO THE POPULATION; NATURAL AND HATCHERY
		if(dyn$m[i]==6) 
			{
			dyn<- recruitment_to_population(spatial=inputs$spatial)
			}

		## ASSIGN WHETHER A FISH WILL SPAWN
		## GIVEN TIME SINCE LAST SPAWN
		dyn$SPN[indx]<-spawn(mps=dyn$MPS[indx],a=inputs$spn_a,
			b=inputs$spn_b,
			mature=dyn$MAT[indx])*inputs$recruitment
			
		## CALCULATE THE NUMBER OF EGGS IN A FEMALE
		dyn$EGGS[indx]<-fecundity(fl=dyn$LEN[indx],
			a=inputs$fec_a,
			b=inputs$fec_b,
			er=inputs$fec_er,
			sex=dyn$SEX[indx],
			spawn=dyn$SPN[indx],
			mature=dyn$MAT[indx])	
		
		## UPDATE THE NUMBER OF MONTHS SINCE SPAWNING FOR FISH JUST SPAWNED
		dyn$MPS[dyn$EGGS>0]<-0
		
		dyn$AGE_0_N_BND<- matrix(colSums(dyn$EGGS),nrow=1) # NUMBER OF EGGS
		dyn$AGE_0_N_BND[]<- rbinom(inputs$nreps,dyn$AGE_0_N_BND,inputs$pr_embryo) # eggs --> embryos
		dyn$AGE_0_N_BND[]<- rbinom(inputs$nreps,dyn$AGE_0_N_BND,inputs$phi_embryo) # embryos --> free embryos
		dyn$AGE_0_N_BND[]<- rbinom(inputs$nreps,dyn$AGE_0_N_BND,inputs$phi_free_embryo) # free embryos --> age0
			
		# UPDATE MONTHS SINCE SPAWNING
		dyn$MPS[indx]<-dyn$MPS[indx]+1 	
		
		
		# PALLID STURGEON STOCKING ###############################################################################
		## STOCKING OCCURS AT THE END OF THE MONTH
		## NOT SUBJECT TO MORTALITY OR MOVEMENT
		## FINGERLING STOCKING (AGE-0)
		dyn<-stocking(fingerlings=inputs$fingerling, 
			fingerlings_month=inputs$fingerling_month,
			yearlings=inputs$yearling,
			yearlings_month=inputs$yearling_month,
			month=dyn$m[i])
		
		# SENESCENCE
		if(length(which(dyn$AGE>=inputs$maxage*12))>0)
			{
			dyn$Z[dyn$AGE>=inputs$maxage*12]<- 0
			}
		# ZERO OUT LENGTH AND WEIGHTS FOR DEAD FISH
		dyn$LEN<- dyn$LEN*dyn$Z
		dyn$WGT<- dyn$WGT*dyn$Z
		# need to draw new linf and k for new fish...				



		# SUMMARIES #####################################################################
		## ABUNDANCE
#		N_SUM<-rbind(N_SUM,colSums(dyn$Z))  # TOTAL AGE-1+ ABUNDANCE
		
		## AGE-1 RECRUITS; NATURAL ORIGIN
#		indx<- lapply(1:inputs$nreps,function(x)
	#		{
#			out<- which(dyn$AGE[,x]>0 & dyn$AGE[,x]<24 & dyn$ORIGIN[,x]==0 & dyn$Z[,x]==1)
#			}) 		
#		recruits<- rbind(recruits,sapply(1:inputs$nreps,function(x) length(indx[[x]])))
#		biomass[i,]<- colSums(dyn$WGT)
#		mn_wght[i,]<- colSums(dyn$WGT)/colSums(dyn$Z)
		# END SUMMARIES #################################################################
		}# end i 
#Rprof(NULL)
#summaryRprof("out.out")
	
	x<- sort(rep(inputs$startYear:(inputs$startYear+inputs$nyears-1),12))+rep(1:12/12,inputs$nyears)
	out<-list(total=N_SUM, 
		years=x,
		sq=sq,qp=qp,pm=pm,mt=mt,tr=tr,
		biomass=biomass,
		mn_wght=mn_wght,
		init_summary=init_summary)
		
	#fn<-paste0("./output/",inputs$output_name,"/",inputs$output_name,"-output.txt")
	#dput(out,fn)
	#fn<-paste0("./output/",inputs$output_name,"/",inputs$output_name,"-input.txt")
	#dput(inputs,fn)
	#fn<-paste0("./output/",inputs$output_name,"/",inputs$output_name,".Rdata")
	#save(out,inputs, file=fn)
	return(out)	
	}
