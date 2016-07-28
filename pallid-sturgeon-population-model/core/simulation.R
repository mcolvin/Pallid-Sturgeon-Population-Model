

### CORE FUNCTION TO DO SIMULATIONS	
sim<- function(inputs,dyn)
	{
	## assign objects from dyn
	k<-dyn$k
	Linf<-dyn$Linf
	LEN<-dyn$LEN
	WGT<-dyn$WGT
	Z<-dyn$Z
	AGE<-dyn$AGE
	MAT<-dyn$MAT
	MPS<-dyn$MPS
	SEX<-dyn$SEX
	SPN<-dyn$SPN
	ORIGIN<-dyn$ORIGIN
	EGGS<-dyn$EGGS
	AGE_0_N_BND<-dyn$AGE_0_N_BND
	AGE_0_H_BND<-dyn$AGE_0_H_BND
	m<-dyn$m
	rm(list='dyn')
	# SUMMARIES
	## ABUNDANCE (AGE-1+)
	N_SUM<-colSums(Z)

	## AGE-1 RECRUITS; NATURAL ORIGIN
	recruits<-	lapply(AGE*(1-ORIGIN),function(x) length(x[x>0 & x<24]))


	#sq<-qp<-pm<-mt<-tr<-matrix(0,ncol=input$nreps,nrow=length(m))# MATRIX
	#mn_wght<-biomass<- matrix(0,nrow=length(m),ncol=inputs$nreps)
	#init_summary<- data.frame(len=LEN[,1],
	#	linf=Linf[,1],
	#	k=k[,1],
	#	age=AGE[,1])
	
	# PROGRESS BAR
	pb<-txtProgressBar(min=1,max=length(m),initial=0,char="*",style=3)

	
#Rprof("out.out") # for profiling in for loop

	# SIMULATE POPULATION DYNAMICS GIVEN INITIAL STATES
	for(i in 1:length(m)) # M IS A VECTOR OF MONTHS 1:12, REPEATED FOR NYEARS
		{
		
		setTxtProgressBar(pb, i)

		indx<- lapply(1:inputs$nreps,function(x){which(Z[,x]==1)}) 
		tmp<- unlist(lapply(1:inputs$nreps,function(x) length(indx[[x]])))
		indx<- cbind(unlist(indx),sort(rep(1:inputs$nreps,tmp)))
	
		### UPDATE WHETHER FISH SURVIVED PRIOR TO UPDATING OTHER STUFF  
		Z[indx]<- dSurvival(phi_age=inputs$phi,age=AGE[indx])   
		# USING WHICH WAS FASTER TO GET INDEX AND APPLY TO INDEX.
		
		### UPDATE TOTAL LENGTH 
		LEN[indx]<-dLength(k=k[indx],
			linf=Linf[indx],
			dT=1/12,
			length1=LEN[indx])		
		### UPDATE WEIGHT # slow
		WGT[indx]<-dWeight(len=LEN[indx],
			a=inputs$a,
			b=inputs$b,
			er=inputs$lw_er)
		### RECRUIT AGE-0 FISH TO THE POPULATION; NATURAL AND HATCHERY
		#if(m[i]==6) 
			#{
			#dyn<- recruitment_to_population(spatial=inputs$spatial)
			#}

		## ASSIGN WHETHER A FISH WILL SPAWN
		## GIVEN TIME SINCE LAST SPAWN
		SPN[indx]<-spawn(mps=MPS[indx],a=inputs$spn_a,
			b=inputs$spn_b,
			mature=MAT[indx])*inputs$recruitment
			
		## CALCULATE THE NUMBER OF EGGS IN A FEMALE
		EGGS[indx]<-fecundity(fl=LEN[indx],
			a=inputs$fec_a,
			b=inputs$fec_b,
			er=inputs$fec_er,
			sex=SEX[indx],
			spawn=SPN[indx],
			mature=MAT[indx])	
		
		## UPDATE THE NUMBER OF MONTHS SINCE SPAWNING FOR FISH JUST SPAWNED
		MPS[EGGS>0]<-0
		
		AGE_0_N_BND<- matrix(colSums(EGGS),nrow=1) # NUMBER OF EGGS
		AGE_0_N_BND[]<- rbinom(inputs$nreps,AGE_0_N_BND,inputs$pr_embryo) # eggs --> embryos
		AGE_0_N_BND[]<- rbinom(inputs$nreps,AGE_0_N_BND,inputs$phi_embryo) # embryos --> free embryos
		AGE_0_N_BND[]<- rbinom(inputs$nreps,AGE_0_N_BND,inputs$phi_free_embryo) # free embryos --> age0
			
		# UPDATE MONTHS SINCE SPAWNING
		MPS[indx]<-MPS[indx]+1 	
		
		
		# PALLID STURGEON STOCKING ###############################################################################
		## STOCKING OCCURS AT THE END OF THE MONTH
		## NOT SUBJECT TO MORTALITY OR MOVEMENT
		## FINGERLING STOCKING (AGE-0)
		#stocking(fingerlings=inputs$fingerling, 
		#	fingerlings_month=inputs$fingerling_month,
		#	yearlings=inputs$yearling,
		#	yearlings_month=inputs$yearling_month,
		#	month=dyn$m[i])
		
		# SENESCENCE
		if(length(which(AGE>=inputs$maxage*12))>0)
			{
			Z[AGE>=inputs$maxage*12]<- 0
			}
		# ZERO OUT LENGTH AND WEIGHTS FOR DEAD FISH
		LEN<- LEN*Z
		WGT<- WGT*Z
		# need to draw new linf and k for new fish...				



		# SUMMARIES #####################################################################
		## ABUNDANCE
		N_SUM<-rbind(N_SUM,colSums(Z))  # TOTAL AGE-1+ ABUNDANCE
		
		## AGE-1 RECRUITS; NATURAL ORIGIN
		indx<- lapply(1:inputs$nreps,function(x)
			{
			out<- which(AGE[,x]>0 & AGE[,x]<24 & ORIGIN[,x]==0 & Z[,x]==1)
			}) 		
		recruits<- rbind(recruits,sapply(1:inputs$nreps,function(x) length(indx[[x]])))
		biomass[i,]<- colSums(WGT)
		mn_wght[i,]<- colSums(WGT)/colSums(Z)
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
