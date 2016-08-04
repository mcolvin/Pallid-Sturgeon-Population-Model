

### CORE FUNCTION TO DO SIMULATIONS	
sim<- function(inputs,dyn)
	{
	## assign objects from dyn
	k_H<-dyn$k_H
	k_N<-dyn$k_N
	
	Linf_H<-dyn$Linf_H
	Linf_N<-dyn$Linf_N
	
	LEN_H<-dyn$LEN_H
	LEN_N<-dyn$LEN_N
	
	WGT_H<-dyn$WGT_H
	WGT_N<-dyn$WGT_N
	
	Z_H<-dyn$Z_H
	Z_N<-dyn$Z_N
	
	AGE_H<-dyn$AGE_H
	AGE_N<-dyn$AGE_N

	MAT_H<-dyn$MAT_H
	MAT_N<-dyn$MAT_N
	
	MPS_H<-dyn$MPS_H
	MPS_N<-dyn$MPS_N
	
	SEX_H<-dyn$SEX_H
	SEX_N<-dyn$SEX_N
	
	SPN_H<-dyn$SPN_H
	SPN_N<-dyn$SPN_N
	
	EGGS_H<-dyn$EGGS_H
	EGGS_N<-dyn$EGGS_N
	
	AGE_0_N_BND<-dyn$AGE_0_N_BND
	AGE_0_H_BND<-dyn$AGE_0_H_BND
	
	m<-dyn$m
	
	rm(list='dyn')
	
	
	# SUMMARIES
	RECRUITS<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	
	MEANWEIGHT_N<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	MEANWEIGHT_H<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	
	BIOMASS_N<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	BIOMASS_H<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	
	N_NAT<- matrix(0,nrow=length(m),ncol=inputs$nreps)
	N_HAT<- matrix(0,nrow=length(m),ncol=inputs$nreps)


	#sq<-qp<-pm<-mt<-tr<-matrix(0,ncol=input$nreps,nrow=length(m))# MATRIX
	#init_summary<- data.frame(len=LEN[,1],
	#	linf=Linf[,1],
	#	k=k[,1],
	#	age=AGE[,1])
	

	# PROGRESS BAR
	pb<-txtProgressBar(min=1,max=length(m),initial=0,char="*",style=3)
	
	#Rprof("out.out") # for profiling in for loop

	# SIMULATE POPULATION DYNAMICS GIVEN INITIAL STATES
	for(i in 1:length(m)) 
		{
		setTxtProgressBar(pb, i)

		indx_H<- lapply(1:inputs$nreps,
			function(x){which(Z_H[,x]==1)}) # ROW INDICES
		tmp<- unlist(lapply(1:inputs$nreps,
			function(x) rep(x,length(indx_H[[x]])))) # COLUMN INDICES
		indx_H<- cbind(unlist(indx_H),tmp)#row,column
	
		indx_N<- lapply(1:inputs$nreps,
			function(x){which(Z_N[,x]==1)}) # ROW INDICES
		tmp<- unlist(lapply(1:inputs$nreps,
			function(x) rep(x,length(indx_N[[x]])))) # COLUMN INDICES
		indx_N<- cbind(unlist(indx_N),tmp)#row,column
	
	
		### UPDATE TOTAL LENGTH 
		LEN_H[indx_H]<-dLength(k=k_H[indx_H],
			linf=Linf_H[indx_H],
			dT=1/12,
			length1=LEN_H[indx_H])
		LEN_N[indx_N]<-dLength(k=k_N[indx_N],
			linf=Linf_N[indx_N],
			dT=1/12,
			length1=LEN_N[indx_N])			
			
			
		### UPDATE WEIGHT # slow
		WGT_H[indx_H]<-dWeight(len=LEN_H[indx_H],
			a=inputs$a,
			b=inputs$b,
			er=inputs$lw_er)
		WGT_N[indx_N]<-dWeight(len=LEN_N[indx_N],
			a=inputs$a,
			b=inputs$b,
			er=inputs$lw_er)
			
		### UPDATE WHETHER A FISH WILL SPAWN
		SPN_H[indx_H]<-spawn(mps=MPS_H[indx_H],a=inputs$spn_a,
			b=inputs$spn_b,
			mature=MAT_H[indx_H])*inputs$recruitment
		SPN_N[indx_N]<-spawn(mps=MPS_N[indx_N],a=inputs$spn_a,
			b=inputs$spn_b,
			mature=MAT_N[indx_N])*inputs$recruitment

			
		### RECRUIT AGE-0 FISH TO THE POPULATION; NATURAL AND HATCHERY
		#if(m[i]==6) 
			#{
			#dyn<- recruitment_to_population(spatial=inputs$spatial)
			#}

			
		## UPDATE THE NUMBER OF EGGS IN A FEMALE  ####slow####
		EGGS_H[indx_H]<-fecundity(fl=LEN_H[indx_H],
			a=inputs$fec_a,
			b=inputs$fec_b,
			er=inputs$fec_er,
			sex=SEX_H[indx_H],
			spawn=SPN_H[indx_H],
			mature=MAT_H[indx_H])	
		EGGS_N[indx_N]<-fecundity(fl=LEN_N[indx_N],
			a=inputs$fec_a,
			b=inputs$fec_b,
			er=inputs$fec_er,
			sex=SEX_N[indx_N],
			spawn=SPN_N[indx_N],
			mature=MAT_N[indx_N])			
		
		
		## UPDATE THE NUMBER OF MONTHS SINCE SPAWNING FOR FISH JUST SPAWNED
		MPS_H[EGGS_H>0]<-0
		MPS_N[EGGS_N>0]<-0
		
		AGE_0_N_BND<- matrix(colSums(EGGS_N)+colSums(EGGS_H),nrow=1) # NUMBER OF EGGS
		AGE_0_N_BND[]<- rbinom(inputs$nreps,
			AGE_0_N_BND,
			inputs$pr_embryo) # eggs --> embryos
		AGE_0_N_BND[]<- rbinom(inputs$nreps,
			AGE_0_N_BND,
			inputs$phi_embryo) # embryos --> free embryos
		AGE_0_N_BND[]<- rbinom(inputs$nreps,
			AGE_0_N_BND,
			inputs$phi_free_embryo) # free embryos --> age0
			
		# UPDATE MONTHS SINCE SPAWNING
		MPS_H[indx_H]<-MPS_H[indx_H]+1 	
		MPS_N[indx_N]<-MPS_N[indx_N]+1 	
		
		
		# PALLID STURGEON STOCKING 
	if(inputs$fingerling > 0 & inputs$fingerling_month==m[i])
		{
		# ADD NUMBER OF FISH STOCKED IN A BEND
		dyn$AGE_0_H_BND[inputs$stocking_bend,]<- AGE_0_H_BND[inputs$stocking_bend,]+inputs$fingerling
		}

	## YEARLING STOCKING (AGE-1+); INDIVIDUALS
	if(inputs$yearling>0 & inputs$yearling_month==m[i])
		{
		### GET INDEXES OF OPEN SLOTS TO STICK STOCKED INDIVIDUALS
		indx_R<- lapply(1:inputs$nreps,
			function(x){out<- which(Z_H[,x]==0)[1:inputs$yearling]}) 
		indx_R<- cbind(unlist(indx_R),
			sort(rep(1:inputs$nreps,inputs$yearling)))
		Z_H[indx_R]<- 1### ADD NEWLY STOCKED INDIVIDUALS TO Z_H
		LEN_H[indx_R]<- rnorm(inputs$yearling*inputs$nreps,
			inputs$yearling_mn,
			inputs$yearling_sd)# UPDATE LENGTH
		WGT_H[indx_R]<- rlnorm(length(indx_R[,1]),
			log(inputs$a*LEN_H[indx_R]^inputs$b),
			inputs$lw_er)	# UPDATE 
		AGE_H[indx_R]<- inputs$yearling_age # ASSIGN AGE
		MAT_H[indx_R]<- 0	# ASSIGN MATURITY
		#RKM[indx_R]<- inputs$yearling_stocking_rkm# ASSIGN LOCATION OF STOCKED INDIVIDUALS
		MAT_H[indx_R]<- 0  # ASSIGN MATURATION STATUS OF NEW RECRUITS
		SEX_H[indx_R]<-rbinom(length(indx_R[,1]),
			n=1,
			p=0.5)# ASSIGN SEX TO 
		}
		
		
		# need to draw new linf and k for new fish...				



		# SUMMARIES #####################################################################
		## ABUNDANCE AGE-1+
		N_NAT[i,]<-colSums(Z_N) 
		N_HAT[i,]<-colSums(Z_H) 
		## BIOMASS
		BIOMASS_N[i,]<- colSums(WGT_N)
		BIOMASS_N[i,]<- colSums(WGT_H)
		## MEAN WEIGHT
		MEANWEIGHT_N[i,]<- colSums(WGT_N)/colSums(Z_N)	
		MEANWEIGHT_H[i,]<- colSums(WGT_H)/colSums(Z_H)
		## AGE-1 RECRUITS; NATURAL ORIGIN
		indx_R<- lapply(1:inputs$nreps,function(x)
			{
			out<- which(AGE_N[,x]>0 & AGE_N[,x]<24 & Z_N[,x]==1)
			}) 		
		RECRUITS[i,]<- sapply(1:inputs$nreps,
			function(x) length(indx_R[[x]]))

		# END SUMMARIES #################################################################
		}# end i 
		#Rprof(NULL)
		#summaryRprof(out.out”)


	out<-list(
		total_N=N_NAT,
		total_H=N_HAT,
		months=m,
		years=cumsum(m/12))
	#	sq=sq,qp=qp,pm=pm,mt=mt,tr=tr,
	#	biomass=biomass,
	#	mn_wght=mn_wght,
	#	init_summary=init_summary)
		
	#fn<-paste0("./output/",inputs$output_name,"/",inputs$output_name,"-output.txt")
	#dput(out,fn)
	#fn<-paste0("./output/",inputs$output_name,"/",inputs$output_name,"-input.txt")
	#dput(inputs,fn)
	#fn<-paste0("./output/",inputs$output_name,"/",inputs$output_name,".Rdata")
	#save(out,inputs, file=fn)
	return(out)	
	}

