

### CORE FUNCTION TO DO SIMULATIONS	
sim<- function(inputs,dyn,recruitmentFreq=0,sizeStructure=TRUE)
	{
	inputs$recruitmentFreq<-recruitmentFreq
	inputs$sizeStructure<-sizeStructure
	
	## ASSIGN OBJECTS FROM DYN INPUT
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
	

	if(inputs$spatial==TRUE)
		{
		BEND_N <- dyn$BEND_N
		BEND_H <- dyn$BEND_H
		}
	
	#rm(list='dyn')	
	
	# SET UP SUMMARIES
	RECRUITS<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	
	MEANWEIGHT_N<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	MEANWEIGHT_H<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	MEANWEIGHT<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	
	MEANLENGTH_N<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	MEANLENGTH_H<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	MEANLENGTH<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	
	BIOMASS_N<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	BIOMASS_H<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	BIOMASS<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	
	N_NAT<- matrix(0,nrow=length(m),ncol=inputs$nreps)
	N_HAT<- matrix(0,nrow=length(m),ncol=inputs$nreps)

	
	if(inputs$sizeStructure==TRUE)
		{
		stock<-matrix(0L,ncol=inputs$nreps,	nrow=length(m))# MATRIX
		quality<-matrix(0L,ncol=inputs$nreps,	nrow=length(m))# MATRIX
		preferred<-matrix(0L,ncol=inputs$nreps,	nrow=length(m))# MATRIX
		memorable<-matrix(0L,ncol=inputs$nreps,	nrow=length(m))# MATRIX
		trophy<-matrix(0L,ncol=inputs$nreps,	nrow=length(m))# MATRIX
		}
	init_summary<- data.frame(
		len=c(LEN_H[1:inputs$hatchery,1],LEN_N[1:inputs$natural,1]),
		wgt=c(WGT_H[1:inputs$hatchery,1],WGT_N[1:inputs$natural,1]),
		linf=c(Linf_H[1:inputs$hatchery,1],Linf_N[1:inputs$natural,1]),
		k=c(k_H[1:inputs$hatchery,1],k_N[1:inputs$natural,1]),
		age=c(AGE_H[1:inputs$hatchery,1],AGE_N[1:inputs$natural,1]))
	# END SUMMARIES

	
	# SET UP INDICES PRIOR TO RUNNING MODEL
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
		

	# PROGRESS BAR
	pb<-txtProgressBar(min=1,max=length(m),initial=0,char="*",style=3)
	
	#Rprof("out.out") # for profiling in for loop

	# SIMULATE POPULATION DYNAMICS GIVEN INITIAL STATES
	for(i in 1:length(m)) 
		{
		setTxtProgressBar(pb, i)
		
		Z_H[indx_H]<- dSurvival(phi_age=inputs$phi,
			age=AGE_H[indx_H],
			maxAge=inputs$maxage)
		
		Z_N[indx_N]<- dSurvival(phi_age=inputs$phi,
			age=AGE_N[indx_N],
			maxAge=inputs$maxage)

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

			
		LEN_H<- LEN_H*Z_H
		LEN_N<- LEN_N*Z_N
		WGT_H<- WGT_H*Z_H
		WGT_N<- WGT_N*Z_N
			
		### UPDATE TOTAL LENGTH 
		LEN_H[indx_H]<-dLength(k=k_H[indx_H],
			linf=Linf_H[indx_H],
			dT=1/12,
			length1=LEN_H[indx_H])
		LEN_N[indx_N]<-dLength(k=k_N[indx_N],
			linf=Linf_N[indx_N],
			dT=1/12,
			length1=LEN_N[indx_N])			
		
		
	#	xx<-data.frame(len1=LEN_N[indx_N],len2=dLength(k=k_N[indx_N],
	#		linf=Linf_N[indx_N],
	#		dT=60,
	#		length1=LEN_N[indx_N]),
	#		linf=Linf_N[indx_N])
	#	xx$weight<-dWeight(len=xx$len2,
	#		a=inputs$a,
	#		b=inputs$b,
	#		er=inputs$lw_er)/1000

		
		### UPDATE WEIGHT # slow
		WGT_H[indx_H]<-dWeight(len=LEN_H[indx_H],
			a=inputs$a,
			b=inputs$b,
			er=inputs$lw_er)
		WGT_N[indx_N]<-dWeight(len=LEN_N[indx_N],
			a=inputs$a,
			b=inputs$b,
			er=inputs$lw_er)
			



		# SPATIAL 
		if(inputs$spatial==TRUE)
			{
			## ADULT MOVEMENT
			BEND_H[indx_H]<- adultMovement(previousLocation=BEND_H[indx_H],
				fromToMatrix=inputs$adult_prob)
			BEND_N[indx_N]<- adultMovement(previousLocation=BEND_N[indx_N],
				fromToMatrix=inputs$adult_prob)
			}

			
		#compare <- microbenchmark::microbenchmark(
		#	a=adultMovement(previousLocation=BEND_H[indx_H],
		#		fromToMatrix=inputs$adult_prob),
		#	b=adultMovement_cmpld(previousLocation=BEND_H[indx_H],
		#		fromToMatrix=inputs$adult_prob), times =20)
		#ggplot2::autoplot(compare)
			
		### RECRUIT AGE-0 FISH TO THE POPULATION; NATURAL AND HATCHERY
		#if(m[i]%in% c(6,7,8)) 
			#{
			#dyn<- recruitment_to_population(spatial=inputs$spatial)
			#}
		if(inputs$recruitmentFreq>0 & m[i]%in% c(6,7,8))
			{
			### UPDATE WHETHER A FISH WILL SPAWN 
			SPN_H[indx_H]<-spawn(mps=MPS_H[indx_H],a=inputs$spn_a,
				b=inputs$spn_b,
				mature=MAT_H[indx_H])
			SPN_N[indx_N]<-spawn(mps=MPS_N[indx_N],a=inputs$spn_a,
				b=inputs$spn_b,
				mature=MAT_N[indx_N])
			
			
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


			}		
			

		
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
		
		
	
		# SUMMARIES #####################################################################
		## ABUNDANCE AGE-1+
		N_NAT[i,]<-colSums(Z_N) 
		N_HAT[i,]<-colSums(Z_H) 
		## BIOMASS IN KILOGRAMS
		BIOMASS_N[i,]<- colSums(WGT_N)/1000
		BIOMASS_H[i,]<- colSums(WGT_H)/1000
		BIOMASS[i,]<- colSums(WGT_H)+colSums(WGT_N)/1000 
		## MEAN WEIGHT IN KILOGRAMS
		MEANWEIGHT_N[i,]<- (colSums(WGT_N)/colSums(Z_N))/1000 
		MEANWEIGHT_H[i,]<- (colSums(WGT_H)/colSums(Z_H))/1000 
		MEANWEIGHT[i,]<-((colSums(WGT_H)+colSums(WGT_N))/(colSums(Z_H)+colSums(Z_N)))/1000 
		
		## MEAN LENGTH IN MM
		MEANLENGTH_N[i,]<- colSums(LEN_N)/colSums(Z_N)	
		MEANLENGTH_H[i,]<- colSums(LEN_H)/colSums(Z_H)
		MEANLENGTH[i,]<- (colSums(LEN_H)+ colSums(LEN_N))/(colSums(Z_H)+colSums(Z_N))
		
		
		## AGE-1 RECRUITS; NATURAL ORIGIN
		indx_R<- lapply(1:inputs$nreps,function(x)
			{
			out<- which(AGE_N[,x]>0 & AGE_N[,x]<24 & Z_N[,x]==1)
			}) 		
		RECRUITS[i,]<- sapply(1:inputs$nreps,
			function(x)
				{ 
				length(indx_R[[x]])
				})
		if(inputs$sizeStructure==TRUE)
			{	
			stock[i,]<- sapply(1:inputs$nreps,function(x)
				{
				length(LEN_H[LEN_H[,x]>330,x])+length(LEN_N[LEN_N[,x]>330,x])
				})	# 330
			quality[i,]<-sapply(1:inputs$nreps,function(x)
				{
				length(LEN_H[LEN_H[,x]>630,x])+length(LEN_N[LEN_N[,x]>630,x])
				}) 			# 630
			preferred[i,]<- sapply(1:inputs$nreps,function(x)
				{
				length(LEN_H[LEN_H[,x]>840,x])+length(LEN_N[LEN_N[,x]>840,x])
				})		# 840
			memorable[i,]<- sapply(1:inputs$nreps,function(x)
				{
				length(LEN_H[LEN_H[,x]>1040,x])+length(LEN_N[LEN_N[,x]>1040,x])
				})		# 1040
			trophy[i,]<- sapply(1:inputs$nreps,function(x)
				{
				length(LEN_H[LEN_H[,x]>1270,x])+length(LEN_N[LEN_N[,x]>1270,x])
				})			# 1270
			}	
			

		# END SUMMARIES #################################################################
		}# end i 
		#Rprof(NULL)
		#summaryRprof(out.out”)

	out<-list(
		total_N=N_NAT,
		total_H=N_HAT,
		months=m,
		recruits=RECRUITS,
		biomass_n=BIOMASS_N,
		biomass_h=BIOMASS_H,
		biomass=BIOMASS,
		mean_weight_n=MEANWEIGHT_N,
		mean_weight_h=MEANWEIGHT_H,
		mean_weight=MEANWEIGHT,
		mean_length_n=MEANLENGTH_N,
		mean_length_h=MEANLENGTH_H,
		mean_length=MEANLENGTH,
		post_length=c(LEN_H[LEN_H[,1]>0,1],LEN_N[LEN_N[,1]>0,1]), # LENGTH DISTRIBUTION POST SIMULATION
		post_weight=c(WGT_H[WGT_H[,1]>0,1],WGT_N[WGT_N[,1]>0,1]), # WEIGHT DISTRIBUTION POST SIMULATION
		years=inputs$startYear+cumsum(rep(1,length(m))/12),
		init_summary=init_summary)
		
		if(inputs$sizeStructure==TRUE)
			{	
			out$stock=stock
			out$quality=quality
			out$preferred=preferred
			out$memorable=memorable
			out$trophy=trophy
			}
	fn0<-paste0("./analyses/",inputs$output_name,"/",inputs$output_name,"-",inputs$version,"/")
	fn<-paste0(fn0,inputs$output_name,"-",inputs$version,"-output.rds")
	saveRDS(list(output=out, input=inputs),fn)
	return(out)	
	}
