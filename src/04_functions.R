
### PROCESS INPUTS TO INITILIZE AND SIMULATE POPULATION	
modelInputs<- function(input){#reactive({
	tmp<-list()
	if(input$basin=="Lower"){indx<-1}else{indx<-2}
	
	# POPULATION CHARACTERISTICS
	tmp$basin= input$basin[indx]
	tmp$maxage=input$maxage[indx]
	tmp$sexratio=input$sexratio[indx]
	tmp$natural=input$natural[indx]
	tmp$hatchery=input$hatchery[indx]
	tmp$natural_age0=input$natural_age0[indx]
	tmp$hatchery_age0=input$hatchery_age0[indx]
	
	## LENGTH-WEIGHT
	tmp$a= exp(input$a_prime)[indx]
	tmp$b= input$b[indx]
	tmp$lw_er=input$lw_er[indx]
	
	# FECUNDITY
	tmp$fec_a=input$fec_a[indx]
	tmp$fec_b=input$fec_b[indx]
	tmp$fec_er=input$fec_er[indx]
	
	# GROWTH
	#k=input$k,
	#t0=input$t0,
	#linf=input$linf,
	#vb_er=input$vb_er,

	# SEXUAL MATURITY AND RETURN TO SPAWNING
	tmp$age_mat=input$age_mat[indx]
	tmp$mat_k=input$mat_k[indx]
	tmp$spn_a=input$spn_a[indx]
	tmp$spn_b=input$spn_b[indx]

	# SURVIVALS
	tmp$phi0=input$phi_age0_mean[indx] 
	tmp$phi1=input$phi_age1_mean[indx]
	tmp$phi2=input$phi_age2_mean[indx]
	tmp$phi=c(tmp$phi1,rep(tmp$phi2,tmp$maxage-1))
	
	# MOVEMENT [NOT ON UI YET]
	tmp$spread=10 #input$spread<- 10
	
	# STOCKING
	tmp$stocking_amount=input$stocking_amount
	tmp$stocking_month=input$stocking_month
	tmp$recruit_mean_length=input$recruit_mean_length
	tmp$recruit_length_sd=input$recruit_length_sd 
	tmp$stocking_bend=input$stocking_bend

	# SIMULATION STUFF
	tmp$nreps=input$nreps
	tmp$nyears=input$nyears
	tmp$daug=input$daug

	tmp$basin_inp=ifelse(input$basin=="Lower",0,1)
	# BEND DATA & META
	tmp$bend_meta<- subset(bend_meta,basin==tmp$basin)
	tmp$n_bends<- nrow(tmp$bend_meta)	
	tmp$bend_lengths<- diff(c(0,bend_meta[which(bend_meta$basin==input$basin),]$bend_start_rkm))
	
	
	
	## MOVEMENT MATRIX
	# BEND_NUM Length.RKM
	
	tmp$prob<- matrix(runif(tmp$n_bends*tmp$n_bends),nrow=tmp$n_bends,ncol=tmp$n_bends)
	tmp$prob[upper.tri(tmp$prob)]<-0
	tmp$prob<- tmp$prob/apply(tmp$prob,1,sum)
	
	# INITIALIZATION
	
	## AGE STRUCTURE
	if(input$agestructure=="Approximate equilibrium")
		{
		tmp$rel_age<- cumprod(tmp$phi)/sum(cumprod(tmp$phi))
		}
	if(input$agestructure=="Uniform")
		{
		pp<- rep(1,tmp$maxage)
		tmp$rel_age<- pp/sum(pp)
		}
	if(input$agestructure=="Random")
		{
		pp<- runif(tmp$maxage)
		tmp$rel_age<- pp/sum(pp)
		}



	## SPATIAL STRUCTURE ADULTS
	tmp$adult_spatial_structure<-input$adult_spatial_structure
	
	## SPATIAL STRUCTURE AGE-0
	if(input$age0_n_spatial_structure=="Uniform")
		{
		pp<- runif(tmp$n_bends)
		tmp$natural_age0_rel_dens<- pp/sum(pp)
		}	
	if(input$age0_h_spatial_structure=="Uniform")
		{
		pp<- runif(tmp$n_bends)
		tmp$hatchery_age0_rel_dens<- pp/sum(pp)
		}	
	
	
	return(tmp)
	} #})

	
### CORE FUNCTION TO DO SIMULATIONS	
sim<- function(inputs)
	{
	
	# SET UP MATRICES FOR SIMULATION	
	## HATCHERY ORIGIN FISH
	k_H<-Linf_H<-LEN_H<-RKM_H<-WGT_H<- matrix(0,inputs$daug,inputs$nreps)# float
	Z_H<-AGE_H<-MAT_H<-MPS_H<-SEX_H<- matrix(0L,inputs$daug,inputs$nreps)# integer
	## NATURAL ORIGIN FISH
	k_N<-Linf_N<-LEN_N<-RKM_N<-WGT_N<- matrix(0,inputs$daug,inputs$nreps)# float
	Z_N<-AGE_N<-MAT_N<-MPS_N<-SEX_N<- matrix(0L,inputs$daug,inputs$nreps)# integer

	
	# FISH ALIVE AT INITIALIZATION [Z]
	Z_H[]<- sapply(1:inputs$nreps, ini_Z,
		n=inputs$hatchery,
		fill0=inputs$daug-inputs$hatchery)
	Z_N[]<- sapply(1:inputs$nreps, ini_Z,
		n=inputs$natural,
		fill0=inputs$daug-inputs$natural)	

		
	# INIITIALIZE GROWTH COEFFICIENTS
	tmp<- ini_growth(x=inputs$nreps,
			n=nrow(Linf_H),
			basin="lower") 	
	Linf_H[]<- tmp$linf
	k_H[]<- tmp$k
	tmp<- ini_growth(x=input$nreps,
			n=nrow(Linf_H),
			basin="lower") 	
	Linf_N[]<- tmp$linf
	k_N[]<- tmp$k			

	
	# INITIALIZE LENGTH
	LEN_H[]<-sapply(1:inputs$nreps, ini_length,
		n=inputs$hatchery,
		linf= Linf_H,
		fill0=inputs$daug-inputs$hatchery,
		basin=inputs$basin)
	LEN_N[]<-sapply(1:inputs$nreps, ini_length,
		n=inputs$hatchery,
		linf= Linf_N,
		fill0=inputs$daug-inputs$hatchery,
		basin=inputs$basin)	

	# INITIALIZE WEIGHT GIVEN LENGTH
	WGT_H[]<-sapply(1:inputs$nreps, ini_wgt,
		a=inputs$a,
		b=inputs$b,
		len=LEN_H,
		live=Z_H)
	WGT_N[]<-sapply(1:inputs$nreps, ini_wgt,
		a=inputs$a,
		b=inputs$b,
		len=LEN_N,
		live=Z_N)

		
	# INITIALIZE AGE  
	AGE_N[]<- sapply(1:inputs$nreps,ini_age,
		n=inputs$natural,
		len=LEN_N,
		linf=Linf_N,
		k=k_N,
		sizeAtHatch=7,
		maxAge=inputs$maxage,
		fill0=inputs$daug-inputs$natural)
	AGE_H[]<- sapply(1:inputs$nreps,ini_age,
		n=inputs$hatchery,
		len=LEN_H,
		linf=Linf_H,
		k=k_H,
		sizeAtHatch=7,
		maxAge=inputs$maxage,
		fill0=inputs$daug-inputs$hatchery)		

	# INITIALIZE MATURITY													
	MAT_H[]<-sapply(1:inputs$nreps, ini_maturity,
		k=inputs$mat_k,
		len=LEN_H,
		age_mat=inputs$age_mat,
		live=Z_H)
	MAT_N<-sapply(1:inputs$nreps, ini_maturity,
		k=inputs$mat_k,
		len=LEN_N,
		age_mat=inputs$age_mat,
		live=Z_N)	

	# INITIALIZE TIME SINCE SPAWNING
	MPS_H[]<-sapply(1:inputs$nreps, ini_mps,
		n=inputs$daug,
		mature=MAT_H,
		live=Z_H)
	MPS_N[]<-sapply(1:inputs$nreps, ini_mps,
		n=inputs$daug,
		mature=MAT_N,
		live=Z_N)			

	# INITIALIZE SEX OF FISH
	SEX_H[]<-sapply(1:inputs$nreps,ini_sex,
		n=inputs$hatchery,
		ratio=inputs$sexratio,
		fill0=inputs$daug-inputs$hatchery)
	SEX_N[]<-sapply(1:inputs$nreps,ini_sex,
		n=inputs$natural,
		ratio=inputs$sexratio,
		fill0=inputs$daug-inputs$natural)


	# INITIALIZE LOCATION OF ADULTS
	RKM_H[]<-sapply(1:inputs$nreps,ini_rkm,
		n=inputs$hatchery,
		type=inputs$adult_spatial_structure,
		bend_lengths=inputs$bend_lengths,
		fill0=inputs$daug-inputs$hatchery)
	RKM_N[]<-sapply(1:inputs$nreps,ini_rkm,
		n=inputs$natural,
		type=inputs$adult_spatial_structure,
		bend_lengths=inputs$bend_lengths,
		fill0=inputs$daug-inputs$natural)	
		
	# INITIALIZE AGE-0 IN EACH BEND
	AGE_0_N_BND<-rmultinom(inputs$nreps,
		inputs$natural_age0,
		inputs$natural_age0_rel_dens)
	AGE_0_H_BND<-rmultinom(inputs$nreps,
		inputs$hatchery_age0,
		inputs$hatchery_age0_rel_dens)
	# END INITIALIZATION ##########	
		
		
		
		
		
		
	# VECTOR OF MONTHS
	## STARTS IN JANUARY
	m<- rep(c(1:12),inputs$nyears) 	
	
	## SUMMARIES
	### ABUNDANCE
	N_N_SUM<-colSums(Z_N)
	N_H_SUM<-colSums(Z_H)
		  
	# PROGRESS BAR
	#withProgress(message = 'Executing model ',value = 0, {	
	#k=0	
	pb<-txtProgressBar(min=1,max=length(m),initial=0,char="*",style=3)
	
	for(i in 1:length(m))
		{
		x<-runif(100000)
		
	}
	
	
	# SIMULATE POPULATION DYNAMICS GIVEN INITIAL STATES
	for(i in 1:length(m)) # M IS A VECTOR OF MONTHS 1:12, REPEATED FOR NYEARS
		{
		#k=k+1
		#incProgress(1/length(m))
		setTxtProgressBar(pb, i)
		
		# STOCKING 
		if(m[i]==inputs$stocking_month)
			{
			# ADD NUMBER OF FISH STOCKED IN A BEND
			AGE_0_H_BND[inputs$stocking_bend,]<- AGE_0_H_BND[inputs$stocking_bend,]+inputs$stocking_amount
			}
			# END STOCKING
		
		# UPDATE WHETHER FISH SURVIVED PRIOR TO UPDATING OTHER STUFF  ####fixme####
		Z_N<- sapply(1:inputs$nreps,dSurvival,
			n=inputs$daug,
			phi_age=inputs$phi,# vector of annual survivals converted to monthly in function
			age=AGE_N,
			live=Z_N) 
		# UPDATE WHETHER FISH SURVIVED PRIOR TO UPDATING OTHER STUFF ####fixme####
		Z_H<- sapply(1:inputs$nreps,dSurvival,
			n=inputs$daug,
			phi_age=inputs$phi,# vector of annual survivals converted to monthly in function
			age=AGE_H,
			live=Z_H)

		# UPDATE TOTAL LENGTH 
		LEN_H<-sapply(1:inputs$nreps,dLength,
			n=inputs$daug,
			k=k_H, 
			linf=Linf_H,
			dT=1/12,
			length1=LEN_H,
			live=Z_H)
		LEN_N<-sapply(1:inputs$nreps,dLength,
			n=inputs$daug,
			k=k_N, 
			linf=Linf_N,
			dT=1/12,
			length1=LEN_N,
			live=Z_N)	
	
		# UPDATE WEIGHT 
		WGT_H<-sapply(1:inputs$nreps,dWeight,
			n=inputs$daug,
			a=inputs$a,
			b=inputs$b,
			len=LEN_H,
			er=inputs$lw_er,
			live=Z_H)
		WGT_N<-sapply(1:inputs$nreps,dWeight,
			n=inputs$daug,
			a=inputs$a,
			b=inputs$b,
			len=LEN_N,
			er=inputs$lw_er,
			live=Z_N)
			
			
			
		if(m[i]==6)
			{
			##MOVE AGE-0 FISH INTO MATRICES
			
			## INDEX OF OPEN SLOTS TO PUT AGE-0 FISH TRANSITIONING
			## TO AGE-1.... HAPPY BIRTHDAY LITTLE FIDDIES
			## need to add error handling if recruitment is greater than slots available......#########################################################
			if(sum(colSums(AGE_0_H_BND))>0)
				{
				indx_h<- unlist(sapply(1:inputs$nreps,function(x) which(Z_H[,x]==0)[1:sum(AGE_0_H_BND[,x])]))		
				indx_h<- cbind(c(indx_h),sort(rep(1:inputs$nreps,colSums(AGE_0_H_BND))))
				Z_H[indx_h]<-1## ADD NEW 1 YEAR OLD RECRUITS
				AGE_H[indx_h]<-1# UPDATE AGE OF RECRUITS		
				LEN_H[indx_h]<-rnorm(length(indx_h[,1]),inputs$recruit_mean_length,inputs$recruit_length_sd)				
				WGT_H[indx_h]<-rlnorm(length(indx_h[,1]),log(inputs$a*LEN_H[indx_h]^inputs$b),inputs$lw_er)					
				RKM_H[indx_h]<- bend2rkm(c(unlist(sapply(1:inputs$nreps,# ASSIGN LOCATION OF RECRUITS
					function(x){rep(1:inputs$n_bends,AGE_0_H_BND[,x])}))))		
				MAT_H[indx_h]<-0# ASSIGN MATURATION STATUS OF NEW RECRUITS
				SEX_H[indx_h]<-rbinom(length(indx_h[,1]),1,0.5)# ASSIGN SEX TO RECRUITS
				}# END IF RECRUITS > 0
				
			if(sum(colSums(AGE_0_N_BND))>0)
				{
				indx_n<- unlist(sapply(1:inputs$nreps,function(x) which(Z_N[,x]==0)[1:sum(AGE_0_N_BND[,x])]))		
				indx_n<- cbind(c(indx_n),sort(rep(1:inputs$nreps,colSums(AGE_0_N_BND))))
				Z_N[indx_n]<-1## ADD NEW 1 YEAR OLD RECRUITS						
				AGE_N[indx_n]<-1# UPDATE AGE OF RECRUITS
				LEN_N[indx_n]<-rnorm(length(indx_n[,1]),inputs$recruit_mean_length,inputs$recruit_length_sd)		
				WGT_N[indx_n]<-rlnorm(length(indx_n[,1]),log(inputs$a*LEN_N[indx_n]^inputs$b),inputs$lw_er)		
				RKM_N[indx_n]<- bend2rkm(c(unlist(sapply(1:inputs$nreps,# ASSIGN LOCATION OF RECRUITS
					function(x)	{rep(1:inputs$n_bends,AGE_0_N_BND[,x])}))))			
				MAT_N[indx_n]<-0# ASSIGN MATURATION STATUS OF NEW RECRUITS			
				SEX_N[indx_n]<-rbinom(length(indx_n[,1]),1,0.5)# ASSIGN SEX TO RECRUITS
				} # END IF RECRUITS > 0
			# END RECRUITMENT
			AGE_0_N_BND[]<-0 # ZERO OUT AGE 0 AFTER THEY MOVE TO AGE-1
			AGE_0_H_BND[]<-0 # ZERO OUT AGE 0 AFTER THEY MOVE TO AGE-1	


			
			## ASSIGN WHETHER A FISH WILL SPAWN
			## GIVEN TIME SINCE LAST SPAWN
			SPN_H<-sapply(1:inputs$nreps,spawn,
				mps=MPS_H,
				a=inputs$spn_a,
				b=inputs$spn_b,
				mature=MAT_H,
				live=Z_H) 
			SPN_N<-sapply(1:inputs$nreps,spawn,
				mps=MPS_N,
				a=inputs$spn_a,
				b=inputs$spn_b,
				mature=MAT_N,
				live=Z_N) 	

				
				
			## CALCULATE THE NUMBER OF EGGS PRODUCED
			EGGS_H<- sapply(1:inputs$nreps,fecundity,
				fl=LEN_H,
				a=inputs$fec_a,
				b=inputs$fec_b,
				er=inputs$fec_er,
				sex=SEX_H,
				live=Z_H,
				spawn=SPN_H)	
			EGGS_N<- sapply(1:inputs$nreps,fecundity,
				fl=LEN_N,
				a=inputs$fec_a,
				b=inputs$fec_b,
				er=inputs$fec_er,
				sex=SEX_N,
				live=Z_N,
				spawn=SPN_N)
				
			## EGGS PER REACH
			AGE_0_BND<- sapply(1:inputs$nreps,function(x){
				N<-tapply(EGGS_N[,x],
					factor(rkm2bend(RKM_N[,x]),	levels=c(1:inputs$n_bends)),
					sum)
				N[is.na(N)]<-0
				H<-tapply(EGGS_H[,x],
					factor(rkm2bend(RKM_H[,x]),	levels=c(1:inputs$n_bends)),
					sum)
				H[is.na(H)]<-0				
				return(N+H)})	


			## FERTILIZATION
			### HOW MANY FEMALES IN EACH BEND
			FEM_BND<- sapply(1:inputs$nreps,function(x){
				N<-tapply(Z_H[,x]*SEX_H[,x],
					factor(rkm2bend(RKM_N[,x]),	levels=c(1:inputs$n_bends)),
					sum)
				N[is.na(N)]<-0
				H<-tapply(Z_N[,x]*SEX_N[,x],
					factor(rkm2bend(RKM_H[,x]),	levels=c(1:inputs$n_bends)),
					sum)
				H[is.na(H)]<-0				
				return(N+H)})
				
				
				
			### HOW MANY MALES IN EACH BEND
			MAL_BND<- sapply(1:inputs$nreps,function(x){
				N<-tapply(Z_H[,x]*(1-SEX_H[,x])*MAT_H[,x],
					factor(rkm2bend(RKM_N[,x]),	levels=c(1:inputs$n_bends)),
					sum)
				N[is.na(N)]<-0
				H<-tapply(Z_N[,x]*(1-SEX_N[,x])*MAT_N[,x],
					factor(rkm2bend(RKM_H[,x]),	levels=c(1:inputs$n_bends)),
					sum)
				H[is.na(H)]<-0				
				return(N+H)})			
  

				

			## DRIFT OF FREE EMBRYOS 
			## [need to add survival] ######################################################################
			AGE_0_N_BND<-sapply(1:inputs$nreps,dFreeEmbryoDrift,
				nbends=nrow(AGE_0_N_BND),
				loc=AGE_0_N_BND,
				prob=inputs$prob)				
			} #
			# END JUNE SPAWNING
			
		
		## TRANSITION OF FREE EMBRYO TO EXOGENOUSLY FEEDING LARVAE & AGE-0
		AGE_0_N_BND<- sapply(1:inputs$nreps,dFEtoEFL,
			n=nrow(AGE_0_N_BND),
			total=AGE_0_N_BND,
			phi=inputs$phi3^(1/12))	

		# UPDATE MONTHS SINCE SPAWNING
		MPS_N<- sapply(1:inputs$nreps,dMPS,
			mps=MPS_N,
			mature=MAT_N,
			live=Z_N)
		MPS_N<- sapply(1:inputs$nreps,dMPS,
			mps=MPS_N,
			mature=MAT_N,
			live=Z_N)	
		# UPDATE MOVEMENT
		
		## ADULT MOVEMENT
		RKM_H<-sapply(1:inputs$nreps,dRKM,
			n=nrow(RKM_H),
			loc=RKM_H,
			live=Z_H,
			er=inputs$spread)			
		RKM_N<-sapply(1:inputs$nreps,dRKM,
			n=nrow(RKM_N),
			loc=RKM_N,
			live=Z_N,
			er=inputs$spread)
		
		## UPDATE ANY AGE-0 MOVEMENT ######################################################################
		#AGE_0_N<-	
		#AGE_0_H<-	
		
		## SUMMARIES
		### ABUNDANCE
		N_N_SUM<-rbind(N_N_SUM,colSums(Z_N))
		N_H_SUM<-rbind(N_H_SUM,colSums(Z_H))
		
	}# end i
	
	##})# end progress bar
	
	return(list(natural=N_N_SUM, hatchery=N_H_SUM))	
	}

	
##### END FUNCTION	