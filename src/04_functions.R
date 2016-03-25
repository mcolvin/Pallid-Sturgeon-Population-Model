### VERSION 2

# to do
sim_pop<- function(inputs=input)
	{

	#inputs=input

	# NUMBER OF ADULTS IN EACH BEND GIVEN DENSITY AND BEND LENGTH
	N_n<- rmultinom(inputs$nreps,
		inputs$natural,inputs$rel_density)
	N_h<- rmultinom(inputs$nreps,
		inputs$hatchery,inputs$rel_density)	
		
	# SET UP MATRICES FOR SIMULATION	
	Z_H<-AGE_H<-LEN_H<-MAT_H<-RKM_H<-MPS_H<-WGT_H<-SEX_H<- matrix(0,inputs$daug,inputs$nreps)
	Z_N<-AGE_N<-LEN_N<-MAT_N<-RKM_N<-MPS_N<-WGT_N<-SEX_N<- matrix(0,inputs$daug,inputs$nreps)

	
	
	
	# FISH ALIVE AT INITIALIZATION [Z]
	Z_H[]<- sapply(1:inputs$nreps, ini_Z,
		n=inputs$hatchery,
		fill0=inputs$daug-inputs$hatchery)
	Z_N[]<- sapply(1:inputs$nreps, ini_Z,
		n=inputs$natural,
		fill0=inputs$daug-inputs$natural)



	# INITIALIZE AGE
	AGE_N[]<- sapply(1:inputs$nreps,ini_age,
		rel_age=inputs$rel_age,
		maxage=inputs$maxage,
		n=inputs$natural,
		fill0=inputs$daug-inputs$natural)
	AGE_H[]<- sapply(1:inputs$nreps,ini_age,
		rel_age=inputs$rel_age,
		maxage=inputs$maxage,
		n=inputs$hatchery,
		fill0=inputs$daug-inputs$hatchery)	

	
	
	# INITIALIZE LENGTH
	LEN_H[]<-sapply(1:inputs$nreps, ini_length,
		linf=inputs$linf ,
		k=inputs$k,
		t0=inputs$t0,
		age=AGE_H,
		live=Z_H)
	LEN_N[]<-sapply(1:inputs$nreps, ini_length,
		linf=inputs$linf ,
		k=inputs$k,
		t0=inputs$t0,
		age=AGE_N,
		live=Z_N)


		
	# INITIALIZE MATURITY
	MAT_H[]<-sapply(1:inputs$nreps, ini_maturity,
		k=inputs$mat_k,
		len=LEN_H,
		age_mat=input$age_mat,
		live=Z_H)
	MAT_N<-sapply(1:inputs$nreps, ini_maturity,
		k=inputs$mat_k,
		len=LEN_N,
		age_mat=input$age_mat,
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

		

	# USING FUNCTIONS TO INITIALIZE
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
		fill0=inputs$daug-inputs$hatchery)
	RKM_N[]<-sapply(1:inputs$nreps,ini_rkm,
		n=inputs$natural,
		fill0=inputs$daug-inputs$natural)		
	
	
	
	# INITIALIZE AGE-0 IN EACH BEND
	AGE_0_N_BND<-rmultinom(inputs$nreps,inputs$natural_age0,inputs$natural_age0_rel_dens)
	AGE_0_H_BND<-rmultinom(inputs$nreps,inputs$hatchery_age0,inputs$hatchery_age0_rel_dens)
	# END INITIALIZATION ##########
	
	
	
	# VECTOR OF MONTHS
	## STARTS IN JANUARY
	m<- rep(c(1:12),input$nyears) 	
	
	## SUMMARIES
	### ABUNDANCE
	N_N_SUM<-colSums(Z_N)
	N_H_SUM<-colSums(Z_H)
	
	
	# SIMULATE POPULATION DYNAMICS GIVEN INITIAL STATES
	for(i in m) # M IS A VECTOR OF MONTHS 1:12, REPEATED FOR NYEARS
		{
		
		# STOCKING 
		if(i==inputs$stocking_month)
			{
			# ADD NUMBER OF FISH STOCKED IN A BEND
			AGE_0_H_BND[inputs$stocking_bend,]<- AGE_0_H_BND[inputs$stocking_bend,]+inputs$stocking_amount
			}
			# END STOCKING
		
		# UPDATE WHETHER FISH SURVIVED PRIOR TO UPDATING OTHER STUFF
		Z_N<- sapply(1:inputs$nreps,dSurvival,
			n=input$daug,
			phi_age=inputs$phi^(1/12),# vector of age survivals
			age=AGE_N,
			live=Z_N)
		# UPDATE WHETHER FISH SURVIVED PRIOR TO UPDATING OTHER STUFF
		Z_H<- sapply(1:inputs$nreps,dSurvival,
			n=input$daug,
			phi_age=inputs$phi^(1/12),# vector of age survivals
			age=AGE_H,
			live=Z_H)


			
		# UPDATE TOTAL LENGTH 
		LEN_H<-sapply(1:inputs$nreps,dLength,
			n=input$daug,
			k=inputs$k, 
			linf=inputs$linf,
			dT=1/12,
			length1=LEN_H,
			er=0.2,
			live=Z_H)
		LEN_N<-sapply(1:inputs$nreps,dLength,
			n=input$daug,
			k=inputs$k, 
			linf=inputs$linf,
			dT=1/12,
			length1=LEN_N,
			er=0.2,
			live=Z_N)		


			
		# UPDATE WEIGHT 
		WGT_H<-sapply(1:inputs$nreps,dWeight,
			n=inputs$daug,
			a=inputs$a,
			b=inputs$b,
			len=LEN_H,
			er=0.1,
			live=Z_H)
		WGT_N<-sapply(1:inputs$nreps,dWeight,
			n=inputs$daug,
			a=inputs$a,
			b=inputs$b,
			len=LEN_N,
			er=0.1,
			live=Z_N)


			
		# UPDATE SEXUAL MATURITY 
		if(i==6)
			{
			##MOVE AGE-0 FISH INTO MATRICES
			
			## INDEX OF OPEN SLOTS TO PUT AGE-0 FISH TRANSITIONING
			## TO AGE-1.... HAPPY BIRTHDAY LITTLE FIDDIES
			## need to add error handling if recruitment is greater than slots available......#########################################################
			if(sum(colSums(AGE_0_H_BND))>0){
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
			}# END AIF RECRUITS > 0
			
			if(sum(colSums(AGE_0_N_BND))>0){
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
				a=-17.5,
				b=0.35,
				mature=MAT_H,
				live=Z_H) 
			SPN_N<-sapply(1:inputs$nreps,spawn,
				mps=MPS_N,
				a=-17.5,
				b=0.35,
				mature=MAT_N,
				live=Z_N) 	


				
			## CALCULATE THE NUMBER OF EGGS PRODUCED
			EGGS_H<- sapply(1:inputs$nreps,fecundity,
				len=LEN_H,
				wgt=WGT_H,
				a=inputs$fec_a,
				b=inputs$fec_b,
				er=inputs$fec_er,
				sex=SEX_H,
				live=Z_H,
				spawn=SPN_H)
			EGGS_N<- sapply(1:inputs$nreps,fecundity,
				len=LEN_N,
				wgt=WGT_N,
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


				
			## SET MONTHS POST SPAWN TO -1 
			## FOR FISH THAT SPAWNED
			MPS_N[SPN_N==1]<- -1
			MPS_H[SPN_H==1]<- -1
			

			
			## EMBRYOS 
			### FERTILIZATON OF EGGS TO EMBRYOS
			AGE_0_N_BND<- sapply(1:inputs$nreps,function(x)
				{
				a<- -3
				b<- 0.5
				tmp<-rbinom(nrow(AGE_0_N_BND),AGE_0_BND[,x],plogis(a+b*MAL_BND[,x]))
				return(tmp)
				})


				
			## EMBRYOS SURVIVING TO FREE EMBRYOS
			AGE_0_N_BND<- sapply(1:inputs$nreps,function(x)
				{
				tmp<-rbinom(nrow(AGE_0_N_BND),AGE_0_BND[,x],inputs$phi_1)
				return(tmp)
				})			

				
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
			phi=inputs$phi_1^(1/12))	

			
			
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
		
	}
return(list(natural=N_N_SUM, hatchery=N_H_SUM))}# END FUNCTION

	
	