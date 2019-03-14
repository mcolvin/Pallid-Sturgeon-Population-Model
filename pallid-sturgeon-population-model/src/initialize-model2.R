### INITIALIZE MODEL OBJECTS
initialize<- function(inputs)
	{
	# SET UP LIST FOR SIMULATION
	dyn<- list(
		k_H = matrix(0,inputs$daug_H,inputs$nreps),
		k_N = matrix(0,inputs$daug_N,inputs$nreps),
		Linf_H = matrix(0,inputs$daug_H,inputs$nreps),
		Linf_N = matrix(0,inputs$daug_N,inputs$nreps),
		LEN_H = matrix(0,inputs$daug_H,inputs$nreps),
		LEN_N = matrix(0,inputs$daug_N,inputs$nreps),
		WGT_H = matrix(0,inputs$daug_H,inputs$nreps),
		WGT_N = matrix(0,inputs$daug_N,inputs$nreps),
		Z_H = matrix(0L,inputs$daug_H,inputs$nreps),
		Z_N = matrix(0L,inputs$daug_N,inputs$nreps),
		AGE_H = matrix(0L,inputs$daug_H,inputs$nreps),
		AGE_N = matrix(0L,inputs$daug_N,inputs$nreps),
		MAT_H = matrix(0L,inputs$daug_H,inputs$nreps),
		MAT_N = matrix(0L,inputs$daug_N,inputs$nreps),
		MPS_H = matrix(0L,inputs$daug_H,inputs$nreps),
		MPS_N = matrix(0L,inputs$daug_N,inputs$nreps),
		SEX_H = matrix(0L,inputs$daug_H,inputs$nreps),
		SEX_N = matrix(0L,inputs$daug_N,inputs$nreps),
		SPN_H = matrix(0L,inputs$daug_H,inputs$nreps),
		SPN_N = matrix(0L,inputs$daug_N,inputs$nreps),
		EGGS_H = matrix(0L,inputs$daug_H,inputs$nreps),
		EGGS_N = matrix(0L,inputs$daug_N,inputs$nreps))
	inputs$hatchery<-matrix(0, nrow=1, ncol=inputs$nreps)
	if(inputs$genetics)
	{
	  dyn$MOM_H<- matrix(0L,inputs$daug_H,inputs$nreps)
	  dyn$DAD_H<- matrix(0L,inputs$daug_H,inputs$nreps)
	  dyn$TAG_N<- matrix(0L,inputs$daug_H,inputs$nreps)
	}
	if(inputs$hatchery_name)
	{
	  dyn$HATCH<- matrix(0L,inputs$daug_H,inputs$nreps)
	}

	# INITIALIZATION
	## [1] ASSIGN LIVE OR DEAD TO NATURAL ORIGIN FISH	
	dyn$Z_N[1:inputs$natural,]<-1

	for(j in 1:inputs$nreps)
	{
	  ## [0] GENERATE HATCHERY ORIGIN DATA
	  ini_H<-ini_hatchery(inputs$stockingHistory,
	                      inputs$genetics,
	                      inputs$hatchery_name)
	  inputs$hatchery[,j]<-nrow(ini_H)
	  indxH<-sample(1:nrow(ini_H), nrow(ini_H))
	  
	  ## [1] ASSIGN LIVE OR DEAD TO HATCHERY ORIGIN FISH	
	  dyn$Z_H[1:nrow(ini_H),j]<-1
	  
		## [2] INIITIALIZE GROWTH COEFFICIENTS
		### ASSUMES GROWTH IS NOT HERITABLE
		tmp<- ini_growth(n=inputs$daug_H,
			mu_ln_Linf=inputs$ln_Linf_mu,
			mu_ln_k=inputs$ln_k_mu,
			vcv=inputs$vcv,
			maxLinf=inputs$maxLinf) 
		dyn$Linf_H[,j]<-tmp$linf
		dyn$k_H[,j]<-tmp$k
		dyn$Linf_H[1:nrow(ini_H),j]<-ifelse(ini_H$L[indxH]<dyn$Linf_H[1:nrow(ini_H),j],
		                       dyn$Linf_H[1:nrow(ini_H),j], 
		                       ini_H$L[indxH]*1.1)
		
		tmp<- ini_growth(n=inputs$daug_N,
			mu_ln_Linf=inputs$ln_Linf_mu,
			mu_ln_k=inputs$ln_k_mu,
			vcv=inputs$vcv,
			maxLinf=inputs$maxLinf) 
		dyn$Linf_N[,j]<-tmp$linf
		dyn$k_N[,j]<-tmp$k	
		
	
		## [3] INITIALIZE LENGTH
		### HATCHERY ORIGIN LENGTH FROM DATA AND RANDOM GROWTH COEFFICIENTS
		dyn$LEN_H[1:nrow(ini_H),j]<-dLength(dyn$k_H[1:nrow(ini_H),j], 
		                                    dyn$Linf_H[1:nrow(ini_H),j],
		                                    ini_H$L[indxH],
		                                    ini_H$dA[indxH])
		### NATURAL ORIGIN LENGTH FROM DISTRIBUTION
		dyn$LEN_N[,j]<-dyn$Z_N[,j]*ini_length(n=inputs$daug_N, 
		                                      basin=inputs$basin,
		                                      origin=0, # 1 FOR HATCHERY, 0 FOR NATURAL
		                                      spatial=FALSE)
		dyn$Linf_N[,j]<-ifelse(dyn$LEN_N[,j]<dyn$Linf_N[,j],
		                       dyn$Linf_N[,j], 
		                       dyn$LEN_N[,j]*1.1)
		
		
		## [6] INITIALIZE AGE
		### HATCHERY ORIGIN AGE IN MONTHS FROM DATA
		dyn$AGE_H[1:nrow(ini_H),j]<-ini_H$A[indxH]
		### NATURAL ORIGIN AGE IN MONTHS FROM LENGTH
		dyn$AGE_N[,j]<-ini_age(len=dyn$LEN_N[,j],
		                       linf=dyn$Linf_N[,j],
		                       k=dyn$k_N[,j],
		                       sizeAtHatch=7,
		                       maxAge=inputs$maxage)	

		
		
		## [4] INITIALIZE WEIGHT GIVEN LENGTH
		### ASSUMES NO EFFECT OF ORIGIN
		dyn$WGT_H[1:nrow(ini_H),j]<-dyn$Z_H[1:nrow(ini_H),j]*
		  ini_wgt(a=inputs$a,
		          b=inputs$b,
				      len=dyn$LEN_H[1:nrow(ini_H),j],
				      er=inputs$lw_er)
		dyn$WGT_N[1:inputs$natural,j]<-dyn$Z_N[1:inputs$natural,j]*
		  ini_wgt(a=inputs$a,
		          b=inputs$b,
				      len=dyn$LEN_N[1:inputs$natural,j],
				      er=inputs$lw_er)				
	
				
		## [5] INITIALIZE SEX
		dyn$SEX_H[,j]<-dyn$Z_H[,j]*
		  ini_sex(n=inputs$daug_H,
		          prob_F=inputs$sexratio)
		dyn$SEX_N[,j]<-dyn$Z_N[,j]*
			ini_sex(n=inputs$daug_N,
			        prob_F=inputs$sexratio)				
				
		
			
					
				
		## [7] INITIALIZE WHETHER A FISH IS SEXUALLY MATURE (DURING INITIAL YEAR)	
		tmp_H<-ini_maturity(age=dyn$AGE_H[,j], 
		                    mat_cdf=inputs$propM)
		dyn$MAT_H[,j]<-dyn$Z_H[,j]*tmp_H$mature
		tmp_N<-ini_maturity(age=dyn$AGE_N[,j], 
		                    mat_cdf=inputs$propM)
		dyn$MAT_N[,j]<-dyn$Z_N[,j]*tmp_N$mature				
				
				
		## [8] INITIALIZE TIME SINCE SPAWNING	(FOR INTIAL YEAR PRIOR TO SPAWNING)
		dyn$MPS_H[,j]<-dyn$Z_H[,j]*ini_mps(n=inputs$daug_H,
		                                   mature=dyn$MAT_H[,j],
		                                   FirstSpawn=tmp_H$FirstSpawn)
		dyn$MPS_N[,j]<-dyn$Z_N[,j]*ini_mps(n=inputs$daug_N,
			                                 mature=dyn$MAT_N[,j],
			                                 FirstSpawn=tmp_N$FirstSpawn)
		

	  ## [9] INITIALIZE IF A FISH SPAWNED (DURING INITIAL YEAR) 
	  dyn$SPN_H[,j] <- dyn$Z_H[,j]*spawn(mps=dyn$MPS_H[,j],
	                                     mature=dyn$MAT_H[,j], 
	                                     FirstSpawn=tmp_H$FirstSpawn)
	  dyn$SPN_N[,j] <- dyn$Z_N[,j]*spawn(mps=dyn$MPS_N[,j],
	                                     mature=dyn$MAT_N[,j], 
	                                     FirstSpawn=tmp_N$FirstSpawn)
	  ## [??] INITIALIZE THE NUMBER OF NATURAL AGE-0 RECRUITS PRODUCED (DURING INTIAL YEAR)
	  # dyn$EGGS_H[,j]<-ini_eggs(fl=dyn$LEN_H[,j],
	  #                           a=inputs$fec_a,
	  #                           b=inputs$fec_b,
	  #                           er=inputs$fec_er,
	  #                           sex=dyn$SEX_H[,j],
	  #                           spawn=dyn$SPN_H[,j])	
	  # dyn$EGGS_N[,j]<-ini_eggs(fl=dyn$LEN_N[,j],
	  #                           a=inputs$fec_a,
	  #                           b=inputs$fec_b,
	  #                           er=inputs$fec_er,
	  #                           sex=dyn$SEX_N[,j],
	  #                           spawn=dyn$SPN_N[,j])	
	  
	  ## [??] INITIALIZE HATCHERY AND PARENTAL INFORMATION
	  if(inputs$hatchery_name)
	  {
	    dyn$HATCH[1:nrow(ini_H),j] <- ini_H$H[indxH]
	  }
	  if(inputs$genetics)
	  {
	    dyn$MOM_H[1:nrow(ini_H),j] <- ini_H$M[indxH]
	    dyn$DAD_H[1:nrow(ini_H),j] <- ini_H$D[indxH]
	    dyn$TAG_N[1:inputs$natural,j] <- paste0("TAG", 1:inputs$natural) 
	  }
	}
	
	## [] INITIALIZE AGE-0 POPULATION AND ADULT SPATIAL STRUCTURE
	### NATURAL ORIGIN AGE-0's: NON-SPATIAL
	if(inputs$spatial==FALSE)
	{
		dyn$AGE_0_N_BND<-matrix(inputs$natural_age0,nrow=1,ncol=inputs$nreps)
	}
	### SPATIAL STRUCTURE
	if(!inputs$spatial & inputs$migration)
	{
	  ## SET UP LOCATIONS
	  dyn$BEND_H<- matrix(0L,inputs$daug_H,inputs$nreps)  
	  dyn$BEND_N<- matrix(0L,inputs$daug_N,inputs$nreps)
	  ## INTIALIZE LOCATION: 1 = "WITHIN BASIN"; 0 = "OUTSIDE BASIN OR DEAD"
	  ### ALL KNOWN FISH INITIALLY WITHIN THE BASIN
	  dyn$BEND_H[,j]<-dyn$Z_H[,j]
	  dyn$BEND_N[,j]<-dyn$Z_N[,j]
	}
	if(inputs$spatial==TRUE)
	{
		## SET UP LOCATIONS
		dyn$BEND_H<- matrix(0L,inputs$daug_H,inputs$nreps)  
		dyn$BEND_N<- matrix(0L,inputs$daug_N,inputs$nreps)
		  #GIVES BEND LOCATION OF EACH ADULT
		dyn$AGE_0_N_BND<-matrix(0L,nrow=inputs$n_bends,ncol=inputs$nreps)
		  #GIVES THE NUMBER OF AGE-0's IN EACH BEND
		for(j in 1:inputs$nreps)
		{
	#### INITIALIZE LOCATION OF HATCHERY & NATURAL ORIGIN AGE-1 PLUS
			dyn$BEND_H[,j]<-dyn$Z_H[,j]*initialize_spatial_location(n=inputs$daug_H,
				nbends=inputs$n_bends,			
				relativeDensity=inputs$hatchery_age1plus_rel_dens)
			dyn$BEND_N[,j]<-dyn$Z_N[,j]*initialize_spatial_location(n=inputs$daug_N,
				nbends=inputs$n_bends,
				relativeDensity=inputs$natural_age1plus_rel_dens)					
	#### NATURAL ORIGIN AGE-0's: SPATIAL
			dyn$AGE_0_N_BND[,j]<-rmultinom(1,inputs$natural_age0,inputs$natural_age0_rel_dens)
		}
	}
	# END INITIALIZATION OF SPATIAL COMPONENTS
	
	## INITIALIZE YEARLINGS AVAILABLE FOR STOCKING
	if(inputs$genetics)
	{
	  dyn$BROOD_1<-inputs$broodstock$BROOD_1
	  dyn$BROOD_1$hatchery_survival<- 
	    plogis(rnorm(nrow(dyn$BROOD_1), 
	                 log(inputs$yearling$phi_mn/(1-inputs$yearling$phi_mn)),
	                 inputs$yearling$phi_sd))
	  tmp<- data.frame(yearlings=rbinom(nrow(dyn$BROOD_1)*inputs$nreps,
	                                   rep(dyn$BROOD_1$remaining,
	                                       each=inputs$nreps),
	                                   rep(dyn$BROOD_1$hatchery_survival,
	                                       each=inputs$nreps)),
	                   rep=rep(1:inputs$nreps, nrow(dyn$BROOD_1)))
	  dyn$BROOD_1<- 
	    as.data.frame(lapply(dyn$BROOD_1[,c("mother", "father", "hatchery")],
	                         function(x) rep(x,inputs$nreps)))
	  dyn$BROOD_1<- cbind(dyn$BROOD_1, tmp)
	}

	
	# VECTOR OF MONTHS
	## STARTS IN JANUARY
	dyn$m<- rep(c(1:12),inputs$nyears) 	
	return(dyn)
	}	

		
		
		
	
	