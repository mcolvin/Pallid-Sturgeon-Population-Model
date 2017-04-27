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

	# INITIALIZATION
	## [1] ASSIGN LIVE OR DEAD	
	dyn$Z_H[1:inputs$hatchery,]<-1
	dyn$Z_N[1:inputs$natural,]<-1

	for(j in 1:inputs$nreps)
		{
		## [3] INIITIALIZE GROWTH COEFFICIENTS
		##     GROWTH IS NOT HERITABLE
		tmp<- ini_growth(n=inputs$daug_H,
			mu_ln_linf=inputs$ln_Linf_mu,
			mu_ln_k=inputs$ln_k_mu,
			vcv=inputs$vcv,
			maxLinf=inputs$maxLinf) 
		dyn$Linf_H[,j]<-tmp$linf
		dyn$k_H[,j]<-tmp$k
		
		tmp<- ini_growth(n=inputs$daug_N,
			mu_ln_linf=inputs$ln_Linf_mu,
			mu_ln_k=inputs$ln_k_mu,
			vcv=inputs$vcv,
			maxLinf=inputs$maxLinf) 
		dyn$Linf_N[,j]<-tmp$linf
		dyn$k_N[,j]<-tmp$k	
		
		
		## [4] INITIALIZE LENGTH
		dyn$LEN_H[,j]<-dyn$Z_H[,j]*ini_length(n=inputs$daug_H, 
				basin=inputs$basin,
				origin=1, # 1 FOR HATCHERY, 0 FOR NATURAL
				spatial=FALSE,
				linf= dyn$Linf_H[,j])
		dyn$Linf_H[,j]<-ifelse(dyn$LEN_H[,j]<dyn$Linf_H[,j],
			dyn$Linf_H[,j], 
			dyn$LEN_H[,j]*1.1)
		
		dyn$LEN_N[,j]<-dyn$Z_N[,j]*ini_length(n=inputs$daug_N, 
				basin=inputs$basin,
				origin=0, # 1 FOR HATCHERY, 0 FOR NATURAL
				spatial=FALSE,
				linf= dyn$Linf_N[,j])
		dyn$Linf_N[,j]<-ifelse(dyn$LEN_N[,j]<dyn$Linf_N[,j],
			dyn$Linf_N[,j], 
			dyn$LEN_N[,j]*1.1)		
		
		
		## INITIALIZE WEIGHT GIVEN LENGTH
		### ASSUMES NO EFFECT OF ORIGIN
		dyn$WGT_H[,j]<-dyn$Z_H[,j]*
			ini_wgt(a=inputs$a,
				b=inputs$b,
				len=dyn$LEN_H[,j],
				er=inputs$lw_er)
		dyn$WGT_N[,j]<-dyn$Z_N[,j]*
			ini_wgt(a=inputs$a,
				b=inputs$b,
				len=dyn$LEN_N[,j],
				er=inputs$lw_er)				
	
				
				
		## [4] INITIALIZE SEX
		dyn$SEX_H[,j]<-dyn$Z_H[,j]*
			ini_sex(n=inputs$daug_H,
				ratio=inputs$sexratio)
		dyn$SEX_N[,j]<-dyn$Z_N[,j]*
			ini_sex(n=inputs$daug_N,
				ratio=inputs$sexratio)				
				
		## [4] INITIALIZE AGE IN MONTHS
		dyn$AGE_H[,j]<-ini_age(len=dyn$LEN_H[,j],
				linf=dyn$Linf_H[,j],
				k=dyn$k_H[,j],
				sizeAtHatch=7,
				maxAge=inputs$maxage)
		dyn$AGE_N[,j]<-ini_age(len=dyn$LEN_N[,j],
				linf=dyn$Linf_N[,j],
				k=dyn$k_N[,j],
				sizeAtHatch=7,
				maxAge=inputs$maxage)				
					
				
		## [4] INITIALIZE WHETHER A FISH IS SEXUALLY MATURE	
		dyn$MAT_H[,j]<-dyn$Z_H[,j]*ini_maturity(k=inputs$mat_k,	
				len=dyn$LEN_H[,j],
				age_mat=inputs$age_mat)
		dyn$MAT_N[,j]<-dyn$Z_N[,j]*ini_maturity(k=inputs$mat_k,	
				len=dyn$LEN_N[,j],
				age_mat=inputs$age_mat)				
				
				
		## INITIALIZE TIME SINCE SPAWNING	
		dyn$MPS_H[,j]<-dyn$Z_H[,j]*ini_mps(n=inputs$daug_H,
			mature=dyn$MAT_H[,j])
		dyn$MPS_N[,j]<-dyn$Z_N[,j]*ini_mps(n=inputs$daug_N,
			mature=dyn$MAT_N[,j])
		}	

	## INITIALIZE IF A FISH WILL SPAWN ONCE CONDITIIONS ARE MET
	# SPN_H  ####fixme####
	

	## INITIALIZE SPATIAL COMPONENTS
	if(inputs$spatial==FALSE)
		{
		dyn$AGE_0_N_BND<-matrix(inputs$n_bends,nrow=1,ncol=inputs$nreps)
		dyn$AGE_0_H_BND<-matrix(inputs$n_bends,nrow=1,ncol=inputs$nreps)
		}
	if(inputs$spatial==TRUE)
		{
		## SET UP LOCATIONS
		dyn$BEND_H<- matrix(0L,inputs$daug_H,inputs$nreps)
		dyn$BEND_N<- matrix(0L,inputs$daug_N,inputs$nreps)
		dyn$AGE_0_N_BND<-matrix(inputs$natural_age0,nrow=inputs$n_bends,ncol=inputs$nreps)
		dyn$AGE_0_H_BND<-matrix(inputs$hatchery_age0,nrow=inputs$n_bends,ncol=inputs$nreps)
		for(j in 1:inputs$nreps)
			{
			# INITIALIZE LOCATION OF ADULTS
			dyn$BEND_H[,j]<-dyn$Z_H[,j]*initialize_spatial_location(n=inputs$daug_H,
				nbends=inputs$n_bends,			
				relativeDensity=inputs$hatchery_age1plus_rel_dens)
			dyn$BEND_N[,j]<-dyn$Z_N[,j]*initialize_spatial_location(n=inputs$daug_N,
				nbends=inputs$n_bends,
				relativeDensity=inputs$natural_age1plus_rel_dens)					
					
			# INITIALIZE AGE-0 IN EACH BEND
			dyn$AGE_0_N_BND[,j]<-rmultinom(1,inputs$natural_age0,inputs$natural_age0_rel_dens)
			dyn$AGE_0_H_BND[,j]<-rmultinom(1,inputs$hatchery_age0,inputs$hatchery_age0_rel_dens)
			}
		}
	# END INITIALIZATION OF SPATIAL COMPONENTS

	
	# VECTOR OF MONTHS
	## STARTS IN JANUARY
	dyn$m<- rep(c(1:12),inputs$nyears) 	
	return(dyn)
	}	
		
		
		
		
	
	