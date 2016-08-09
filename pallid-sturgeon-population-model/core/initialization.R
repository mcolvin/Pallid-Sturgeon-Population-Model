
### PROCESS INPUTS TO INITILIZE AND SIMULATE POPULATION	
modelInputs<- function(input,spatial,recruitmentFreq=0,sizeStructure=TRUE){
	tmp<-list()
	tmp$basin_inp=ifelse(input$basin=="Lower",0,1)	
	
	
	## POPULATION CHARACTERISTICS
	tmp$basin= input$basin
	tmp$maxage=input[[input$basin]]$maxage
	tmp$sexratio=input[[input$basin]]$sexratio
	tmp$natural=input[[input$basin]]$natural
	tmp$hatchery=input[[input$basin]]$hatchery
	tmp$natural_age0=input[[input$basin]]$natural_age0
	tmp$hatchery_age0=input[[input$basin]]$hatchery_age0
	

	## LENGTH-WEIGHT
	tmp$a			= exp(input[[input$basin]]$a_prime)
	tmp$a_prime		= input[[input$basin]]$a_prime
	tmp$b			= input[[input$basin]]$b
	tmp$lw_er		=input[[input$basin]]$lw_er
	
	## FECUNDITY
	tmp$fec_a=input[[input$basin]]$fec_a
	tmp$fec_b=input[[input$basin]]$fec_b
	tmp$fec_er=input[[input$basin]]$fec_er
	
	# GROWTH
	tmp$maxLinf<-input[[input$basin]]$maxLinf
	tmp$ln_Linf_mu<-input[[input$basin]]$ln_Linf_mu
	tmp$ln_k_mu<- input[[input$basin]]$ln_k_mu
	tmp$vcv<- input[[input$basin]]$vcv


	# SEXUAL MATURITY AND RETURN TO SPAWNING
	tmp$age_mat=input[[input$basin]]$age_mat
	tmp$mat_k=input[[input$basin]]$mat_k
	tmp$spn_a=input[[input$basin]]$spn_a
	tmp$spn_b=input[[input$basin]]$spn_b

	# SURVIVALS
	tmp$pr_embryo<-input[[input$basin]] $pr_embryo 
	tmp$phi_embryo<-input[[input$basin]]$phi_embryo
	tmp$phi_free_embryo<-input[[input$basin]]$phi_free_embryo
	tmp$phi0=input[[input$basin]]$phi_age0_mean
	tmp$phi1=input[[input$basin]]$phi_age1_mean
	tmp$phi2=input[[input$basin]]$phi_age2_mean
	tmp$phi=c(tmp$phi1,rep(tmp$phi2,tmp$maxage-1))
	
	
	# STOCKING
	tmp$fingerling=input$stockingInput$fingerling
	tmp$fingerling_month=input$stockingInput$fingerling_month
	tmp$fingerling_mn=input$stockingInput$fingerling_mn
	tmp$fingerling_sd=input$stockingInput$fingerling_sd 
	tmp$fingerling_stocking_rkm=input$stockingInput$fingerling_stocking_rkm

	
	### YEARLINGS
	tmp$yearling<-input$stockingInput$yearling
	tmp$yearling_month<-input$stockingInput$yearling_month
	tmp$yearling_mn<-input$stockingInput$yearling_mn
	tmp$yearling_sd<-input$stockingInput$yearling_sd
	tmp$yearling_age<-input$stockingInput$yearling_age
	tmp$yearling_stocking_rkm<-input$stockingInput$yearling_stocking_rkm
	
	
	# SIMULATION STUFF
	tmp$nreps=input$simulationInput$nreps
	tmp$nyears=input$simulationInput$nyears
	tmp$daug_H=input$simulationInput$daug_H
	tmp$daug_N=input$simulationInput$daug_N
	tmp$startYear<-input$simulationInput$startYear
	tmp$spatial=spatial
	tmp$recruitmentFreq = recruitmentFreq
	tmp$sizeStructure = sizeStructure

	tmp$commit<-input$commit
	tmp$output_name<- input$output_name	
	tmp$version<- input$version	
	
	# SPATIAL
	## SPATIAL STRUCTURE ADULTS
	if(spatial==TRUE)
		{	
		## BEND DATA & META
		tmp$bend_meta<- bend_meta[[input$basin]]
		tmp$n_bends<- nrow(bend_meta[[input$basin]])	
		tmp$bend_lengths<- diff(c(0,bend_meta[[input$basin]]$bend_start_rkm))
		
		## MONTHLY MOVEMENT MATRIX
		tmp$prob<- matrix(runif(tmp$n_bends*tmp$n_bends),nrow=tmp$n_bends,ncol=tmp$n_bends)
		tmp$prob[upper.tri(tmp$prob)]<-0
		tmp$prob<- tmp$prob/apply(tmp$prob,1,sum)
		tmp$spatial<- input$spatial

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
		tmp$size_indices<-input$size_indices
		## END SPATIAL 
		}
	return(tmp)
	}

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
		#ORIGIN = matrix(0L,inputs$daug,inputs$nreps),
		EGGS_H = matrix(0L,inputs$daug_H,inputs$nreps),
		EGGS_N = matrix(0L,inputs$daug_N,inputs$nreps))
	
	# INITIALIZATION
	
	dyn$Z_H[1:inputs$hatchery,]<-1
	dyn$Z_N[1:inputs$natural,]<-1
	
	## [1] ASSIGN LIVE OR DEAD
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
	

	## INITIALIZE SPATIAL
	if(inputs$spatial==FALSE)
		{
		dyn$AGE_0_N_BND<-matrix(inputs$natural_age0,nrow=1,ncol=inputs$nreps)
		dyn$AGE_0_H_BND<-matrix(inputs$hatchery_age0,nrow=1,ncol=inputs$nreps)
		}
	if(inputs$spatial==TRUE)
		{
		dyn$RKM_H<- matrix(0,inputs$daug_H,inputs$nreps)
		dyn$RKM_N<- matrix(0,inputs$daug_N,inputs$nreps)
		dyn$AGE_0_N_BND<-matrix(inputs$natural_age0,nrow=inputs$n_bends,ncol=inputs$nreps)
		dyn$AGE_0_H_BND<-matrix(inputs$hatchery_age0,nrow=inputs$n_bends,ncol=inputs$nreps)
		for(j in 1:input$nreps)
			{
			# INITIALIZE LOCATION OF ADULTS
			dyn$RKM_H[,j]<-dyn$Z[,j]*ini_rkm(n=inputs$daug_H,
					type=inputs$adult_spatial_structure,
					bend_lengths=inputs$bend_lengths)
			dyn$RKM_N[,j]<-dyn$Z[,j]*ini_rkm(n=inputs$daug_N,
					type=inputs$adult_spatial_structure,
					bend_lengths=inputs$bend_lengths)					
					
			# INITIALIZE AGE-0 IN EACH BEND
			dyn$AGE_0_N_BND[,j]<-rmultinom(1,inputs$natural_age0,inputs$natural_age0_rel_dens)
			dyn$AGE_0_H_BND[,j]<-rmultinom(1,inputs$hatchery_age0,inputs$hatchery_age0_rel_dens)
			}
		}
	# END INITIALIZATION ##########	

	#if(sizeStructure==TRUE)
	#	{
#
#		
#		}

	
	# VECTOR OF MONTHS
	## STARTS IN JANUARY
	dyn$m<- rep(c(1:12),inputs$nyears) 	
	return(dyn)
	}	
		
		
		
		
	
	