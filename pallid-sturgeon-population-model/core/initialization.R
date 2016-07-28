
### PROCESS INPUTS TO INITILIZE AND SIMULATE POPULATION	
modelInputs<- function(input){#reactive({
	tmp<-list()
	if(input$basin=="Lower"){indx<-1}else{indx<-2}
	tmp$basin_inp=ifelse(input$basin=="Lower",0,1)	
	
	
	## POPULATION CHARACTERISTICS
	tmp$basin= input$basin
	tmp$maxage=input$maxage[indx]
	tmp$sexratio=input$sexratio[indx]
	tmp$natural=input$natural[indx]
	tmp$hatchery=input$hatchery[indx]
	tmp$natural_age0=input$natural_age0[indx]
	tmp$hatchery_age0=input$hatchery_age0[indx]
	

	## LENGTH-WEIGHT
	tmp$a= exp(input$a_prime)[indx]
	tmp$a_prime= input$a_prime[indx]
	tmp$b= input$b[indx]
	tmp$lw_er=input$lw_er[indx]
	
	## FECUNDITY
	tmp$fec_a=input$fec_a[indx]
	tmp$fec_b=input$fec_b[indx]
	tmp$fec_er=input$fec_er[indx]
	
	# GROWTH
	tmp$ln_Linf_mu<-c(6.982160,7.136028770)[indx]
	tmp$ln_k_mu<- c(-2.382711,-3.003764445)[indx]
	tmp$vcv<- as.matrix(cbind(c(0.0894,-0.1327,-0.1327,0.3179),(c(0.2768,-0.364,-0.364,0.6342))))


	# SEXUAL MATURITY AND RETURN TO SPAWNING
	tmp$age_mat=input$age_mat[indx]
	tmp$mat_k=input$mat_k[indx]
	tmp$spn_a=input$spn_a[indx]
	tmp$spn_b=input$spn_b[indx]

	# SURVIVALS
	tmp$pr_embryo<-input$pr_embryo[indx] 
	tmp$phi_embryo<-input$phi_embryo[indx] 
	tmp$phi_free_embryo<-input$phi_free_embryo[indx] 
	tmp$phi0=input$phi_age0_mean[indx] 
	tmp$phi1=input$phi_age1_mean[indx]
	tmp$phi2=input$phi_age2_mean[indx]
	tmp$phi=c(tmp$phi1,rep(tmp$phi2,tmp$maxage-1))
	tmp$recruitment<- input$recruitment # TURN RECRUITMENT ON OR OFF
	
	
	# STOCKING
	tmp$fingerling=input$fingerling
	tmp$fingerling_month=input$fingerling_month
	tmp$fingerling_mn=input$fingerling_mn
	tmp$fingerling_sd=input$fingerling_sd 
	tmp$fingerling_stocking_rkm=input$fingerling_stocking_rkm

	
	### YEARLINGS
	tmp$yearling<-input$yearling
	tmp$yearling_month<-input$yearling_month
	tmp$yearling_mn<-input$yearling_mn
	tmp$yearling_sd<-input$yearling_sd
	tmp$yearling_age<-input$yearling_age
	tmp$yearling_stocking_rkm<-input$yearling_stocking_rkm
	
	
	# SIMULATION STUFF
	tmp$nreps=input$nreps
	tmp$nyears=input$nyears
	tmp$daug=input$daug
	tmp$startYear<-input$startYear
	


	tmp$commit<-input$commit
	tmp$output_name<- input$output_name	
	
	# SPATIAL
	## BEND DATA & META
	tmp$bend_meta<- subset(bend_meta,basin==tmp$basin)
	tmp$n_bends<- nrow(tmp$bend_meta)	
	tmp$bend_lengths<- diff(c(0,bend_meta[which(bend_meta$basin==input$basin),]$bend_start_rkm))
	## MONTHLY MOVEMENT MATRIX
	tmp$prob<- matrix(runif(tmp$n_bends*tmp$n_bends),nrow=tmp$n_bends,ncol=tmp$n_bends)
	tmp$prob[upper.tri(tmp$prob)]<-0
	tmp$prob<- tmp$prob/apply(tmp$prob,1,sum)
	tmp$spatial<- input$spatial
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
	tmp$size_indices<-input$size_indices
	## END SPATIAL 


	return(tmp)
	} #})

### INITIALIZE MODEL OBJECTS
initialize<- function(inputs)
	{
	# SET UP LIST FOR SIMULATION
	dyn<- list(
		k = as.data.table(matrix(0,inputs$daug,inputs$nreps)),
		Linf = as.data.table(matrix(0,inputs$daug,inputs$nreps)),
		LEN = as.data.table(matrix(0,inputs$daug,inputs$nreps)),
		WGT = as.data.table(matrix(0,inputs$daug,inputs$nreps)),
		Z = as.data.table(matrix(0L,inputs$daug,inputs$nreps)),
		AGE = as.data.table(matrix(0L,inputs$daug,inputs$nreps)),
		MAT = as.data.table(matrix(0L,inputs$daug,inputs$nreps)),
		MPS = as.data.table(matrix(0L,inputs$daug,inputs$nreps)),
		SEX = as.data.table(matrix(0L,inputs$daug,inputs$nreps)),
		SPN = as.data.table(matrix(0L,inputs$daug,inputs$nreps)),
		ORIGIN = as.data.table(matrix(0L,inputs$daug,inputs$nreps)),
		EGGS = as.data.table(matrix(0L,inputs$daug,inputs$nreps)))
	
	# INITIALIZATION
	
	
	## [1] ASSIGN LIVE OR DEAD
	for(j in 1:inputs$nreps)
		{
		set(dyn$Z,j=j,
			value=c(rep(1,inputs$natural+inputs$hatchery),
				rep(0,inputs$daug-(inputs$natural+inputs$hatchery))))
		## [2] ASSIGN ORIGIN HATCHERY=1 NATURAL = 0
		set(dyn$ORIGIN,j=j,
			value=c(rep(1,inputs$hatchery),
				rep(0,inputs$daug-inputs$hatchery)))
		## [3] INIITIALIZE GROWTH COEFFICIENTS; GROWTH IS NOT HERITABLE
		tmp<- ini_growth(x=1,n=inputs$daug,
			mu_ln_linf=inputs$ln_Linf_mu,
			mu_ln_k=inputs$ln_k_mu,
			vcv=inputs$vcv) 
		set(dyn$Linf,j=j,value=tmp$linf)
		set(dyn$k,j=j,value=tmp$k)
		## [4] INITIALIZE LENGTH
		set(dyn$LEN,
			j=j,
			value=dyn$Z[[j]]*ini_length(n=input$daug, 
				basin=inputs$basin,
				origin=dyn$ORIGIN[[j]], 
				spatial=FALSE,
				linf= dyn$Linf[[j]]))
		set(dyn$Linf,
			i=NULL,
			j=j,
			value=ifelse(dyn$LEN[[j]]<dyn$Linf[[j]],dyn$Linf[[j]], dyn$LEN[[j]]*1.1))
		## INITIALIZE WEIGHT GIVEN LENGTH
		### ASSUMES NO EFFECT OF ORIGIN
		set(dyn$WGT,
			j=j,
			value=dyn$Z[[j]]*ini_wgt(a=inputs$a,b=inputs$b,len=dyn$LEN[[j]],er=inputs$lw_er))
		## [4] INITIALIZE SEX
		set(dyn$SEX,
			j=j,
			value=dyn$Z[[j]]*ini_sex(n=input$daug,ratio=inputs$sexratio))
		## [4] INITIALIZE AGE IN MONTHS
		set(dyn$AGE,
			j=j,
			value=ini_age(len=dyn$LEN[[j]],
				linf=dyn$Linf[[j]],
				k=dyn$k[[j]],
				sizeAtHatch=7,
				maxAge=inputs$maxage))
		## [4] INITIALIZE WHETHER A FISH IS SEXUALLY MATURE	
		set(dyn$MAT,
			j=j,
			value=dyn$Z[[j]]*ini_maturity(k=inputs$mat_k,	
				len=dyn$LEN[[j]],
				age_mat=inputs$age_mat))
		## INITIALIZE TIME SINCE SPAWNING	
		set(dyn$MPS,
			j=j,
			value=dyn$Z[[j]]*ini_mps(n=inputs$daug,
				mature=dyn$MAT[[j]]))
		}	

	## INITIALIZE IF A FISH WILL SPAWN ONCE CONDITIIONS ARE MET
	# SPN_H  ####fixme####
	

	## INITIALIZE SPATIAL
	if(inputs$spatial==FALSE)
		{
		dyn$AGE_0_N_BND<-as.data.table(matrix(inputs$natural_age0,nrow=1,ncol=inputs$nreps))
		dyn$AGE_0_H_BND<-as.data.table(matrix(inputs$hatchery_age0,nrow=1,ncol=inputs$nreps))
		}
	if(inputs$spatial==TRUE)
		{
		dyn$RKM<- as.data.table(matrix(0,inputs$daug,inputs$nreps))
		dyn$AGE_0_N_BND<-as.data.table(matrix(inputs$natural_age0,nrow=inputs$n_bends,ncol=inputs$nreps))
		dyn$AGE_0_H_BND<-as.data.table(matrix(inputs$hatchery_age0,nrow=inputs$n_bends,ncol=inputs$nreps))
		for(j in 1:input$nreps)
			{
			# INITIALIZE LOCATION OF ADULTS
			set(dyn$RKM,
				j=j,
				value=dyn$Z[[j]]*ini_rkm(n=inputs$daug,
					type=inputs$adult_spatial_structure,
					bend_lengths=inputs$bend_lengths))
			# INITIALIZE AGE-0 IN EACH BEND
			set(dyn$AGE_0_N_BND,
				j=j,
				value= rmultinom(1,inputs$natural_age0,inputs$natural_age0_rel_dens))
			set(dyn$AGE_0_H_BND,
				j=j,
				value=rmultinom(1,inputs$hatchery_age0,inputs$hatchery_age0_rel_dens))
			}
		}
	# END INITIALIZATION ##########	
	
	# VECTOR OF MONTHS
	## STARTS IN JANUARY
	dyn$m<- rep(c(1:12),inputs$nyears) 	
	return(dyn)
	}	
		
		
		
		
	
	