### VERSION 2

# to do
## 1. set up values in the initialization of indData
## 2. convert movement to rkm rather than bend to bend
## 3. assign bend to rkm



	
#sim_pop<- function(inputs=input)
#	{
inputs=input

	# NUMBER OF ADULTS IN EACH BEND GIVEN DENSITY AND BEND LENGTH
	N_n<- rmultinom(inputs$nreps,
		inputs$natural,inputs$rel_density)
	N_h<- rmultinom(inputs$nreps,
		inputs$hatchery,inputs$rel_density)	
		
	# SET UP MATRICES FOR SIMULATION	
	Z_H<-AGE_H<-LEN_H<-MAT_H<-RKM_H<-MPS_H<-WGT_H<-SEX_H<- matrix(0,inputs$daug,inputs$nreps)
	Z_N<-AGE_N<-LEN_N<-MAT_N<-RKM_N<-MPS_N<-WGT_N<-SEX_N<- matrix(0,inputs$daug,inputs$nreps)

	# FISH ALIVE AT INITIALIZATION 
	## [Z]
	Z_H[]<- sapply(1:inputs$nreps, ini_Z,
		n=inputs$hatchery,
		fill0=inputs$daug-inputs$hatchery)
	Z_N[]<- sapply(1:inputs$nreps, ini_Z,
		n=inputs$natural,
		fill0=inputs$daug-inputs$natural)
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
	MPS_H[]<-sapply(1:inputs$nreps, ini_mps,
		n=inputs$daug,
		mature=MAT_H,
		live=Z_H)
	MPS_N[]<-sapply(1:inputs$nreps, ini_mps,
		n=inputs$daug,
		mature=MAT_N,
		live=Z_N)
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
	RKM_H[]<-sapply(1:inputs$nreps,ini_rkm,
		n=inputs$hatchery,
		fill0=inputs$daug-inputs$hatchery)
	RKM_N[]<-sapply(1:inputs$nreps,ini_rkm,
		n=inputs$natural,
		fill0=inputs$daug-inputs$natural)		
	# END INITIALIZATION ##########
	
	
	# VECTOR OF MONTHS
	## STARTS IN JANUARY
	m<- rep(c(1:12),input$nyears) 	
	# SIMULATE POPULATION DYNAMICS GIVEN INITIAL STATES
	
	for(i in m) # M IS A VECTOR OF MONTHS 1:12, REPEATED FOR NYEARS
		{
		# UPDATE WHETHER FISH SURVIVED PRIOR TO UPDATING OTHER STUFF
		Z_N<- sapply(1:inputs$nreps,dSurvival,
			n=input$daug,
			phi_age=inputs$phi,# vector of age survivals
			age=AGE_N,
			live=Z_N)
		# UPDATE WHETHER FISH SURVIVED PRIOR TO UPDATING OTHER STUFF
		Z_H<- sapply(1:inputs$nreps,dSurvival,
			n=input$daug,
			phi_age=inputs$phi,# vector of age survivals
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
			## BY FEMALES
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
					factor(bend(RKM_N[,x]),	levels=c(1:300)),
					sum)
				N[is.na(N)]<-0
				H<-tapply(EGGS_H[,x],
					factor(bend(RKM_H[,x]),	levels=c(1:300)),
					sum)
				H[is.na(H)]<-0				
				return(N+H)})
			## FERTILIZATION
			### HOW MANY FEMALES IN EACH BEND
			FEM_BND<- sapply(1:inputs$nreps,function(x){
				N<-tapply(Z_H[,x]*SEX_H[,x],
					factor(bend(RKM_N[,x]),	levels=c(1:300)),
					sum)
				N[is.na(N)]<-0
				H<-tapply(Z_N[,x]*SEX_N[,x],
					factor(bend(RKM_H[,x]),	levels=c(1:300)),
					sum)
				H[is.na(H)]<-0				
				return(N+H)})	
			### HOW MANY MALES IN EACH BEND
			MAL_BND<- sapply(1:inputs$nreps,function(x){
				N<-tapply(Z_H[,x]*(1-SEX_H[,x])*MAT_H[,x],
					factor(bend(RKM_N[,x]),	levels=c(1:300)),
					sum)
				N[is.na(N)]<-0
				H<-tapply(Z_N[,x]*(1-SEX_N[,x])*MAT_N[,x],
					factor(bend(RKM_H[,x]),	levels=c(1:300)),
					sum)
				H[is.na(H)]<-0				
				return(N+H)})			
			## SET MONTHS POST SPAWN TO -1 
			## FOR FISH THAT SPAWNED
			MPS_N[SPN_N==1]<- -1
			MPS_H[SPN_H==1]<- -1
			
			## EMBRYOS 
			### FERTILIZATON OF EGGS TO EMBRYOS
			AGE_0_BND<- sapply(1:inputs$nreps,function(x)
				{
				a<- -3
				b<- 0.5
				tmp<-rbinom(nrow(AGE_0_BND),AGE_0_BND[,x],plogis(a+b*MAL_BND[,x]))
				return(tmp)
				})
			## EMBRYOS TO FREE EMBRYOS
			AGE_0_BND<- sapply(1:inputs$nreps,function(x)
				{
				tmp<-rbinom(nrow(AGE_0_BND),AGE_0_BND[,x],inputs$phi_1)
				return(tmp)
				})			
			## DOWNSTREAM DRIFT
			
			
			
			## FREE EMBRYOS TO EXO. FEEDING LARVAE

			
			
			
			} # END JUNE SPAWNING
		
		# UPDATE MONTHS SINCE SPAWNING
		MPS_N<- sapply(1:inputs$nreps,dMPS,
			mps=MPS_N,
			mature=MAT_N,
			live=Z_N)
		MPS_N<- sapply(1:inputs$nreps,dMPS,
			mps=MPS_N,
			mature=MAT_N,
			live=Z_N)		

			
			
		
		}
	}


	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	