
### PROCESS INPUTS TO INITILIZE AND SIMULATE POPULATION	
modelInputs<- function(input){#reactive({
	tmp<-list()
	if(input$basin=="Lower"){indx<-1}else{indx<-2}
	
	# POPULATION CHARACTERISTICS
	tmp$basin= input$basin
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
	tmp$recruitment<- input$recruitment # TURN RECRUITMENT ON OR OFF
	
	# MOVEMENT [NOT ON UI YET]
	tmp$spread=10 #input$spread<- 10
	
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
	
	return(tmp)
	} #})

	
### CORE FUNCTION TO DO SIMULATIONS	
sim<- function(inputs)
	{
	
	# SET UP MATRICES FOR SIMULATION	
	## HATCHERY ORIGIN FISH
	k_H<-Linf_H<-LEN_H<-WGT_H<- matrix(0,inputs$daug,inputs$nreps)# float
	Z_H<-AGE_H<-MAT_H<-MPS_H<-SEX_H<-SPN_H<- matrix(0L,inputs$daug,inputs$nreps)# integer
	## NATURAL ORIGIN FISH
	k_N<-Linf_N<-LEN_N<-WGT_N<- matrix(0,inputs$daug,inputs$nreps)# float
	Z_N<-AGE_N<-MAT_N<-MPS_N<-SEX_N<-SPN_H<- matrix(0L,inputs$daug,inputs$nreps)# integer


	indx_h<- cbind(rep(1:inputs$hatchery,inputs$nreps),sort(rep(1:inputs$nreps,inputs$hatchery)))
	indx_n<- cbind(rep(1:inputs$natural,inputs$nreps),sort(rep(1:inputs$nreps,inputs$natural)))
	
	# FISH ALIVE AT INITIALIZATION [Z]
	Z_H[indx_h]<- 1
	Z_N[indx_n]<- 1

	
	# INIITIALIZE GROWTH COEFFICIENTS
	## ASSUMES THAT GROWTH IS NOT HERITABLE
	tmp<- ini_growth(x=inputs$nreps,
			n=nrow(Linf_H),
			basin="lower") 	
	Linf_H[]<- tmp$linf
	k_H[]<- tmp$k
	tmp<- ini_growth(x=inputs$nreps,
			n=nrow(Linf_N),
			basin="lower") 	
	Linf_N[]<- tmp$linf
	k_N[]<- tmp$k			

	
	# INITIALIZE LENGTH
	LEN_H[indx_h]<-ini_length(linf= Linf_H[indx_h],	basin=inputs$basin)
	LEN_N[indx_n]<-ini_length(linf= Linf_N[indx_n],	basin=inputs$basin)	

	
	# INITIALIZE WEIGHT GIVEN LENGTH
	WGT_H[indx_h]<-ini_wgt(a=inputs$a,b=inputs$b,len=LEN_H[indx_h],er=inputs$lw_er)
	WGT_N[indx_n]<-ini_wgt(a=inputs$a,b=inputs$b,len=LEN_N[indx_n],er=inputs$lw_er)


	# INITIALIZE SEX OF FISH
	SEX_H[indx_h]<-ini_sex(n=nrow(indx_h),ratio=inputs$sexratio)
	SEX_N[indx_n]<-ini_sex(n=nrow(indx_n),ratio=inputs$sexratio)

	
	# INITIALIZE AGE IN MONTHS
	AGE_N[indx_n]<- ini_age(len=LEN_N[indx_n],linf=Linf_N[indx_n],k=k_N[indx_n],sizeAtHatch=7,maxAge=inputs$maxage)
	AGE_H[indx_h]<- ini_age(len=LEN_H[indx_h],linf=Linf_H[indx_h],k=k_H[indx_h],sizeAtHatch=7,maxAge=inputs$maxage)
	

	# INITIALIZE WHETHER A FISH IS SEXUALLY MATURE									
	MAT_H[indx_h]<-ini_maturity(k=inputs$mat_k,	len=LEN_H[indx_h],age_mat=inputs$age_mat)
	MAT_N[indx_n]<-ini_maturity(k=inputs$mat_k,	len=LEN_N[indx_n],age_mat=inputs$age_mat)
		

	# INITIALIZE TIME SINCE SPAWNING
	MPS_H[indx_h]<-ini_mps(n=inputs$hatchery,mature=MAT_H[indx_h])
	MPS_N[indx_n]<-ini_mps(n=inputs$natural,mature=MAT_N[indx_n])		
	
	# INITIALIZE IF A FISH WILL SPAWN ONCE CONDITIIONS ARE MET
	#SPN_H
		
	if(inputs$spatial==FALSE)
		{
		AGE_0_N_BND<-matrix(inputs$natural_age0,nrow=1,ncol=inputs$nreps)
		AGE_0_H_BND<-matrix(inputs$hatchery_age0,nrow=1,ncol=inputs$nreps)
		}
		
	if(inputs$spatial==TRUE)
		{
		RKM_H<- matrix(0,inputs$daug,inputs$nreps)# float
		RKM_N<- matrix(0,inputs$daug,inputs$nreps)# float
		
		# INITIALIZE LOCATION OF ADULTS
		RKM_H[indx_h]<-ini_rkm(n=inputs$hatchery,type=inputs$adult_spatial_structure,bend_lengths=inputs$bend_lengths)
		RKM_N[indx_n]<-ini_rkm(n=inputs$natural,type=inputs$adult_spatial_structure,bend_lengths=inputs$bend_lengths)
		
		# INITIALIZE AGE-0 IN EACH BEND
		AGE_0_N_BND<-rmultinom(inputs$nreps,inputs$natural_age0,inputs$natural_age0_rel_dens)
		AGE_0_H_BND<-rmultinom(inputs$nreps,inputs$hatchery_age0,inputs$hatchery_age0_rel_dens)
		}
	# END INITIALIZATION ##########	
		
		
		
		
		
		
	# VECTOR OF MONTHS
	## STARTS IN JANUARY
	m<- rep(c(1:12),inputs$nyears) 	
	
	## SUMMARIES
	### ABUNDANCE (AGE-1+)
	N_N_SUM<-colSums(Z_N)
	N_H_SUM<-colSums(Z_H)
	### AGE-1 RECRUITS; NATURAL ORIGIN
	indx<- lapply(1:inputs$nreps,function(x){out<- which(AGE_N[,x]>0 & AGE_N[,x]<24)}) 		
	recruits<- matrix(sapply(1:inputs$nreps,function(x) length(indx[[x]])),nrow=1)
	sq<-qp<-pm<-mt<-tr<-data.frame()
	
	# PROGRESS BAR
	#withProgress(message = 'Executing model ',value = 0, {	
	#k=0	
	pb<-txtProgressBar(min=1,max=length(m),initial=0,char="*",style=3)

	# SIMULATE POPULATION DYNAMICS GIVEN INITIAL STATES
	for(i in 1:length(m)) # M IS A VECTOR OF MONTHS 1:12, REPEATED FOR NYEARS
		{
		#k=k+1
		#incProgress(1/length(m))
		setTxtProgressBar(pb, i)

		indx_h<- lapply(1:inputs$nreps,function(x){which(Z_H[,x]==1)}) 
		tmp<- unlist(lapply(1:inputs$nreps,function(x) length(indx_h[[x]])))
		indx_h<- cbind(unlist(indx_h),sort(rep(1:inputs$nreps,tmp)))
		
		indx_n<- lapply(1:inputs$nreps,function(x){which(Z_N[,x]==1)}) 
		tmp<- unlist(lapply(1:inputs$nreps,function(x) length(indx_n[[x]])))
		indx_n<- cbind(unlist(indx_n),sort(rep(1:inputs$nreps,tmp)))			
	
		### UPDATE WHETHER FISH SURVIVED PRIOR TO UPDATING OTHER STUFF  
		Z_N[indx_n]<- dSurvival(phi_age=inputs$phi,	age=AGE_N[indx_n]) 
		Z_H[indx_h]<- dSurvival(phi_age=inputs$phi,	age=AGE_H[indx_h]) 
		
		### UPDATE TOTAL LENGTH 
		LEN_H[indx_h]<-dLength(k=k_H[indx_h],linf=Linf_H[indx_h],dT=1/12,length1=LEN_H[indx_h])
		LEN_N[indx_n]<-dLength(k=k_N[indx_n],linf=Linf_N[indx_n],dT=1/12,length1=LEN_N[indx_n])
	
		### UPDATE WEIGHT 
		WGT_H[indx_h]<-dWeight(len=LEN_H[indx_h],a=inputs$a,b=inputs$b,er=inputs$lw_er)
		WGT_N[indx_n]<-dWeight(len=LEN_H[indx_n],a=inputs$a,b=inputs$b,er=inputs$lw_er)



		### SPAWNING AND RECRUITMENT 
		if(m[i]==6)
			{
			# EXPAND AGE-0 COHORTS TO INDIVIDUALS			
			## INDEX OF OPEN SLOTS TO PUT AGE-0 FISH TRANSITIONING
			## TO AGE-1.... HAPPY BIRTHDAY LITTLE FIDDIES
			if(sum(colSums(AGE_0_N_BND))>0)
				{
				indx_h<- unlist(sapply(1:inputs$nreps,function(x) which(Z_H[,x]==0)[1:sum(AGE_0_H_BND[,x])]))		
				indx_h<- cbind(c(indx_h),sort(rep(1:inputs$nreps,colSums(AGE_0_H_BND))))
				Z_H[indx_h]<-1    # ADD NEW 1 YEAR OLD RECRUITS
				AGE_H[indx_h]<-12 # UPDATE AGE OF RECRUITS		
				LEN_H[indx_h]<-rnorm(length(indx_h[,1]),inputs$recruit_mean_length,inputs$recruit_length_sd)				
				WGT_H[indx_h]<-rlnorm(length(indx_h[,1]),log(inputs$a*LEN_H[indx_h]^inputs$b),inputs$lw_er)					
				if(inputs$spatial==TRUE){
					RKM_H[indx_h]<- bend2rkm(c(unlist(sapply(1:inputs$nreps,# ASSIGN LOCATION OF RECRUITS
						function(x){rep(1:inputs$n_bends,AGE_0_H_BND[,x])}))))
					}		
				MAT_H[indx_h]<-0# ASSIGN MATURATION STATUS OF NEW RECRUITS
				SEX_H[indx_h]<-rbinom(length(indx_h[,1]),1,0.5)# ASSIGN SEX TO RECRUITS
				}
					
			if(sum(colSums(AGE_0_N_BND))>0)
				{
				indx_n<- unlist(sapply(1:inputs$nreps,function(x) which(Z_N[,x]==0)[1:sum(AGE_0_N_BND[,x])]))		
				indx_n<- cbind(c(indx_n),sort(rep(1:inputs$nreps,colSums(AGE_0_N_BND))))
				Z_N[indx_n]<-1## ADD NEW 1 YEAR OLD RECRUITS						
				AGE_N[indx_n]<-1# UPDATE AGE OF RECRUITS
				LEN_N[indx_n]<-rnorm(length(indx_n[,1]),inputs$recruit_mean_length,inputs$recruit_length_sd)		
				WGT_N[indx_n]<-rlnorm(length(indx_n[,1]),log(inputs$a*LEN_N[indx_n]^inputs$b),inputs$lw_er)		
		
				MAT_N[indx_n]<-0# ASSIGN MATURATION STATUS OF NEW RECRUITS			
				SEX_N[indx_n]<-rbinom(length(indx_n[,1]),1,0.5)# ASSIGN SEX TO RECRUITS
				if(input$spatial==TRUE)
					{
					RKM_N[indx_n]<- bend2rkm(c(unlist(sapply(1:inputs$nreps,# ASSIGN LOCATION OF RECRUITS
						function(x)	{rep(1:inputs$n_bends,AGE_0_N_BND[,x])}))))}				
					} # END IF RECRUITS > 0
			# END RECRUITMENT
			AGE_0_N_BND[]<-0 # ZERO OUT AGE 0 AFTER THEY MOVE TO AGE-1
			AGE_0_H_BND[]<-0 # ZERO OUT AGE 0 AFTER THEY MOVE TO AGE-1	
			}

			if(inputs$recruit==TRUE) ####fixme####
				{
				## ASSIGN WHETHER A FISH WILL SPAWN
				## GIVEN TIME SINCE LAST SPAWN
				SPN_H[indx_h]<-spawn(
					mps=MPS_H[indx_h],
					a=inputs$spn_a,
					b=inputs$spn_b,
					mature=MAT_H[indx_h])
				SPN_N[indx_n]<-spawn(
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

				AGE_0_N_BND<-sapply(1:inputs$nreps,dFreeEmbryoDrift,
					nbends=nrow(AGE_0_N_BND),
					loc=AGE_0_N_BND,
					prob=inputs$prob)				

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
				} # END RECRUITMENT
		
		
		if(input$spatial==TRUE)
			{
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
			## UPDATE ANY AGE-0 MOVEMENT #########################################################################
			#AGE_0_N<-	
			#AGE_0_H<-					
			} # END SPATIAL


		
		
		# PALLID STURGEON STOCKING ###############################################################################
		## STOCKING OCCURS AT THE END OF THE MONTH
		## NOT SUBJECT TO MORTALITY OR MOVEMENT
		## FINGERLING STOCKING (AGE-0)
		if(inputs$fingerling>0 & m[i]==inputs$fingerling_month)
			{
			# ADD NUMBER OF FISH STOCKED IN A BEND
			AGE_0_H_BND[inputs$stocking_bend,]<- AGE_0_H_BND[inputs$stocking_bend,]+inputs$fingerling
			}

		## YEARLING STOCKING (AGE-1+); INDIVIDUALS
		if(inputs$yearling>0 & inputs$yearling_month==m[i])
			{
			### GET INDEXES OF OPEN SLOTS TO STICK STOCKED INDIVIDUALS
			indx<- lapply(1:inputs$nreps,function(x){out<- which(Z_H[,x]==0)[1:inputs$yearling]}) 
			indx<- cbind(unlist(indx),sort(rep(1:inputs$nreps,inputs$yearling)))
			Z_H[indx]<- 1### ADD NEWLY STOCKED INDIVIDUALS TO Z_H
			LEN_H[indx]<- rnorm(inputs$yearling*inputs$nreps,inputs$yearling_mn,inputs$yearling_sd)# UPDATE LENGTH
			WGT_H[indx]<- rlnorm(length(indx[,1]),log(inputs$a*LEN_H[indx]^inputs$b),inputs$lw_er)	# UPDATE WEIGHT
			AGE_H[indx]<- inputs$yearling_age # ASSIGN AGE
			MAT_H[indx]<- 0	# ASSIGN MATURITY
			RKM_H[indx]<- inputs$yearling_stocking_rkm# ASSIGN LOCATION OF STOCKED INDIVIDUALS
			MAT_H[indx]<- 0# ASSIGN MATURATION STATUS OF NEW RECRUITS
			SEX_H[indx]<-rbinom(length(indx[,1]),1,0.5)# ASSIGN SEX TO RECRUITS			
			}
		# END STOCKING #################################################################	

		# PSD
		if(input$size_indices==TRUE)
			{
			PSD<- sapply(1:input$nreps,function(x){
				out<-c(
				length(c(which(LEN_H[,x]>=330 & LEN_H[,x]<629),which(LEN_N[,x]>=330 & LEN_N[,x]<629))),# STOCK
				length(c(which(LEN_H[,x]>=630 & LEN_H[,x]<839),which(LEN_N[,x]>=630 & LEN_N[,x]<839))),# QUALITY
				length(c(which(LEN_H[,x]>=840 & LEN_H[,x]<1039),which(LEN_N[,x]>=840 & LEN_N[,x]<1039))),# PREFERRED
				length(c(which(LEN_H[,x]>=1040 & LEN_H[,x]<1269),which(LEN_N[,x]>=1040 & LEN_N[,x]<1269))),# MEMORABLE
				length(c(which(LEN_H[,x]>=1270),which(LEN_N[,x]>=1270))))# TROPHY
				out<-trunc(out/sum(out)*100)
			return(out) 
			})
			sq<- rbind(sq,PSD[1,])
			qp<- rbind(qp,PSD[2,])
			pm<- rbind(pm,PSD[3,])
			mt<- rbind(mt,PSD[4,])
			tr<- rbind(tr,PSD[5,])
			}
		
		# SUMMARIES
		## ABUNDANCE
		N_N_SUM<-rbind(N_N_SUM,colSums(Z_N))  # TOTAL AGE-1+ ABUNDANCE
		N_H_SUM<-rbind(N_H_SUM,colSums(Z_H))  # TOTAL AGE-1+ ABUNDANCE
		## AGE-1 RECRUITS; NATURAL ORIGIN
		indx<- lapply(1:inputs$nreps,function(x){out<- which(AGE_N[,x]>0 & AGE_N[,x]<24)}) 		
		recruits<- rbind(recruits,sapply(1:inputs$nreps,function(x) length(indx[[x]])))
		}# end i    		##})# end shiny progress bar
	return(list(natural=N_N_SUM, hatchery=N_H_SUM,sq=sq,qp=qp,pm=pm,mt=mt,tr=tr))	
	}

	
##### END FUNCTION	