



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
	tmp$b= input$b[indx]
	tmp$lw_er=input$lw_er[indx]
	
	## FECUNDITY
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

	
### CORE FUNCTION TO DO SIMULATIONS	
sim<- function(inputs)
	{
	
	# SET UP LIST FOR SIMULATION	
	dyn<- list(
		k= matrix(0,inputs$daug,inputs$nreps),
		Linf= matrix(0,inputs$daug,inputs$nreps),
		LEN= matrix(0,inputs$daug,inputs$nreps),
		WGT= matrix(0,inputs$daug,inputs$nreps),
		Z= matrix(0L,inputs$daug,inputs$nreps),
		AGE= matrix(0L,inputs$daug,inputs$nreps),
		MAT= matrix(0L,inputs$daug,inputs$nreps),
		MPS= matrix(0L,inputs$daug,inputs$nreps),
		SEX= matrix(0L,inputs$daug,inputs$nreps),
		SPN= matrix(0L,inputs$daug,inputs$nreps),
		ORIGIN= matrix(0L,inputs$daug,inputs$nreps))
	
	# INITIALIZATION

	indx<- cbind(rep(1:(inputs$natural+inputs$hatchery),inputs$nreps),
		sort(rep(1:inputs$nreps,(inputs$natural+inputs$hatchery))))
	## ASSIGN HATCHERY FISH AS A 1
	dyn$ORIGIN[cbind(rep(1:inputs$hatchery,inputs$nreps),
		sort(rep(1:inputs$nreps,inputs$hatchery)))]<- 1
	## FISH ALIVE AT INITIALIZATION [Z]
	dyn$Z[indx]<- 1


	
	## INIITIALIZE GROWTH COEFFICIENTS
	## ASSUMES THAT GROWTH IS NOT HERITABLE
	tmp<- ini_growth(x=inputs$nreps,n=nrow(Linf),basin=tolower(inputs$basin)) 	
	dyn$Linf[]<- tmp$linf
	dyn$k[]<- tmp$k
	
	

	
	## INITIALIZE LENGTH
	dyn$LEN[indx]<-ini_length(linf= dyn$Linf[indx],	basin=inputs$basin)
	
	
	## INITIALIZE WEIGHT GIVEN LENGTH
	dyn$WGT[indx]<-ini_wgt(a=inputs$a,b=inputs$b,len=dyn$LEN[indx],er=inputs$lw_er)


	
	## INITIALIZE SEX OF FISH
	dyn$SEX[indx]<-ini_sex(n=nrow(indx),ratio=inputs$sexratio)

	
	## INITIALIZE AGE IN MONTHS 
	dyn$AGE[indx]<- ini_age(len=dyn$LEN[indx],linf=dyn$Linf[indx],k=dyn$k[indx],sizeAtHatch=7,maxAge=inputs$maxage)
	
	## INITIALIZE WHETHER A FISH IS SEXUALLY MATURE									
	dyn$MAT[indx]<-ini_maturity(k=inputs$mat_k,	len=dyn$LEN[indx],age_mat=inputs$age_mat)


	## INITIALIZE TIME SINCE SPAWNING
	dyn$MPS[indx]<-ini_mps(n=(inputs$hatchery+inputs$natural),mature=dyn$MAT[indx])
	
	
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
		dyn$RKM<- matrix(0,inputs$daug,inputs$nreps)# float
		
		# INITIALIZE LOCATION OF ADULTS
		dyn$RKM[indx]<-ini_rkm(n=inputs$hatchery+inputs$natural,
			type=inputs$adult_spatial_structure,
			bend_lengths=inputs$bend_lengths)
	
		# INITIALIZE AGE-0 IN EACH BEND
		dyn$AGE_0_N_BND<-rmultinom(inputs$nreps,inputs$natural_age0,inputs$natural_age0_rel_dens)
		dyn$AGE_0_H_BND<-rmultinom(inputs$nreps,inputs$hatchery_age0,inputs$hatchery_age0_rel_dens)
		}
	# END INITIALIZATION ##########	
		
		
		
		
		
		
	# VECTOR OF MONTHS
	## STARTS IN JANUARY
	m<- rep(c(1:12),inputs$nyears) 	
	
	# SUMMARIES
	## ABUNDANCE (AGE-1+)
	N_SUM<-colSums(Z)

	## AGE-1 RECRUITS; NATURAL ORIGIN
	indx<- lapply(1:inputs$nreps,function(x){out<- which(AGE[,x]>0 & AGE[,x]<24 & Z[,x]==1 & ORIGIN[,x]==0)}) 

	
	recruits<- matrix(sapply(1:inputs$nreps,function(x) length(indx[[x]])),nrow=1)
	sq<-qp<-pm<-mt<-tr<-matrix(0,ncol=input$nreps,nrow=length(m))# MATRIX
	mn_wght<-biomass<- matrix(0,nrow=length(m),ncol=inputs$nreps)
	
	
	# PROGRESS BAR
	pb<-txtProgressBar(min=1,max=length(m),initial=0,char="*",style=3)

	# SIMULATE POPULATION DYNAMICS GIVEN INITIAL STATES
	for(i in 1:length(m)) # M IS A VECTOR OF MONTHS 1:12, REPEATED FOR NYEARS
		{
		#k=k+1
		#incProgress(1/length(m))
		setTxtProgressBar(pb, i)

		indx<- lapply(1:inputs$nreps,function(x){which(dyn$Z[,x]==1)}) 
		tmp<- unlist(lapply(1:inputs$nreps,function(x) length(indx[[x]])))
		indx<- cbind(unlist(indx),sort(rep(1:inputs$nreps,tmp)))
	
		### UPDATE WHETHER FISH SURVIVED PRIOR TO UPDATING OTHER STUFF  
		dyn$Z[indx]<- dSurvival(phi_age=inputs$phi,	age=AGE[indx])   # slow

		
		### UPDATE TOTAL LENGTH 
		dyn$LEN[indx]<-dLength(k=k[indx],linf=Linf[indx],dT=1/12,length1=dyn$LEN[indx])
	
		### UPDATE WEIGHT # slow
		dyn$WGT[indx]<-dWeight(len=dyn$LEN[indx],a=inputs$a,b=inputs$b,er=inputs$lw_er)

	

	
		# RECRUITMENT & SPAWNING MODELULE
		if(inputs$recruit==TRUE) ####fixme####
			{
			
			
			# [1] EXPAND AGE-0 COHORTS TO INDIVIDUALS			
			## INDEX OF OPEN SLOTS TO PUT AGE-0 FISH TRANSITIONING
			## TO AGE-1.... HAPPY BIRTHDAY LITTLE FIDDIES
			
			# RECRUIT HATCHERY FISH TO SUPER POPULATION
			if(sum(dyn$AGE_0_H_BND)>0)
				{
				indx<- unlist(sapply(1:inputs$nreps,function(x)
					{
					which(dyn$Z[,x]==0)[1:sum(dyn$AGE_0_H_BND[,x])]
					}))		
				indx<- cbind(c(indx),sort(rep(1:inputs$nreps,colSums(dyn$AGE_0_H_BND))))
				dyn$Z[indx]<-1    # ADD NEW 1 YEAR OLD RECRUITS
				dyn$AGE[indx]<-12 # UPDATE AGE OF RECRUITS		
				dyn$LEN[indx]<-rnorm(length(indx[,1]),inputs$recruit_mean_length,inputs$recruit_length_sd)				
				dyn$WGT[indx]<-rlnorm(length(indx[,1]),log(inputs$a*dyn$LEN[indx_h]^inputs$b),inputs$lw_er)	
				dyn$ORIGIN[indx]<- 1
				dyn$MAT[indx]<-0# ASSIGN MATURATION STATUS OF NEW RECRUITS
				dyn$SEX[indx]<-rbinom(length(indx[,1]),1,0.5)# ASSIGN SEX TO RECRUITS				
				if(inputs$spatial==TRUE)
					{
					dyn$RKM[indx]<- bend2rkm(c(unlist(sapply(1:inputs$nreps,# ASSIGN LOCATION OF RECRUITS
						function(x){rep(1:inputs$n_bends,AGE_0_H_BND[,x])}))))
					}	
				}
			# RECRUIT NATURAL FISH TO SUPER POPULATION
			if(sum(AGE_0_N_BND)>0)
				{
				indx<- unlist(sapply(1:inputs$nreps,function(x)
					{
					which(Z[,x]==0)[1:sum(AGE_0_N_BND[,x])]
					}))		
				indx<- cbind(c(indx),sort(rep(1:inputs$nreps,colSums(AGE_0_H_BND))))
				dyn$Z[indx]<-1    # ADD NEW 1 YEAR OLD RECRUITS
				dyn$AGE[indx]<-12 # UPDATE AGE OF RECRUITS		
				dyn$LEN[indx]<-rnorm(length(indx[,1]),inputs$recruit_mean_length,inputs$recruit_length_sd)				
				dyn$WGT[indx]<-rlnorm(length(indx[,1]),log(inputs$a*dyn$LEN[indx_h]^inputs$b),inputs$lw_er)	
				dyn$ORIGIN[indx]<- 1
				dyn$MAT[indx]<-0# ASSIGN MATURATION STATUS OF NEW RECRUITS
				dyn$SEX[indx]<-rbinom(length(indx[,1]),1,0.5)# ASSIGN SEX TO RECRUITS				
				if(inputs$spatial==TRUE)
					{
					RKM[indx]<- bend2rkm(c(unlist(sapply(1:inputs$nreps,# ASSIGN LOCATION OF RECRUITS
						function(x){rep(1:inputs$n_bends,AGE_0_N_BND[,x])}))))
					}				
				}
			dyn$AGE_0_N_BND[]<-0 # ZERO OUT AGE 0 AFTER THEY MOVE TO AGE-1
			dyn$AGE_0_H_BND[]<-0 # ZERO OUT AGE 0 AFTER THEY MOVE TO AGE-1	
			
			
			
			## ASSIGN WHETHER A FISH WILL SPAWN
			## GIVEN TIME SINCE LAST SPAWN
			dyn$SPN[indx]<-spawn(mps=dyn$MPS[indx],a=inputs$spn_a,
				b=inputs$spn_b,mature=dyn$MAT[indx])
				
			## CALCULATE THE NUMBER OF EGGS PRODUCED
			dyn$EGGS[indx]<-fecundity(fl=LEN_H[indx],a=inputs$fec_a,
				b=inputs$fec_b,	er=inputs$fec_er,
				sex=dyn$SEX[indx],spawn=dyn$SPN[indx])	
			
			if(inputs$spatial==FALSE)
				{
				dyn$AGE_0_N_BND<- colSums(EGGS)
				}
				
				
				
				
			} # END RECRUITMENT

			

		# SPATIAL SUBMODULE
		if(inputs$spatial==TRUE)
			{				
			## EGGS PER REACH
			AGE_0_BND<- sapply(1:inputs$nreps,function(x){
				E<-tapply(EGGS[,x],
					factor(rkm2bend(RKM[,x]),	levels=c(1:inputs$n_bends)),
					sum)
				E[is.na(E)]<-0
				return(E)})	
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
				
				### FERTILIZATION & EMBRYOS
				
				### FREE EMBRYOS & DRIFT
				# AGE_0_N_BND<-c()

				
				### AGE-0
				AGE_0_N_BND<-sapply(1:inputs$nreps,dFreeEmbryoDrift,
					nbends=nrow(AGE_0_N_BND),
					loc=AGE_0_N_BND,
					prob=inputs$prob)				

				## TRANSITION OF FREE EMBRYO TO EXOGENOUSLY FEEDING LARVAE & AGE-0
				AGE_0_N_BND<- sapply(1:inputs$nreps,dFEtoEFL,
					n=nrow(AGE_0_N_BND),
					total=AGE_0_N_BND,
					phi=inputs$phi3^(1/12))	
				
			
			


		
		
		

			# UPDATE MOVEMENT
			
			## ADULT MOVEMENT
			RKM[indx]<-loc2(loc1=RKM[indx],er=10,month=m[i])
	

			## UPDATE ANY AGE-0 MOVEMENT #########################################################################
			#AGE_0_N<-	
			#AGE_0_H<-					
			} # END SPATIAL

		# UPDATE MONTHS SINCE SPAWNING
		MPS[indx]<-MPS[indx]+1 

		
		
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
			indx<- lapply(1:inputs$nreps,function(x){out<- which(Z[,x]==0)[1:inputs$yearling]}) 
			indx<- cbind(unlist(indx),sort(rep(1:inputs$nreps,inputs$yearling)))
			Z[indx]<- 1### ADD NEWLY STOCKED INDIVIDUALS TO Z_H
			LEN[indx]<- rnorm(inputs$yearling*inputs$nreps,inputs$yearling_mn,inputs$yearling_sd)# UPDATE LENGTH
			WGT[indx]<- rlnorm(length(indx[,1]),log(inputs$a*LEN[indx]^inputs$b),inputs$lw_er)	# UPDATE WEIGHT
			AGE[indx]<- inputs$yearling_age # ASSIGN AGE
			MAT[indx]<- 0	# ASSIGN MATURITY
			RKM[indx]<- inputs$yearling_stocking_rkm# ASSIGN LOCATION OF STOCKED INDIVIDUALS
			MAT[indx]<- 0  # ASSIGN MATURATION STATUS OF NEW RECRUITS
			SEX[indx]<-rbinom(length(indx[,1]),1,0.5)# ASSIGN SEX TO RECRUITS	
			ORIGIN[indx]<-1
			}
		# END STOCKING ##################################################################	

		
		
		# PSD MODULE ####################################################################
		if(inputs$size_indices==TRUE & m[i]==6)
			{
			PSD<- sapply(1:input$nreps,function(x){
				out<-c(
				length(c(which(LEN[,x]>=330 & LEN[,x]<629))),# STOCK
				length(c(which(LEN[,x]>=630 & LEN[,x]<839))),# QUALITY
				length(c(which(LEN[,x]>=840 & LEN[,x]<1039))),# PREFERRED
				length(c(which(LEN[,x]>=1040 & LEN[,x]<1269))),# MEMORABLE
				length(c(which(LEN[,x]>=1270))))# TROPHY
				out<-trunc(out/sum(out)*100)
			return(out) 
			})
			sq[i,]<- PSD[1,]
			qp[i,]<- PSD[2,]
			pm[i,]<- PSD[3,]
			mt[i,]<- PSD[4,]
			tr[i,]<- PSD[5,]
			}
		# END PSD MODULE ################################################################
		
		
		
	
		# SUMMARIES #####################################################################
		## ABUNDANCE
		N_SUM<-rbind(N_SUM,colSums(Z))  # TOTAL AGE-1+ ABUNDANCE
		
		## AGE-1 RECRUITS; NATURAL ORIGIN
		indx<- lapply(1:inputs$nreps,function(x){out<- which(AGE[,x]>0 & AGE[,x]<24 & ORIGIN[,x]==0 & Z[,x]==1)}) 		
		recruits<- rbind(recruits,sapply(1:inputs$nreps,function(x) length(indx[[x]])))
		biomass[i,]<- colSums(WGT)
		mn_wght[i,]<- colSums(WGT)/colSums(Z)
		
		
		# END SUMMARIES #################################################################
		
		# SENESCENCE
		if(length(which(AGE>=inputs$maxage*12))>0)
			{
			Z[AGE>=inputs$maxage*12]<- 0
			}
		
		
		
		# ZERO OUT LENGTH AND WEIGHTS FOR DEAD FISH
		LEN<- LEN*Z
		WGT<- WGT*Z

		
		# need to draw new linf and k for new fish...
		
		}# end i    		##})# end shiny progress bar
		# END LOOP ######################################################################

	len_init<-len_init[tolower(len_init$basin)==tolower(inputs$basin),]
	len_init$rel_freq<- c(0,diff(len_init$x))
	
	x<- sort(rep(inputs$startYear:(inputs$startYear+inputs$nyears-1),12))+rep(1:12/12,inputs$nyears)
	
	
	out<-list(total=N_SUM, 
		years=x,
		sq=sq,qp=qp,pm=pm,mt=mt,tr=tr,
		len_init=len_init,
		biomass=biomass,
		mn_wght=mn_wght)
		
	fn<-paste0("./output/",inputs$output_name,"/",inputs$output_name,"-output.txt")
	dput(out,fn)
	fn<-paste0("./output/",inputs$output_name,"/",inputs$output_name,"-input.txt")
	dput(inputs,fn)
	fn<-paste0("./output/",inputs$output_name,"/",inputs$output_name,".Rdata")
	save(out,inputs, file=fn)
	return(out)	
	}

	
##### END FUNCTION	