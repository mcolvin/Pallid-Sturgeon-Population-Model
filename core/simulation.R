

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
		ORIGIN= matrix(0L,inputs$daug,inputs$nreps),
		EGGS= matrix(0L,inputs$daug,inputs$nreps))
	
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
	tmp<- ini_growth(x=inputs$nreps,n=inputs$daug,
		mu_ln_linf=inputs$ln_Linf_mu,
		mu_ln_k=inputs$ln_k_mu,
		vcv=inputs$vcv) 	
	dyn$Linf[]<- tmp$linf
	dyn$k[]<- tmp$k

	
	## INITIALIZE LENGTH
	dyn$LEN[indx]<-ini_length(n=length(indx), 
		basin=inputs$basin,
		origin=dyn$ORIGIN[indx], 
		spatial=FALSE,
		linf= dyn$Linf[indx])
	
	## INITIALIZE WEIGHT GIVEN LENGTH
	### ASSUMES NO EFFECT OF ORIGIN
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
		dyn$AGE_0_N_BND<-as.matrix(rmultinom(inputs$nreps,inputs$natural_age0,inputs$natural_age0_rel_dens))
		dyn$AGE_0_H_BND<-as.matrix(rmultinom(inputs$nreps,inputs$hatchery_age0,inputs$hatchery_age0_rel_dens))
		}
	# END INITIALIZATION ##########	
		
		
		
		
		
		
	# VECTOR OF MONTHS
	## STARTS IN JANUARY
	m<- rep(c(1:12),inputs$nyears) 	
	
	# SUMMARIES
	## ABUNDANCE (AGE-1+)
	N_SUM<-colSums(dyn$Z)

	## AGE-1 RECRUITS; NATURAL ORIGIN
	indx<- lapply(1:inputs$nreps,function(x){out<- which(dyn$AGE[,x]>0 & dyn$AGE[,x]<24 & 
		dyn$Z[,x]==1 & dyn$ORIGIN[,x]==0)}) 

	
	recruits<- matrix(sapply(1:inputs$nreps,function(x) length(indx[[x]])),nrow=1)
	sq<-qp<-pm<-mt<-tr<-matrix(0,ncol=input$nreps,nrow=length(m))# MATRIX
	mn_wght<-biomass<- matrix(0,nrow=length(m),ncol=inputs$nreps)
	
	init_summary<- data.frame(len=dyn$LEN[,1],
		linf=dyn$Linf[,1],
		k=dyn$k[,1],
		age=dyn$AGE[,1])
	
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
		dyn$Z[indx]<- dSurvival(phi_age=inputs$phi,	age=dyn$AGE[indx])   

		
		### UPDATE TOTAL LENGTH 
		dyn$LEN[indx]<-dLength(k=dyn$k[indx],linf=dyn$Linf[indx],dT=1/12,length1=dyn$LEN[indx])
	
		### UPDATE WEIGHT # slow
		dyn$WGT[indx]<-dWeight(len=dyn$LEN[indx],a=inputs$a,b=inputs$b,er=inputs$lw_er)

	

	
		# RECRUITMENT & SPAWNING MODELULE
		if(inputs$recruit==TRUE & m[i]==6) 
			{
			# [1] EXPAND AGE-0 COHORTS TO INDIVIDUALS			
			## INDEX OF OPEN SLOTS TO PUT AGE-0 FISH TRANSITIONING
			## TO AGE-1.... HAPPY BIRTHDAY LITTLE FIDDIES
			
			# RECRUIT HATCHERY FISH TO SUPER POPULATION
			if(sum(dyn$AGE_0_H_BND)>0)
				{
				indxr<- unlist(sapply(1:inputs$nreps,function(x)
					{
					which(dyn$Z[,x]==0)[1:sum(dyn$AGE_0_H_BND[,x])]
					}))		
				indxr<- cbind(c(indxr),sort(rep(1:inputs$nreps,colSums(dyn$AGE_0_H_BND))))
				dyn$Z[indxr]<-1    # ADD NEW 1 YEAR OLD RECRUITS
				dyn$AGE[indxr]<-12 # UPDATE AGE OF RECRUITS		
				dyn$LEN[indxr]<-rnorm(length(indxr[,1]),inputs$recruit_mean_length,inputs$recruit_length_sd)				
				dyn$WGT[indxr]<-rlnorm(length(indxr[,1]),log(inputs$a*dyn$LEN[indxr]^inputs$b),inputs$lw_er)	
				dyn$ORIGIN[indxr]<- 1
				dyn$MAT[indxr]<-0# ASSIGN MATURATION STATUS OF NEW RECRUITS
				dyn$SEX[indxr]<-rbinom(length(indxr[,1]),1,0.5)# ASSIGN SEX TO RECRUITS				
				if(inputs$spatial==TRUE)
					{
					dyn$RKM[indxr]<- bend2rkm(c(unlist(sapply(1:inputs$nreps,# ASSIGN LOCATION OF RECRUITS
						function(x){rep(1:inputs$n_bends,dyn$AGE_0_H_BND[,x])}))))
					}	
				}
			# RECRUIT NATURAL ORIGIN FISH TO SUPER POPULATION
			if(sum(dyn$AGE_0_N_BND)>0)
				{
				#if(min((nrow(dyn$Z)-colSums(dyn$Z))-dyn$AGE_0_N_BND)<0) {do not add}# NUMBER OF SLOTS OPEN

				# INDEX OF OPEN SLOTS
				indxr<- unlist(sapply(1:inputs$nreps,function(x)
					{
					which(dyn$Z[,x]==0)[1:sum(dyn$AGE_0_N_BND[,x])]
					}))		
				indxr<- cbind(c(indxr),sort(rep(1:inputs$nreps,colSums(dyn$AGE_0_N_BND))))
				dyn$Z[indxr]<-1    # ADD NEW 1 YEAR OLD RECRUITS
				dyn$AGE[indxr]<-12 # UPDATE AGE OF RECRUITS	
				
				# ASSIGN LENGTH GIVEN AGE
				dyn$LEN[indxr]<-dLength(k=dyn$k[indxr], linf=dyn$Linf[indxr],length1=7,dT=1)
				dyn$WGT[indxr]<-rlnorm(length(indxr[,1]),log(inputs$a*dyn$LEN[indxr]^inputs$b),inputs$lw_er)	
				dyn$ORIGIN[indxr]<- 1
				dyn$MAT[indxr]<-0# ASSIGN MATURATION STATUS OF NEW RECRUITS
				dyn$SEX[indxr]<-rbinom(length(indxr[,1]),1,0.5)# ASSIGN SEX TO RECRUITS				
				if(inputs$spatial==TRUE)
					{
					RKM[indxr]<- bend2rkm(c(unlist(sapply(1:inputs$nreps,# ASSIGN LOCATION OF RECRUITS
						function(x){rep(1:inputs$n_bends,dyn$AGE_0_N_BND[,x])}))))
					}		
				}
			dyn$AGE_0_N_BND[]<-0 # ZERO OUT AGE 0 AFTER THEY MOVE TO AGE-1
			dyn$AGE_0_H_BND[]<-0 # ZERO OUT AGE 0 AFTER THEY MOVE TO AGE-1	
			
			
			
			## ASSIGN WHETHER A FISH WILL SPAWN
			## GIVEN TIME SINCE LAST SPAWN
			dyn$SPN[indx]<-spawn(mps=dyn$MPS[indx],a=inputs$spn_a,
				b=inputs$spn_b,
				mature=dyn$MAT[indx])
				
			## CALCULATE THE NUMBER OF EGGS PRODUCED
			dyn$EGGS[indx]<-fecundity(fl=dyn$LEN[indx],a=inputs$fec_a,
				b=inputs$fec_b,	er=inputs$fec_er,
				sex=dyn$SEX[indx],spawn=dyn$SPN[indx],
				mature=dyn$MAT[indx])	
			
			## UPDATE THE NUMBER OF MONTHS SINCE SPAWNING FOR FISH JUST SPAWNED
			dyn$MPS[which(dyn$EGGS>0)]<-0


			
			if(inputs$spatial==FALSE)
				{
				dyn$AGE_0_N_BND<- matrix(colSums(dyn$EGGS),nrow=1) # NUMBER OF EGGS
				dyn$AGE_0_N_BND[]<- rbinom(inputs$nreps,dyn$AGE_0_N_BND,inputs$pr_embryo) # eggs --> embryos
				dyn$AGE_0_N_BND[]<- rbinom(inputs$nreps,dyn$AGE_0_N_BND,inputs$phi_embryo) # embryos --> free embryos
				dyn$AGE_0_N_BND[]<- rbinom(inputs$nreps,dyn$AGE_0_N_BND,inputs$phi_free_embryo) # free embryos --> age0
				}

				
				
			# SPATIAL SUBMODULE
			if(inputs$spatial==TRUE)
				{				
				## EGGS PER REACH
				AGE_0_BND<- sapply(1:inputs$nreps,function(x){
					E<-tapply(EGGS[,x],
						factor(rkm2bend(RKM[,x]),levels=c(1:inputs$n_bends)),
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
			
			} # END RECRUITMENT
		
		# UPDATE MONTHS SINCE SPAWNING
		dyn$MPS[indx]<-dyn$MPS[indx]+1 		
		
		
		
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
				length(c(which(dyn$LEN[,x]>=330 & dyn$LEN[,x]<629))),# STOCK
				length(c(which(dyn$LEN[,x]>=630 & dyn$LEN[,x]<839))),# QUALITY
				length(c(which(dyn$LEN[,x]>=840 & dyn$LEN[,x]<1039))),# PREFERRED
				length(c(which(dyn$LEN[,x]>=1040 & dyn$LEN[,x]<1269))),# MEMORABLE
				length(c(which(dyn$LEN[,x]>=1270))))# TROPHY
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
		N_SUM<-rbind(N_SUM,colSums(dyn$Z))  # TOTAL AGE-1+ ABUNDANCE
		
		## AGE-1 RECRUITS; NATURAL ORIGIN
		indx<- lapply(1:inputs$nreps,function(x){out<- which(dyn$AGE[,x]>0 & dyn$AGE[,x]<24 & dyn$ORIGIN[,x]==0 & dyn$Z[,x]==1)}) 		
		recruits<- rbind(recruits,sapply(1:inputs$nreps,function(x) length(indx[[x]])))
		biomass[i,]<- colSums(dyn$WGT)
		mn_wght[i,]<- colSums(dyn$WGT)/colSums(dyn$Z)
		
		
		# END SUMMARIES #################################################################
		
		# SENESCENCE
		if(length(which(dyn$AGE>=inputs$maxage*12))>0)
			{
			dyn$Z[dyn$AGE>=inputs$maxage*12]<- 0
			}
		
		
		
		# ZERO OUT LENGTH AND WEIGHTS FOR DEAD FISH
		dyn$LEN<- dyn$LEN*dyn$Z
		dyn$WGT<- dyn$WGT*dyn$Z

		
		# need to draw new linf and k for new fish...
		}# end i    		##})# end shiny progress bar
		# END FOR LOOP ######################################################################

		
		
		
	len_init<-len_init[tolower(len_init$basin)==tolower(inputs$basin),]
	len_init$rel_freq<- c(0,diff(len_init$x))
	
	x<- sort(rep(inputs$startYear:(inputs$startYear+inputs$nyears-1),12))+rep(1:12/12,inputs$nyears)
	
	
	out<-list(total=N_SUM, 
		years=x,
		sq=sq,qp=qp,pm=pm,mt=mt,tr=tr,
		biomass=biomass,
		mn_wght=mn_wght,
		init_summary=init_summary)
		
	fn<-paste0("./output/",inputs$output_name,"/",inputs$output_name,"-output.txt")
	dput(out,fn)
	fn<-paste0("./output/",inputs$output_name,"/",inputs$output_name,"-input.txt")
	dput(inputs,fn)
	fn<-paste0("./output/",inputs$output_name,"/",inputs$output_name,".Rdata")
	save(out,inputs, file=fn)
	return(out)	
	}

	
##### END FUNCTION	