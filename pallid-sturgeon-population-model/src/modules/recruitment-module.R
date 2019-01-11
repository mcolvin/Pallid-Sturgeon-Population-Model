# RECRUITMENT & SPAWNING MODULE
recruitment_to_population<- function(inputs=NULL,
                                     dyn=NULL,
                                     spatial=FALSE)
	{
	# [1] EXPAND AGE-0 COHORTS TO INDIVIDUALS			
	## INDEX OF OPEN SLOTS TO PUT AGE-0 FISH TRANSITIONING
	## TO AGE-1.... HAPPY BIRTHDAY LITTLE FIDDIES
	
	# RECRUIT STOCKED HATCHERY FISH TO POPULATION
  ## NEED TO CODE THE STOCKING PART FOR THIS TO BE NON-ZERO 
	if(sum(dyn$AGE_0_H_BND)>0)
		{
		indxr<- unlist(sapply(1:inputs$nreps,function(x)
			{
			which(dyn$Z_H[,x]==0)[1:sum(dyn$AGE_0_H_BND[,x])]
			}))		
		indxr<- cbind(c(indxr),sort(rep(1:inputs$nreps,colSums(dyn$AGE_0_H_BND))))
		# ADD NEW 1 YEAR OLD RECRUITS
		dyn$Z_H[indxr]<-1 
		# ADD AGE OF RECRUITS
		dyn$AGE_H[indxr]<-12 	
		# UPDATE GROWTH COEFFICIENTS
		tmp<- ini_growth(n=sum(dyn$AGE_0_H_BND),
		                 mu_ln_Linf=inputs$ln_Linf_mu,
		                 mu_ln_k=inputs$ln_k_mu,
		                 vcv=inputs$vcv,
		                 maxLinf=inputs$maxLinf) 
		dyn$Linf_H[indxr]<-tmp$linf
		dyn$k_H[indxr]<-tmp$k
		# ADD  INITIAL LENGTH OF RECRUITS
		## METHOD 1: RECRUIT DISTRIBUTION
		dyn$LEN_H[indxr]<-rnorm(length(indxr[,1]),
		                        inputs$recruit_mean_length,
		                        inputs$recruit_length_sd)				
		## METHOD 2: LENGTH FROM AGE AND VB GROWTH 
		dyn$LEN_H[indxr]<-dLength(k=dyn$k_H[indxr], 
		                          linf=dyn$Linf_H[indxr],
		                          length1=7,
		                          dT=1)
		# ASSIGN SEX
		dyn$SEX_H[indxr]<-ini_sex(n=length(indxr[,1]),
		                          prob_F=inputs$sexratio)
		# NOTE: MATURATION, MPS, SPAWNING, AND EGGS ALL ZERO ALREADY
		#       AND WEIGHT WILL BE ASSIGNED WITHIN SIM
		# ASSIGN LOCATION OF RECRUITS
		if(inputs$spatial==TRUE)
			{
			dyn$RKM[indxr]<- bend2rkm(c(unlist(sapply(1:inputs$nreps,
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
			which(dyn$Z_N[,x]==0)[1:sum(dyn$AGE_0_N_BND[,x])]
			}))		
		indxr<- cbind(c(indxr),sort(rep(1:inputs$nreps,colSums(dyn$AGE_0_N_BND))))
		# ADD NEW 1 YEAR OLD RECRUITS
		dyn$Z_N[indxr]<-1    
		# ADD AGE OF RECRUITS
		dyn$AGE_N[indxr]<-12 	
		# UPDATE GROWTH COEFFICIENTS
		tmp<- ini_growth(n=sum(dyn$AGE_0_N_BND),
		                 mu_ln_Linf=inputs$ln_Linf_mu,
		                 mu_ln_k=inputs$ln_k_mu,
		                 vcv=inputs$vcv,
		                 maxLinf=inputs$maxLinf) 
		dyn$Linf_N[indxr]<-tmp$linf
		dyn$k_N[indxr]<-tmp$k
		# ADD  INITIAL LENGTH OF RECRUITS
		## METHOD 1: RECRUIT LENGTH DISTRIBUTION
		dyn$LEN_N[indxr]<-rnorm(length(indxr[,1]),
		                        inputs$recruit_mean_length,
		                        inputs$recruit_length_sd)				
		## METHOD 2: LENGTH FROM AGE AND VB GROWTH 
		dyn$LEN_N[indxr]<-dLength(k=dyn$k_N[indxr], 
		                          linf=dyn$Linf_N[indxr],
		                          length1=7,
		                          dT=1)
		# ASSIGN SEX
		dyn$SEX_N[indxr]<-ini_sex(n=length(indxr[,1]),
		                          prob_F=inputs$sexratio)
		# NOTE: MATURATION, MPS, SPAWNING, AND EGGS ALL ZERO ALREADY
		#       AND WEIGHT WILL BE ASSIGNED WITHIN SIM
		# ASSIGN LOCATION OF RECRUITS
		if(inputs$spatial==TRUE)
			{
			RKM[indxr]<- bend2rkm(c(unlist(sapply(1:inputs$nreps,
				function(x){rep(1:inputs$n_bends,dyn$AGE_0_N_BND[,x])}))))
			}		
		}
	dyn$AGE_0_N_BND[]<-0 # ZERO OUT AGE 0 AFTER THEY MOVE TO AGE-1
	dyn$AGE_0_H_BND[]<-0 # ZERO OUT AGE 0 AFTER THEY MOVE TO AGE-1	
	return(dyn)
	} # END RECRUITMENT
	
	
	
#	# SPATIAL SUBMODULE
#	if(inputs$spatial==TRUE)
#		{				
#		## EGGS PER REACH
#		AGE_0_BND<- sapply(1:inputs$nreps,function(x){
#			E<-tapply(EGGS[,x],
#				factor(rkm2bend(RKM[,x]),levels=c(1:inputs$n_bends)),
#				sum)
#			E[is.na(E)]<-0
#			return(E)})	
#		
#		
#		## FERTILIZATION
#		### HOW MANY FEMALES IN EACH BEND
#		FEM_BND<- sapply(1:inputs$nreps,function(x){
#			N<-tapply(Z_H[,x]*SEX_H[,x],
#				factor(rkm2bend(RKM_N[,x]),	levels=c(1:inputs$n_bends)),
#				sum)
#			N[is.na(N)]<-0
#			H<-tapply(Z_N[,x]*SEX_N[,x],
#				factor(rkm2bend(RKM_H[,x]),	levels=c(1:inputs$n_bends)),
#				sum)
#			H[is.na(H)]<-0				
#			return(N+H)})
#
#		### HOW MANY MALES IN EACH BEND
#		MAL_BND<- sapply(1:inputs$nreps,function(x){
#			N<-tapply(Z_H[,x]*(1-SEX_H[,x])*MAT_H[,x],
#				factor(rkm2bend(RKM_N[,x]),	levels=c(1:inputs$n_bends)),
#				sum)
#			N[is.na(N)]<-0
#			H<-tapply(Z_N[,x]*(1-SEX_N[,x])*MAT_N[,x],
#				factor(rkm2bend(RKM_H[,x]),	levels=c(1:inputs$n_bends)),
#				sum)
#			H[is.na(H)]<-0				
#			return(N+H)})			
#			
#		### FERTILIZATION & EMBRYOS
#			
#		### FREE EMBRYOS & DRIFT
#		# AGE_0_N_BND<-c()
#		### AGE-0
#		AGE_0_N_BND<-sapply(1:inputs$nreps,dFreeEmbryoDrift,
#			nbends=nrow(AGE_0_N_BND),
#			loc=AGE_0_N_BND,
#			prob=inputs$prob)				
#
#		## TRANSITION OF FREE EMBRYO TO EXOGENOUSLY FEEDING LARVAE & AGE-0
#		AGE_0_N_BND<- sapply(1:inputs$nreps,dFEtoEFL,
#			n=nrow(AGE_0_N_BND),
#			total=AGE_0_N_BND,
#			phi=inputs$phi3^(1/12))	
#		# UPDATE MOVEMENT
#		
#		## ADULT MOVEMENT
#		RKM[indx]<-loc2(loc1=RKM[indx],er=10,month=m[i])
#
#
#		## UPDATE ANY AGE-0 MOVEMENT #########################################################################
#		#AGE_0_N<-	
#		#AGE_0_H<-					
#		} # END SPATIAL	
#	