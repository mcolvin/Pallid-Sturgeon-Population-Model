# RECRUITMENT & SPAWNING MODELULE
recruitment_to_population<- function(spatial=FALSE)
	{
	# [1] EXPAND AGE-0 COHORTS TO INDIVIDUALS			
	## INDEX OF OPEN SLOTS TO PUT AGE-0 FISH TRANSITIONING
	## TO AGE-1.... HAPPY BIRTHDAY LITTLE FIDDIES
	
	# RECRUIT HATCHERY FISH TO POPULATION
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