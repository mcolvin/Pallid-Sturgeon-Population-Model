stocking<- function(fingerlings,
	fingerlings_month, yearlings, yearlings_month,month)
	{
	# THIS FUNCTION ADDS HATCHERY FISH TO THE DYNAMIC PARTS
	# OF THE SIMULATION.  
	
	if(fingerlings > 0 & fingerlings_month==month)
		{
		# ADD NUMBER OF FISH STOCKED IN A BEND
		dyn$AGE_0_H_BND[inputs$stocking_bend,]<- dyn$AGE_0_H_BND[inputs$stocking_bend,]+inputs$fingerling
		}

	## YEARLING STOCKING (AGE-1+); INDIVIDUALS
	if(yearlings>0 & yearlings_month==month)
		{
		### GET INDEXES OF OPEN SLOTS TO STICK STOCKED INDIVIDUALS
		indx<- lapply(1:inputs$nreps,function(x){out<- which(dyn$Z[,x]==0)[1:inputs$yearling]}) 
		indx<- cbind(unlist(indx),sort(rep(1:inputs$nreps,inputs$yearling)))
		dyn$Z[indx]<- 1### ADD NEWLY STOCKED INDIVIDUALS TO Z_H
		dyn$LEN[indx]<- rnorm(inputs$yearling*inputs$nreps,inputs$yearling_mn,inputs$yearling_sd)# UPDATE LENGTH
		dyn$WGT[indx]<- rlnorm(length(indx[,1]),log(inputs$a*dyn$LEN[indx]^inputs$b),inputs$lw_er)	# UPDATE WEIGHT
		dyn$AGE[indx]<- inputs$yearling_age # ASSIGN AGE
		dyn$MAT[indx]<- 0	# ASSIGN MATURITY
		dyn$RKM[indx]<- inputs$yearling_stocking_rkm# ASSIGN LOCATION OF STOCKED INDIVIDUALS
		dyn$MAT[indx]<- 0  # ASSIGN MATURATION STATUS OF NEW RECRUITS
		dyn$SEX[indx]<-rbinom(length(indx[,1]),1,0.5)# ASSIGN SEX TO RECRUITS	
		dyn$ORIGIN[indx]<-1
		}
	# END STOCKING ##################################################################	
	return(dyn)
	}