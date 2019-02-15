
### PROCESS INPUTS TO INITILIZE AND SIMULATE POPULATION	
modelInputs<- function(input=NULL, 
                       basin=NULL, 
                       spatial=FALSE,
                       bend_meta=NULL,
                       hatchery_name=FALSE,
                       genetics=FALSE)
	{
  ## ERROR HANDLING
  basin<-tolower(basin)
  if(basin!="lower" & basin!="upper")
  {
    return(print("Please specify either the upper or lower basin."))
  }
  if(spatial & is.null(bend_meta))
  {
    return(print("An input for bend_meta is required when spatial=TRUE.")) 
  }
  if(genetics)
  { 
    if(sum(input$stockingInput[[basin]]$fingerling$stocking_no)>sum(input$geneticsInput[[basin]]$fingerling$no_offspring)
       | sum(input$stockingInput[[basin]]$yearling$stocking_no)>sum(input$geneticsInput[[basin]]$yearling$no_offspring))
    {
      return(print("More fish are to be stocked than genetic information is available for.")) 
    }
  }
  
  # POPULATION INPUTS FOR GIVEN BASIN
  tmp <- input[[basin]]
	tmp$basin_inp <- ifelse(basin=="lower",0,1)
	tmp$basin <- basin
	## MODIFICATIONS 
	### LENGTH-WEIGHT
	tmp$a <- exp(tmp$a_prime)
  ### SURVIVAL   NOTE: SURVIVAL ERRORS CURRENTLY UNUSED!!!
	names(tmp)[which(names(tmp) %in% c("phi_age0_mean", "phi_age1_mean", 
	                        "phi_age2_mean"))]<-c("phi0", "phi1", "phi2") #parameter name change
	tmp$phi <- c(tmp$phi1,rep(tmp$phi2,tmp$maxage-1))
	### MATURATION
	#### SIGMOID FUNCTION (PROPORTION OF AGE-A FISH THAT ARE MATURE)
	tmp$propM<- 1/(1+exp(-tmp$mat_k*(1:tmp$maxage-tmp$age_mat_50)))
	#### ADJUST FOR MIN AND MAX AGE OF MATURATION
	tmp$propM[1:(tmp$age_mat_min-1)]<-0
	tmp$propM[tmp$age_mat_max:tmp$maxage]<-1
	#### PROBABILITY A FISH MATURES AT AGE A GIVEN THE FISH WAS IMMATURE 
	#### AT AGE A-1 (GIVEN CONSTANT SURVIVAL)
	tmp$pMatC<-rep(0, length(tmp$propM))
	tmp$pMatC[1]<-tmp$propM[1]
	for(i in 2:length(tmp$pMatC))
	{
	  tmp$pMatC[i]<-ifelse(tmp$propM[i-1]==1, 1, 
	                      (tmp$propM[i]-tmp$propM[i-1])/(1-tmp$propM[i-1]))
	}
	#### PROBABILITY A FISH MATURES AT AGE A (GIVEN CONSTANT SURVIVAL) 
	tmp$pMat<-rep(0, length(tmp$propM))
	tmp$pMat[1]<-tmp$propM[1]
	for(i in 2:length(tmp$pMat))
	{
	  tmp$pMat[i]<-tmp$propM[i]-tmp$propM[i-1]
	}
	#####################################################################
	#   CAN ALSO GO IN REVERSE IF ESTIMATING pMat (FISH MATURES AT AGE  #
	#    A GIVEN IMMATURE AT AGE A-1) IS EASIER                         #
	#####################################################################
	# #### RESULTING propM FOR MATURE FISH
	# tmp$propM<-rep(0, length(tmp$pMat))
	# tmp$propM[1]<-tmp$pMat[1]
	# for(i in 2:length(tmp$propM))
	# {
	#   tmp$propM[i]<-tmp$propM[i-1]+(1-tmp$propM[i-1])*tmp$pMat[i]
	# }
	
	# HATCHERY FISH DATA
	tmp$stockingHistory<-input$stockingHistory[[basin]]
	tmp$stockingHistory$MPStock<- (input$simulationInput$startYear-
	                                 tmp$stockingHistory$year-1)*12 +
	  12-tmp$stockingHistory$month
	tmp$stockingHistory$current_age<- tmp$stockingHistory$age +
	  tmp$stockingHistory$MPStock
	tmp$hatchery_age0<- subset(tmp$stockingHistory, current_age<12)
	tmp$stockingHistory<- subset(tmp$stockingHistory, current_age>=12)
	
	# SIMULATION STUFF
	tmp <- c(tmp, input$simulationInput)
	tmp$spatial <- spatial
	tmp$genetics <- genetics
	tmp$hatchery_name <- hatchery_name
	tmp$commit <- input$commit
	tmp$output_name <- input$output_name	
	tmp$version <- input$version	
	
	# SPATIAL
	## SPATIAL STRUCTURE ADULTS
	if(spatial==TRUE)
	{	
		## BEND DATA & META
		tmp$bend_meta <- bend_meta[[basin]]
		tmp$n_bends <- nrow(bend_meta[[basin]])	
		tmp$bend_lengths <- bend_meta[[basin]]$Length.RKM
		
		## MOVEMENT MATRICES

		### FREE EMBRYOS
		#### UNIFORM RANDOM DRIFT:  ENTRY ij IS THE PROBABILTY OF DRIFTING
		####    FROM BEND i TO BEND j; ROW 1 IS FARTHEST DOWNSTREAM)
		tmp$p_retained<- input$spatialInput[[basin]]$p_retained
		tmp$drift_prob<- matrix(runif(tmp$n_bends*tmp$n_bends),nrow=tmp$n_bends,ncol=tmp$n_bends)
		tmp$drift_prob[upper.tri(tmp$drift_prob)]<-0
		tmp$drift_prob<- tmp$drift_prob/apply(tmp$drift_prob,1,sum)*tmp$p_retained
		tmp$drift_prob<- cbind(tmp$drift_prob, 1-tmp$p_retained)
		
		### FINGERLING DISPERSAL
		#### UNIFORM RANDOM DRIFT WITH STRONGER ABILITY TO HOLD POSITION:  
		####    ENTRY ij IS THE PROBABILTY OF DRIFTING FROM BEND i TO BEND j; 
		####    ROW 1 IS FARTHEST DOWNSTREAM)
		# tmp$disp_prob<- matrix(runif(tmp$n_bends*tmp$n_bends),nrow=tmp$n_bends,ncol=tmp$n_bends)
		# tmp$disp_prob[upper.tri(tmp$disp_prob)]<-0
		# diag(tmp$disp_prob)<-c(0.5,0.5*(1:(tmp$nbends-1)))
		# tmp$disp_prob<- tmp$disp_prob/apply(tmp$disp_prob,1,sum)
		tmp$disp_prob<- matrix(0,nrow=tmp$n_bends,ncol=tmp$n_bends)
		for(i in 1:nrow(tmp$disp_prob))
		{
		  for(j in 1:i)
		  {
		    tmp$disp_prob[i,j]<-(1/2)^(i-j+1)
		  }
		}
		tmp$disp_prob<- tmp$disp_prob/apply(tmp$disp_prob,1,sum)

		### ADULTS (MONTHLY)
		#### NON-SPAWNING
		## ORIGINAL:
		tmp$adult_mov_prob<- matrix(runif(tmp$n_bends*tmp$n_bends,0,0.1),nrow=tmp$n_bends,ncol=tmp$n_bends)
		diag(tmp$adult_mov_prob)<-0.7
		tmp$adult_mov_prob<- tmp$adult_mov_prob/apply(tmp$adult_mov_prob,1,sum)
		# ## IF YOU WANT A PARTICULAR PROBABILITY OF STAYING IN A GIVEN BEND:
		# fidelity<-0.08 #PROBABILITY OF REMAINING IN THE SAME BEND 
		#   #UPPER MEAN  WAS ORIGINALLY: 0.7/(0.05*(156-1)+0.7)~0.08
		#   #fidelity<- 1/tmp$n_bends GIVES UNIFORM AMONG ALL BENDS INCLUDING 
		#   #   THE SAME BEND
		# tmp$adult_mov_prob<- matrix(runif(tmp$n_bends*tmp$n_bends),
		#                             nrow=tmp$n_bends,ncol=tmp$n_bends)
		# diag(tmp$adult_mov_prob)<-0
		# tmp$adult_mov_prob<- 
		#   tmp$adult_mov_prob*(1-fidelity)/apply(tmp$adult_mov_prob,1,sum)
		# diag(tmp$adult_mov_prob)<-fidelity
	#   ## IF YOU WANT A PARTICULAR PROBABILITY OF STAYING IN A GIVEN BEND 
	#   ## AND MOVEMENT PROBABILITY UNIFORM IN DISTANCE:
	#   fidelity<-0.08 #PROBABILITY OF REMAINING IN THE SAME BEND 
	#     #UPPER MEAN  WAS ORIGINALLY: 0.7/(0.05*(156-1)+0.7)~0.08
	#     #fidelity<- tmp$bend_lengths/sum(tmp$bend_lengths) GIVES UNIFORM
	#     #   IN DISTANCE FOR ALL BENDS IN THE BASIN
	#   tmp$adult_mov_prob<- matrix(rep(tmp$bend_lengths, each=tmp$n_bends), 
	#                             nrow=tmp$n_bends,ncol=tmp$n_bends)
	#   tmp$adult_mov_prob<- 
	#     tmp$adult_mov_prob*(1-fidelity)/(sum(tmp$bend_lengths)-tmp$bend_lengths)
	#   diag(tmp$adult_mov_prob)<-fidelity
# 	## IF UPPER VS. LOWER MOVEMENT DOES NOT DEPEND ON WHICH BEND YOU ARE IN:
# 		ust<-0.5 #PROBABILITY OF MOVING UPSTREAM GIVEN MOVEMENT
# 		# UPSTREAM
# 		adult_mov_probU<- matrix(0,nrow=tmp$n_bends,ncol=tmp$n_bends)
# 		adult_mov_probU[upper.tri(adult_mov_probU)]<- 
# 		  runif((tmp$n_bends^2-tmp$n_bends)/2)
# 		adult_mov_probU<- 
# 		  adult_mov_probU*ust*(1-fidelity)/apply(adult_mov_probU,1,sum)
# 		adult_mov_probU[tmp$n_bends,]<- c(rep(0, tmp$n_bends-1), ust*(1-fidelity))
# 		# DOWNSTREAM
# 		adult_mov_probL<- matrix(0,nrow=tmp$n_bends,ncol=tmp$n_bends)
# 		adult_mov_probL[lower.tri(adult_mov_probL)]<- 
# 		  runif((tmp$n_bends^2-tmp$n_bends)/2)
# 		adult_mov_probL<- 
# 		  adult_mov_probL*(1-ust)*(1-fidelity)/apply(adult_mov_probL,1,sum)
# 		adult_mov_probL[1,]<- c((1-ust)*(1-fidelity), rep(0, tmp$n_bends-1))
# 		# MOVEMENT MATRIX 
# 		tmp$adult_mov_prob<-adult_mov_probU+adult_mov_probL
#     diag(tmp$adult_mov_prob)<-diag(tmp$adult_mov_prob)+fidelity
#     rm(adult_mov_probL, adult_mov_probU)
		#### SPAWNING
		tmp$spn_bends<-input$spatialInput[[basin]]$spn_bends
		tmp$spn_mov_prob<- matrix(0,nrow=tmp$n_bends,ncol=tmp$n_bends)
		if(is.null(tmp$spn_bnd_probs))
		{
		  tmp$spn_bnd_probs<-runif(length(tmp$spn_bend))
		  tmp$spn_bnd_probs<-tmp$spn_bnd_probs/sum(tmp$spn_bnd_probs)
		}
		tmp$spn_mov_prob[, tmp$spn_bends]<- rep(tmp$spn_bnd_probs,
		                                        each=nrow(tmp$spn_mov_prob))
		# TO ADD NOISE ADD OTHER INPUTS EARLIER AND UNCOMMENT:
		#tmp$spn_mov_prob<- tmp$spn_mov_prob/apply(tmp$spn_mov_prob,1,sum)
		
		## SPATIAL STRUCTURE AGE-0
		pp<- runif(tmp$n_bends)
		tmp$natural_age0_rel_dens<- pp/sum(pp)
		pp<- runif(tmp$n_bends)
		tmp$hatchery_age0_rel_dens<- pp/sum(pp)

		## SPATIAL STRUCTURE AGE-1+
		pp<- runif(tmp$n_bends)
		tmp$natural_age1plus_rel_dens<- pp/sum(pp)
		pp<- runif(tmp$n_bends)
		tmp$hatchery_age1plus_rel_dens<- pp/sum(pp)		 
	}## END SPATIAL
	
	# STOCKING INPUTS
	### FINGERGLINGS
	tmp$fingerling<- input$stockingInput[[basin]]$fingerling
	### YEARLINGS
	tmp$yearling<- input$stockingInput[[basin]]$yearling
	if(spatial)
	{
	  tmp$bend_meta$upper_rkm<-tmp$bend_meta$LOWER_RIVER_MILE[1]*1.60934+cumsum(tmp$bend_meta$Length.RKM)
	  tmp$fingerling$bend<-unlist(lapply(1:nrow(tmp$fingerling),function(i)
	    {
	      return(min(which(tmp$bend_meta$upper>=tmp$fingerling$stocking_rkm[i])))
	    }))
	  tmp$yearling$bend<-unlist(lapply(1:nrow(tmp$yearling),function(i)
	  {
	    return(min(which(tmp$bend_meta$upper>=tmp$yearling$stocking_rkm[i])))
	  }))
	}
	if(!spatial)
	{
	  tmp$fingerling<-ddply(tmp$fingerling, .(month), summarize,
	                        age=mean(age),
	                        length_mn=ifelse(sum(stocking_no)!=0,
	                                         sum(length_mn*stocking_no)/sum(stocking_no),
	                                         mean(length_mn)),
	                        length_sd=max(length_sd),
	                        stocking_no=sum(stocking_no))
	  tmp$yearling<-ddply(tmp$yearling, .(month), summarize,
	                        age=mean(age),
	                        length_mn=ifelse(sum(stocking_no)!=0,
	                                         sum(length_mn*stocking_no)/sum(stocking_no),
	                                         mean(length_mn)),
	                        length_sd=max(length_sd),
	                        stocking_no=sum(stocking_no))
	}
	
	# GENETICS INPUTS
	if(genetics)
	{
	  tmp$genetics_info$fingerling<-input$geneticsInput[[basin]]$fingerling
	  tmp$genetics_info$yearling<-input$geneticsInput[[basin]]$yearling
	## HATCHERY FISH INPUTS W/ GENETICS
	  tmp$hatchery_info<- rbind(input$geneticsInput[[basin]]$age1plus, 
	                            input$geneticsInput[[basin]]$age0)
	  
	}
	# HATCHERY FISH INPUTS W/O GENETICS
	if(!genetics & hatchery_name)
	{
	  tmp$hatchery_info<-aggregate(no_stocked~hatchery+age,
	                               input$geneticsInput[[basin]]$age1plus, 
	                               sum) 
	  tmp2<-aggregate(no_stocked~hatchery+age,
	                  input$geneticsInput[[basin]]$age0,
	                  sum) 
	  tmp$hatchery_info<-rbind(tmp$hatchery_info, tmp2)
	  rm(tmp2)
	}
	
	return(tmp)
}
