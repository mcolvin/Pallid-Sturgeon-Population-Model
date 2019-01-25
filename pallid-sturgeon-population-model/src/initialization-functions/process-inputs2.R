
### PROCESS INPUTS TO INITILIZE AND SIMULATE POPULATION	
modelInputs<- function(input=NULL, 
                        basin=NULL, 
                        spatial=FALSE,
                        bend_meta=NULL)
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
	  #####################################################################
	  #   CHOOSE BEST FORMAT DEPENDENT ON WHAT IS EASIER TO ESTIMATE      #
	  #   ADJUST RELATED INPUTS & FUNCTIONS AS NEEDED BASED ON ESTIMATES  #              #
	  #####################################################################
	#### INPUT PROBABILITY OF FIRST SPAWN AT AGE A, GIVEN FISH WAS 
	#### IMMATURE AT AGE A-1 
	tmp$mat_dist<- 1/(1+exp(-tmp$mat_k*(1:tmp$maxage-tmp$age_mat_50)))
	  # THIS FUNCTIONAL FORM, MODIFIED BY AN AGE_MIN AND AGE_MAX, WOULD 
	  # ALSO BE A GOOD MODEL FOR THE CDF PROBABLY GO WITH THIS AND OPTION
	  # BELOW FOR CREATING MAT_DIST
	tmp$mat_dist[1:(tmp$age_mat_min-1)]<-0
	#### RESULTING CDF FOR MATURE FISH
	tmp$mat_cdf<-rep(0, length(tmp$mat_dist))
	tmp$mat_cdf[1]<-tmp$mat_dist[1]
	for(i in 2:length(tmp$mat_cdf))
	{
	  tmp$mat_cdf[i]<-tmp$mat_cdf[i-1]+(1-tmp$mat_cdf[i-1])*tmp$mat_dist[i]
	}
	# #### OR INPUT CDF FOR MATURE FISH AND GET TRANSITION PROBS OUT
	# cdf<-c(rep(0,7), c(0.1, 0.6, 0.8, 0.9, 1), rep(1, 48))
	# mat_dist<-rep(0, length(cdf))
	# mat_dist[1]<-cdf[1]
	# for(i in 2:length(mat_dist))
	# {
	#   mat_dist[i]<-ifelse(cdf[i-1]==1, 1, (cdf[i]-cdf[i-1])/(1-cdf[i-1]))
	# }
	
	
	# STOCKING INPUTS
	### FINGERGLINGS
	tmp$fingerling<- data.frame(
			month=input$stockingInput[[basin]]$fingerling_month,
			mean_length=input$stockingInput[[basin]]$fingerling_mn,
			length_sd=input$stockingInput[[basin]]$fingerling_sd	,	
			number=input$stockingInput[[basin]]$fingerling,
			age=input$stockingInput[[basin]]$fingerling_age,
			bend=NA)#input$stockingInput$bend)
	### YEARLINGS
	tmp$yearling<-data.frame(
		month=input$stockingInput[[basin]]$yearling_month,
		mean_length=input$stockingInput[[basin]]$yearling_mn,
		length_sd=input$stockingInput[[basin]]$yearling_sd	,	
		number=input$stockingInput[[basin]]$yearling,
		age=input$stockingInput[[basin]]$yearling_age,
		bend=NA)#input$stockingInput$bend)

	# SIMULATION STUFF
	tmp <- c(tmp, input$simulationInput)
	tmp$spatial <- spatial
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
		
		## MONTHLY MOVEMENT MATRIX

		### FREE EMBRYOS
		#### UNIFORM RANDOM DRIFT:  ENTRY ij IS THE PROBABILTY OF DRIFTING
		####    FROM BEND i TO BEND j; ROW 1 IS FARTHEST DOWNSTREAM)
		tmp$p_retained<- input$spatialInput[[basin]]$p_retained
		tmp$drift_prob<- matrix(runif(tmp$n_bends*tmp$n_bends),nrow=tmp$n_bends,ncol=tmp$n_bends)
		tmp$drift_prob[upper.tri(tmp$drift_prob)]<-0
		tmp$drift_prob<- tmp$drift_prob/apply(tmp$drift_prob,1,sum)*tmp$p_retained
		tmp$drift_prob<- cbind(tmp$drift_prob, 1-tmp$p_retained)

		### ADULTS
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
		
	return(tmp)
}
