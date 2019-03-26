
### PROCESS INPUTS TO INITILIZE AND SIMULATE POPULATION	
modelInputs<- function(input=NULL, 
                       basin=NULL, 
                       spatial=FALSE,
                       bend_meta=NULL,
                       migration=FALSE,
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
  # if(genetics)
  # { 
    # if(sum(input$stockingInput[[basin]]$fingerling$stocking_no)>sum(input$geneticsInput[[basin]]$fingerling$no_offspring)
    #    | sum(input$stockingInput[[basin]]$yearling$stocking_no)>sum(input$geneticsInput[[basin]]$yearling$no_offspring))
    # {
    #   return(print("More fish are to be stocked than genetic information is available for.")) 
    # }
  # }
  
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
	rm(i)
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
	
	# SIMULATION STUFF
	tmp <- c(tmp, input$simulationInput)
	tmp$spatial <- spatial
	tmp$migration <- migration
	tmp$genetics <- genetics
	tmp$hatchery_name <- hatchery_name
	tmp$commit <- input$commit
	tmp$output_name <- input$output_name	
	tmp$version <- input$version	
	
	# HATCHERY FISH DATA
	tmp$stockingHistory<-input$stockingHistory[[basin]]
	tmp$stockingHistory<- subset(tmp$stockingHistory, 
	                             year<tmp$startYear)
	## SEPERATE OUT AGE-0's
	tmp$stockingHistory$MPStock<- (tmp$startYear-
	                                 tmp$stockingHistory$year-1)*12 +
	  12-tmp$stockingHistory$month
	tmp$stockingHistory$current_age<- tmp$stockingHistory$age +
	  tmp$stockingHistory$MPStock
	tmp$hatchery_age0<- subset(tmp$stockingHistory, current_age<12)
	tmp$stockingHistory<- subset(tmp$stockingHistory, current_age>=12)
	## ADD IN SURVIVAL ESTIMATES BY AGE AND YEARS SINCE STOCKING
	### CALCULATE MONTHS IN EACH AGE CLASS
	tmp$stockingHistory$phi00<-ifelse(tmp$stockingHistory$age<3,
	                                  sapply(tmp$stockingHistory$current_age, 
	                                         min, 3)-
	                                    tmp$stockingHistory$age, 0)
	tmp$stockingHistory$phi0<-ifelse(tmp$stockingHistory$age<12 & 
	                                   tmp$stockingHistory$current_age>3,
	                                 sapply(tmp$stockingHistory$current_age, 
	                                        min, 12) - 
	                                   sapply(tmp$stockingHistory$age, 
	                                          max, 3), 0)
	tmp$stockingHistory$phi1<-ifelse(tmp$stockingHistory$age<24 & 
	                                   tmp$stockingHistory$current_age>12,
	                                 sapply(tmp$stockingHistory$current_age, 
	                                        min, 24) - 
	                                   sapply(tmp$stockingHistory$age, 
	                                          max, 12), 0)
	tmp$stockingHistory$phi2<-ifelse(tmp$stockingHistory$current_age>24,
	                                 tmp$stockingHistory$current_age-24, 
	                                 0)
	### ADD IN BASIN SPECIFIC SURVIVALS
	if(is.null(tmp$stockingHistory$survival_est))
	{
	  #### LOWER
	  if(basin=="lower")
	  {
	    ##### WILDHABER ET AL. 2017(a) ECOLOGICAL MODELLING
	    ##### W/ 1-3 ESTIMATED AS 0.00011/0.0510
	    # tmp$stockingHistory$survival_est<- 
	    #   0.0021^(tmp$stockingHistory$phi00/3)*
	    #   0.0510^(tmp$stockingHistory$phi0/9)*
	    #   0.3674^(tmp$stockingHistory$phi1/12)*
	    #   0.9220^(tmp$stockingHistory$phi2/12)
	    # sum(tmp$stockingHistory$number*tmp$stockingHistory$survival_est)
	    ##### STEFFENSEN ET AL. 2019 -- MAY ALSO WANT TO TRY MODELING WITH
	    #####  DECREASING AGE-1 SURVIVAL BY AGE CLASS... HOW MUCH OF A DECREASE IN
	    #####  SURVIVAL WOULD BE NEEDED TO MAKE CREATE THE ESTIMATES SEEN IN
	    #####  FIG. 7?
	    tmp$stockingHistory$survival_est<- 
	      0.075^(tmp$stockingHistory$phi00/12)*
	      0.075^(tmp$stockingHistory$phi0/12)*
	      0.279^(tmp$stockingHistory$phi1/12)*
	      0.945^(tmp$stockingHistory$phi2/12)
	    # sum(tmp$stockingHistory$number*tmp$stockingHistory$survival_est)
	    ##### LOWER AT 21,790 WHICH IS GREATER THAN THE PREDICTED 12,134 USED
	    ##### PREVIOUSLY
	    ##### COMPARE WITH STEFFENSEN ET AL. 2018 https://doi.org/10.1111/jai.13646
	    # aggregate(NUMBERS.STOCKED~YEAR_BORN, dat[which(dat$RPMA==4),], sum)
	    # (dat is calculated in compile-stocking-data.R)
	    ##### 1992 & 1997 EXCLUDED...LOOK INTO IF SHOULD EXCLUDE TOO
	    ##### 2004: KIRK HAS ONE MORE FISH THAN US
	    ##### 2008: WE HAVE 558 MORE
	    ##### 2010: WE HAVE 37 MORE
	    ##### 2011: WE HAVE 116 MORE
	    ##### 2012: WE HAVE 99 MORE
	    ##### 2013: WE HAVE 812 MORE
	    ##### 2015: WE HAVE 500 MORE
	  }
	  if(basin=="upper")
	  {
	    ##### USING ROTELLA 2017 THE BEST WE CAN QUICKLY
	    phi_fing<-input$stockingHistory$extra$upper$phi_fingerling
	    phi_year<-input$stockingHistory$extra$upper$phi_yearling
	    tmp$stockingHistory$survival_est<- 
	      phi_fing[1]^(tmp$stockingHistory$phi00/12)*
	      phi_fing[1]^(tmp$stockingHistory$phi0/12)*
	      phi_fing[2]^(tmp$stockingHistory$phi1/12)*
	      phi_fing[3]^(tmp$stockingHistory$phi2/12)
	    tmp$stockingHistory$survival_est<- 
	      ifelse(tmp$stockingHistory$age>=12,
	             phi_year[1]^(tmp$stockingHistory$phi1/12)*
	               phi_year[2]^(tmp$stockingHistory$phi2/12),
	             tmp$stockingHistory$survival_est)
	    # sum(tmp$stockingHistory$number*tmp$stockingHistory$survival_est)
	    ##### INDICATES 241,120 HATCHERY FISH VS. ESTIMATED 16,444 IN 2016
	    ##### BUT ALSO DATA SHOWS 1,666,804 STOCKINGS (WITH 1,663,460 WITH ""
	    ##### FOR THE FIN CURL ENTRY) IN RPMA 2 BETWEEN 1998 AND 2016, NOT
	    ##### 245,249 AS IN ROTELLA 2017.  WHY THE DISCREPANCY??? -- HOW TO TELL
	    ##### IF HAD IRIDOVIRUS?  DOES NOT SEEM TO BE INDICATED IN ANY COMMENTS
	    ##### (COUNTS IN ROTELLA ARE FOR DISEASE & FIN CURL FREE)
	    rm(phi_fing, phi_year)
	  }
	  ### ADD HERE TO MODIFY BY FAMILY, HATCHERY, OR YEAR-CLASS
	  ### CALCULATE CURRENT NUMBERS BY BASIN
	  ### NOTE: USING STOCKED AND SURVIVAL GAVE VERY DIFFERENT NUMBERS FROM 
	  ### POP. ESTIMATES, SO JUST USING TO DEFINE FAMILY LOT PROPORTIONS 
	  ### AND NOT TO DETERMINE THE NUMBER OF HATCHERY FISH AND AGE 
	  ### DISTRIBUTION
	}
	
	
	### NATURAL CATCH HISTORY: CURRENT PARENTS IN THE NATURAL POPULATION
	tmp$naturalCatchHistory<- input$catchHistory[[basin]]
	Fhist<-ddply(tmp$stockingHistory, .(mother), summarize,
	             first_encount=strptime(paste0(min(year),"-03-01"),
	                                    format="%Y-%m-%d", tz="UTC"),
	             last_encount=strptime(paste0(max(year),"-06-01"),
	                                   format="%Y-%m-%d", tz="UTC"))
	Fhist<- Fhist[-which(Fhist$mother %in% c("", "MIX", "UKNOWN", "GAVINS")),]
	Fhist$sex<- 1
	names(Fhist)[1]<-"individual"
	tmp$naturalCatchHistory<-rbind.fill(tmp$naturalCatchHistory, Fhist)
	
	Mhist<-ddply(tmp$stockingHistory, .(father), summarize,
	             first_encount=strptime(paste0(min(year),"-03-01"),
	                                    format="%Y-%m-%d", tz="UTC"),
	             last_encount=strptime(paste0(max(year),"-06-01"),
	                                   format="%Y-%m-%d", tz="UTC"))
	Mhist<- Mhist[-which(Mhist$father %in% c("", "MIX", "MIX2", "MIX*", 
	                                         "MIX**", "UKNOWN", "GAVINS")),]
	#NEED TO ALSO REMOVE LAST ENCOUNTERS THAT WERE DUE TO CRYOPRESERVED SPERM
	Mhist$sex<- 0
	names(Mhist)[1]<-"individual"
	tmp$naturalCatchHistory<-rbind.fill(tmp$naturalCatchHistory, Mhist)
	
	rm(Fhist, Mhist)
	tmp$naturalCatchHistory[is.na(tmp$naturalCatchHistory$length),"length"]<-0
	tmp$naturalCatchHistory[is.na(tmp$naturalCatchHistory$sex),"sex"]<- -999
	tmp$naturalCatchHistory<-ddply(tmp$naturalCatchHistory, .(individual), summarize,
	                               sex=max(sex),
	                               length=max(length),
	                               dol=last_encount[which.max(length)],
	                               first_encount=min(first_encount, na.rm=TRUE),
	                               last_encount=max(last_encount, na.rm=TRUE))
	tmp$naturalCatchHistory[which(tmp$naturalCatchHistory$length==0),"length"]<- NA
	tmp$naturalCatchHistory[which(tmp$naturalCatchHistory$sex==-999),"sex"]<- NA
	tmp$naturalCatchHistory$minAge<-round(as.numeric(difftime(tmp$naturalCatchHistory$dol, 
	                                                          tmp$naturalCatchHistory$first_encount,
	                                                          units = "days"))/
	                                        (365.25/12)) + tmp$age_mat_min*12
	
	tmp$naturalCatchHistory$dT<-round(as.numeric(difftime(tmp$naturalCatchHistory$last_encount, 
	                                                      tmp$naturalCatchHistory$dol,
	                                                      units = "days"))/(365.25/12))
	tmp$naturalCatchHistory$phi_dT<- round(as.numeric(difftime(strptime(paste0(tmp$startYear-1,"-12-31"),
	                                                                    format="%Y-%m-%d", tz="UTC"), 
	                                                           tmp$naturalCatchHistory$last_encount,
	                                                           units = "days"))/(365.25/12)) 
	tmp$naturalCatchHistory$pr<-tmp$phi2^(tmp$naturalCatchHistory$phi_dT/12)

	
	# SPATIAL
	## SPATIAL STRUCTURE ADULTS
	if(spatial==TRUE)
	{	
		## BEND DATA & META
		tmp$bend_meta <- bend_meta[[basin]]
		tmp$bend_meta$RIVER<-"MO"
    if(basin=="upper")
    {
		  YR <- data.frame(B_SEGMENT=22, BEND_NUM=1, 
		                   UPPER_RIVER_MILE=round(113*0.621371, 1),
		                   LOWER_RIVER_MILE=0, STATE="MT", Length.RKM=113,
		                   basin="upper", id=999,RIVER="YR") 
		    #Intake at 113 rkm from UBPSRWG 2004 Annual Report
		  YR$Length.RM<-YR$UPPER_RIVER_MILE-YR$LOWER_RIVER_MILE
		  tmp$bend_meta<-rbind.fill(tmp$bend_meta, YR)
		  rm(YR)
		  #YR<-rbind.fill(bend_meta$upper[1:max(which(bend_meta$upper$B_SEGMENT==4)),],
		  #                YR)
		  #YR<-rbind(YR, bend_meta$upper[min(which(bend_meta$upper$B_SEGMENT==3)):nrow(bend_meta$upper),])
    }
		tmp$n_bends <- nrow(tmp$bend_meta)	
		tmp$bend_lengths <- tmp$bend_meta$Length.RKM
    if(migration)
    {
      if(basin=="upper")
      {
        YR <- data.frame(B_SEGMENT=22, BEND_NUM=2, 
                         UPPER_RIVER_MILE=NA,
                         LOWER_RIVER_MILE=round(113*0.621371, 1), 
                         STATE="MT", basin="upper", id=1000, RIVER="YR") 
        tmp$bend_meta<-rbind.fill(tmp$bend_meta, YR)
        rm(YR)
        LS <- data.frame(B_SEGMENT=52, BEND_NUM=1, 
                         UPPER_RIVER_MILE=min(tmp$bend_meta$LOWER_RIVER_MILE),
                         LOWER_RIVER_MILE=NA, STATE="ND",
                         basin="upper", id=0, RIVER="MO")
        tmp$bend_meta<-rbind.fill(tmp$bend_meta, LS)
        rm(LS)
      }
      if(basin=="lower")
      {
        MS <- data.frame(B_SEGMENT=71, BEND_NUM=1, 
                         UPPER_RIVER_MILE=0,
                         LOWER_RIVER_MILE=NA, STATE="MO",
                         basin="lower", id=0, RIVER="MS")
        tmp$bend_meta<-rbind.fill(tmp$bend_meta, MS)
        rm(MS)
      }
		  tmp$outside_bends<-(tmp$n_bends+1):nrow(tmp$bend_meta)
    }
		
		## MOVEMENT MATRICES

		### FREE EMBRYOS
		#### UNIFORM RANDOM DRIFT:  ENTRY ij IS THE PROBABILTY OF DRIFTING
		####    FROM BEND i TO BEND j; ROW 1 IS FARTHEST DOWNSTREAM)
		tmp$p_retained<- input$spatialInput[[basin]]$p_retained
		tmp$drift_prob<- matrix(runif(tmp$n_bends*tmp$n_bends),
		                        nrow=tmp$n_bends, ncol=tmp$n_bends)
		tmp$drift_prob[upper.tri(tmp$drift_prob)]<-0
		if(basin=="upper")
		{
		  ## ADJUST FOR LOWER YELLOWSTONE
		  tmp$drift_prob[nrow(tmp$drift_prob), 
		                 (length(which(tmp$bend_meta$B_SEGMENT==4))+1):
		                   (ncol(tmp$drift_prob)-1)]<-0
		  tmp$p_retained<-c(tmp$p_retained, 
		                    input$spatialInput[[basin]]$LYR$p_retained)
		  if(migration)
		  {
		    ## ADD ON UPPER YELLOWSTONE
		    tmp$drift_prob <- rbind(tmp$drift_prob, runif(tmp$n_bends))
		    tmp$drift_prob[nrow(tmp$drift_prob), 
		                   (length(which(tmp$bend_meta$B_SEGMENT==4))+1):
		                     (ncol(tmp$drift_prob)-1)]<-0
		    tmp$p_retained<-c(tmp$p_retained, 
		                      input$spatialInput[[basin]]$UYR$p_passage*
		                        input$spatialInput[[basin]]$UYR$p_retained_given_passage)
		  }
		}
		tmp$drift_prob<- tmp$drift_prob/apply(tmp$drift_prob,1,sum)*tmp$p_retained
		if(migration & basin=="upper")
		{
		  tmp$drift_prob <- cbind(tmp$drift_prob, 
		                          c(rep(0, tmp$n_bends),
		                            1-input$spatialInput[[basin]]$UYR$p_passage))
		}
		tmp$drift_prob<- cbind(tmp$drift_prob, 1-rowSums(tmp$drift_prob))
		
		# ### FINGERLING DISPERSAL
		# ### NOT IN USE YET... NEEDS ADDITIONS FOR MIGRATION AS WELL
		# #### DRIFT WITH STRONGER ABILITY TO HOLD POSITION:  
		# ####    ENTRY ij IS THE PROBABILTY OF DRIFTING FROM BEND i TO BEND j; 
		# ####    ROW 1 IS FARTHEST DOWNSTREAM)
		# # tmp$disp_prob<- matrix(runif(tmp$n_bends*tmp$n_bends),nrow=tmp$n_bends,ncol=tmp$n_bends)
		# # tmp$disp_prob[upper.tri(tmp$disp_prob)]<-0
		# # diag(tmp$disp_prob)<-c(0.5,0.5*(1:(tmp$nbends-1)))
		# # tmp$disp_prob<- tmp$disp_prob/apply(tmp$disp_prob,1,sum)
		# tmp$disp_prob<- matrix(0,nrow=tmp$n_bends,ncol=tmp$n_bends)
		# M<-ifelse(basin=="upper", nrow(tmp$disp_prob)-1, nrow(tmp$disp_prob))
		# for(i in 1:M)
		# {
		#   for(j in 1:i)
		#   {
		#     tmp$disp_prob[i,j]<-(1/2)^(i-j+1)
		#   }
		# }
		# if(basin=="upper")
		# {
		#   J<-length(which(tmp$bend_meta$B_SEGMENT==4))
		#   for(j in 1:J)
		#   {
		#     tmp$disp_prob[nrow(tmp$disp_prob),j]<-(1/2)^(J-j+2)
		#     tmp$disp_prob[nrow(tmp$disp_prob),nrow(tmp$disp_prob)]<-0.5
		#   }
		# }
		# tmp$disp_prob<- tmp$disp_prob/apply(tmp$disp_prob,1,sum)

		### ADULTS (MONTHLY)
		#### NON-SPAWNING
		## ORIGINAL:
	  tmp$adult_mov_prob<- matrix(runif(tmp$n_bends*tmp$n_bends,0,0.1),nrow=tmp$n_bends,ncol=tmp$n_bends)
		diag(tmp$adult_mov_prob)<-0.7
		tmp$adult_mov_prob<- tmp$adult_mov_prob/apply(tmp$adult_mov_prob,1,sum)
		if(migration)
		{
		  tmp$p_dwnstrm <-input$spatialInput[[basin]]$p_dwnstrm
		  tmp$p_leave <-ifelse(basin=="upper",
		                       tmp$p_dwnstrm$to+input$spatialInput[[basin]]$p_upper_YR$to,
		                       tmp$p_dwnstrm$to)
		  tmp$adult_mov_prob <- tmp$adult_mov_prob*(1-tmp$p_leave)
		  if(basin=="upper")
		  {
		    tmp$p_upper_YR <-input$spatialInput[[basin]]$p_upper_YR
		    tmp$adult_mov_prob<-cbind(tmp$adult_mov_prob, tmp$p_upper_YR$to)
		  }
		  tmp$adult_mov_prob<- cbind(tmp$adult_mov_prob, tmp$p_dwnstrm$to)
		  if(basin=="upper")
		  {
		    YR_return_dist <- runif(tmp$n_bends)
		    YR_return_dist[tmp$n_bends]<-50
		    YR_return_dist <- YR_return_dist/sum(YR_return_dist)*
		      tmp$p_upper_YR$from*(1-tmp$p_dwnstrm$to)
		    YR_return_dist <- c(YR_return_dist[1:tmp$n_bends],
		                        1-tmp$p_upper_YR$from,
		                        tmp$p_upper_YR$from*tmp$p_dwnstrm$to)
		    tmp$adult_mov_prob<-rbind(tmp$adult_mov_prob, unname(YR_return_dist))
		  }
		  return_upstrm_dist <- runif(tmp$n_bends)
		  if(basin=="upper")
		  {
		    return_upstrm_dist<- return_upstrm_dist/sum(return_upstrm_dist)*
		      (1-tmp$p_upper_YR$to)
		    return_upstrm_dist<-c(return_upstrm_dist, tmp$p_upper_YR$to)
		  }
		  return_upstrm_dist<- return_upstrm_dist/sum(return_upstrm_dist)*
		    tmp$p_dwnstrm$from
		  return_upstrm_dist<- c(return_upstrm_dist, 1-tmp$p_dwnstrm$from)
		  tmp$adult_mov_prob<- rbind(tmp$adult_mov_prob, unname(return_upstrm_dist))
		}
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
		if(migration & basin=="upper" & input$spatialInput[["upper"]]$UYR$spawn)
		{
		  tmp$spn_bends<-c(tmp$spn_bends, tmp$n_bends+1)
		}
		M<-ifelse(migration, tmp$n_bends+length(tmp$outside_bends), tmp$n_bends)
		tmp$spn_mov_prob<- matrix(0,nrow=tmp$n_bends,ncol=M)
		if(is.null(input$spatialInput[[basin]]$spn_bnd_probs))
		{
		  tmp$spn_bnd_probs<-runif(length(tmp$spn_bend))
		  if(migration & basin=="upper" & input$spatialInput[["upper"]]$UYR$spawn)
		  {
		    tmp$spn_bnd_probs[length(tmp$spn_bnd_probs)]<- 
		      tmp$spn_bnd_probs[length(tmp$spn_bnd_probs)]*
		      input$spatialInput[[basin]]$UYR$p_YR_spn_passage
		  }
		  tmp$spn_bnd_probs<-tmp$spn_bnd_probs/sum(tmp$spn_bnd_probs)
		}
		tmp$spn_mov_prob[, tmp$spn_bends]<- rep(tmp$spn_bnd_probs,
		                                        each=nrow(tmp$spn_mov_prob))
		if(migration)
		{
		  if(basin=="upper")
		  {
		    UYR_spn<-c(tmp$spn_mov_prob[1,1:tmp$n_bends]/
		                 sum(tmp$spn_mov_prob[1,1:tmp$n_bends])*
		                 input$spatialInput[[basin]]$UYR$p_spn_mig, 
		               1-input$spatialInput[[basin]]$UYR$p_spn_mig, 0)
		    tmp$spn_mov_prob<- rbind(tmp$spn_mov_prob, UYR_spn)
		  }
		  dwnstrm_spn<- c(tmp$spn_mov_prob[1,1:nrow(tmp$spn_mov_prob)]/
		                    sum(tmp$spn_mov_prob[1,1:nrow(tmp$spn_mov_prob)])*
		                    input$spatialInput[[basin]]$p_spn_mig, 
		                  1-input$spatialInput[[basin]]$p_spn_mig)
		  tmp$spn_mov_prob<- rbind(tmp$spn_mov_prob, dwnstrm_spn)
		}
		# TO ADD NOISE ADD OTHER INPUTS EARLIER AND UNCOMMENT:
		#tmp$spn_mov_prob<- tmp$spn_mov_prob/apply(tmp$spn_mov_prob,1,sum)
		
		## SPATIAL STRUCTURE AGE-0
		if(length(tmp$spn_bends)>1)
		{
		  tmp$natural_age0_rel_dens<- 
		    colSums(tmp$drift_prob[tmp$spn_bends,]*tmp$spn_bnd_probs)
		}
		if(length(tmp$spn_bends)==1)
		{
		  tmp$natural_age0_rel_dens<- tmp$drift_prob[tmp$spn_bends,]
		}
		if(!migration)
		{
		  tmp$natural_age0_rel_dens<- tmp$natural_age0_rel_dens[1:tmp$n_bends]
		}
		pp<- runif(tmp$n_bends)
		tmp$hatchery_age0_rel_dens<- pp/sum(pp)

		## SPATIAL STRUCTURE AGE-1+
		pp<- runif(tmp$n_bends)
		tmp$natural_age1plus_rel_dens<- pp/sum(pp)
		pp<- runif(tmp$n_bends)
		tmp$hatchery_age1plus_rel_dens<- pp/sum(pp)		 
	}## END SPATIAL
	
	
	if(!spatial & migration)
	{
	  tmp$p_retained<-input$spatialInput[[basin]]$p_retained
	  p_migrate_out<-input$spatialInput[[basin]]$p_dwnstrm$to
	  p_migrate_in<- input$spatialInput[[basin]]$p_dwnstrm$from
	  tmp$migrate<-matrix(c(1-p_migrate_out, p_migrate_out,
	                        p_migrate_in, 1-p_migrate_in),
	                      nrow=2, ncol=2, byrow = TRUE)
	  if(basin=="upper")
	  {
	    to_UYR<-input$spatialInput[[basin]]$p_upper_YR$to
	    from_UYR<-input$spatialInput[[basin]]$p_upper_YR$from
	    tmp$migrate<-matrix(c(tmp$migrate[1,1]-to_UYR, 
	                          to_UYR, tmp$migrate[1,2],
	                          from_UYR*(1-tmp$migrate[1,2]),
	                          1-from_UYR, from_UYR*tmp$migrate[1,2],
	                          tmp$migrate[2,1]*(1-to_UYR),
	                          tmp$migrate[2,1]*to_UYR, tmp$migrate[2,2]),
	                        nrow=3, ncol=3, byrow = TRUE)
	    tmp$passage<- input$spatialInput[[basin]]$UYR$p_passage
	    tmp$drift_in<- tmp$passage*
	      input$spatialInput[[basin]]$UYR$p_retained_given_passage
	    tmp$p_retained<- c(tmp$p_retained, 
	                       input$spatialInput[[basin]]$LYR$p_retained)
	  }
	  tmp$p_retained<-mean(tmp$p_retained)
	}
	
	# TOTAL TRACKED POPULATION NUMBERS
	if(basin=="upper")
	{
	  tmp$total_natural<-ifelse(migration,
	                            tmp$natural+tmp$LS_natural+tmp$UYR_natural,
	                            tmp$natural)
	  tmp$total_hatchery<-ifelse(migration,
	                             tmp$hatchery+tmp$LS_hatchery+tmp$UYR_hatchery,
	                             tmp$hatchery)
	}
	if(basin=="lower")
	{
	  tmp$total_natural<-ifelse(migration,
	                            tmp$natural+tmp$MS_natural,
	                            tmp$natural)
	  tmp$total_hatchery<-ifelse(migration,
	                             tmp$hatchery+tmp$MS_hatchery,
	                             tmp$hatchery)
	}
	
	
	# STOCKING & GENETICS INPUTS
	### FINGERGLINGS
	tmp$fingerling<- input$stockingInput[[basin]]$fingerling
	### YEARLINGS
	tmp$yearling<- input$stockingInput[[basin]]$yearling
	if(spatial)
	{
	  tmp$bend_meta$upper_rkm<-tmp$bend_meta$LOWER_RIVER_MILE[1]*1.60934+cumsum(tmp$bend_meta$Length.RKM)
	  if(basin=="upper")
	  {
	    tmp$bend_meta$upper_rkm[nrow(tmp$bend_meta)]<-tmp$bend_meta$Length.RKM[nrow(tmp$bend_meta)]
	  }
	  tmp$fingerling$bend<-unlist(lapply(1:nrow(tmp$fingerling),function(i)
	  {
	    return(min(which(tmp$bend_meta$upper_rkm>=tmp$fingerling$stocking_rkm[i] & 
	                       tmp$bend_meta$RIVER=="MO")))
	  }))
	  tmp$yearling$bend<-unlist(lapply(1:nrow(tmp$yearling),function(i)
	  {
	    return(min(which(tmp$bend_meta$upper>=tmp$yearling$stocking_rkm[i] & 
	                       tmp$bend_meta$RIVER=="MO")))
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
	                        stocking_no=sum(stocking_no),
	                        phi0_mn=ifelse(sum(stocking_no)!=0,
	                                       sum(phi0_mn*stocking_no)/sum(stocking_no),
	                                       mean(phi0_mn)),
	                        phi0_sd=max(phi0_sd))
	  tmp$yearling<-ddply(tmp$yearling, .(month), summarize,
	                      age=mean(age),
	                      length_mn=ifelse(sum(stocking_no)!=0,
	                                       sum(length_mn*stocking_no)/sum(stocking_no),
	                                       mean(length_mn)),
	                      length_sd=max(length_sd),
	                      stocking_no=sum(stocking_no),
	                      phi_mn=ifelse(sum(stocking_no)!=0,
	                                    sum(phi_mn*stocking_no)/sum(stocking_no),
	                                    mean(phi_mn)),
	                      phi_sd=max(phi_sd))
	}
	## BROODSTOCK
	tmp$broodstock<- input$stockingInput[[basin]]$broodstock
	if(genetics)
	{
	  ### CURRENT FISH IN HATCHERY
	  if(basin=="lower")
	  {
	    #### PULL FINGERLING FAMILY LOTS STOCKED DURING THE PAST YEAR
	    tmp$broodstock$BROOD_1<-
	      rbind(tmp$hatchery_age0,tmp$stockingHistory[
	        which(tmp$stockingHistory$year==tmp$startYear-1 
	              & tmp$stockingHistory$current_age+9<24),
	        names(tmp$hatchery_age0)])
	    #### ADD IN KNOWN FAMILY LOTS FROM THE PAST YEAR (2018) AND NUMBER 
	    #### OF EGGS INCLUDING ANY MORTALITY INDICATED
	    tmp2<- data.frame(mother=c("46263A6E1B", "462704502D", "462704502D",
	                               "4627693763",
	                               "4627545945", "4627545945", "4626641923",
	                               "4626641923", "4627201358", "46270E6C3C",
	                               "46270E6C3C", "4626711111", "4626711111"),
	                      father=c("4625761D21", "4716281E21", "4715591B05",
	                               "47187B4132",
	                               "6C00107471", "46276D476B", "462711443D",
	                               "F7F39", "48683A3B7D", "47041F697D", 
	                               "D6C1E", "4626773563", "462457120D"),
	                      hatchery=rep("GAVINS POINT", 13),
	                      no_offspring=c(37400, 0.5*25200, 0.5*25200, 1500, 
	                                     28860, 7280, 18560, 6032, 19458, 
	                                     17064, 7344, 6244, 6021))
	    if(nrow(tmp$broodstock$BROOD_1)!=0)
	    {
	      tmp$broodstock$BROOD_1<-
	        merge(tmp$broodstock$BROOD_1[,c("mother", "father", 
	                                        "hatchery", "number")],
	                                  tmp2,by=c("mother", "father", 
	                                            "hatchery"), 
	                                  all=TRUE)
	    }
	    if(nrow(tmp$broodstock$BROOD_1)==0){tmp$broodstock$BROOD_1<-tmp2}
	    tmp$broodstock$BROOD_1[is.na(tmp$broodstock$BROOD_1$number),]$number<-0
	    rm(tmp2)
	    # 2018 LB STOCKING REPORT INCLUDES FEMALE 4715674971 W/ 3,634  
	    # AGE-0's IN TABLE BUT THERE IS NO CORRESPONDING MALE PROGENY IN  
	    # MALE TABLE; SINCE ALSO NOT IN STOCKING DATABASE EXCLUDED (I.E.,  
	    # NOT ADDED) STOCKING DATABASE HAS FEMALE 4627545945 FOR 8 AGE-0's 
	    # WITH MALE 46271B725E, BUT NO CORRESPONDING DATA EXISTS ON THESE 
	    # IN THE STOCKING REPORT; SINCE NO EGG COUNTS AVAILABLE FOR THIS 
	    # CROSS EXCLUDED
	    tmp$broodstock$BROOD_1<- 
	      tmp$broodstock$BROOD_1[-which(tmp$broodstock$BROOD_1$father=="46271B725E"),]
	  }
	  if(basin=="upper")
	  {
	    ### PULL FINGERLING FAMILY LOTS STOCKED THIS YEAR
	    tmp$broodstock$BROOD_1<-
	      rbind(tmp$hatchery_age0,
	            tmp$stockingHistory[
	              which(tmp$stockingHistory$year==tmp$startYear-1 
	                    & tmp$stockingHistory$current_age+9<24),
	              names(tmp$hatchery_age0)])
	    #### NOTE: IF WE HAVE DATA ON NUMBER OF EMBRYOS PRODUCED THIS WOULD 
	    #### BE USEFUL HERE
	    tmp$broodstock$BROOD_1<- tmp$broodstock$BROOD_1[, c("mother", "father", "hatchery", "number")]
	    #### RANDOMLY DRAW FEMALE LENGTHS FROM INVERSE DISTRIBUTION
	    tmp$broodstock$BROOD_1$length<- 
	      len_ini_upp_natural_nospace(runif(nrow(tmp$broodstock$BROOD_1)))
	    #### DRAW FECUNDITIES FOR EACH FEMALE BASED ON LENGTH
	    fl_normalized<- (tmp$broodstock$BROOD_1$length - 1260.167)/277.404
	    #SAME AS FECUNDITY FUNCTION
	    loglambda<- rnorm(length(fl_normalized),
	                      tmp$fec_a+tmp$fec_b*fl_normalized,
	                      tmp$fec_er)
	    tmp$broodstock$BROOD_1$no_offspring<- rpois(nrow(tmp$broodstock$BROOD_1),
	                                                exp(loglambda))
	    tmp$broodstock$BROOD_1$no_offspring<- 
	      ifelse(tmp$broodstock$BROOD_1$no_offspring>=tmp$broodstock$BROOD_1$number,
	             tmp$broodstock$BROOD_1$no_offspring,
	             tmp$broodstock$BROOD_1$number)
	    rm(fl_normalized, loglambda)
	  }
	  # NOTE: WE CAN SKIP THE NEXT TWO STEPS  IF WE HAVE REMAINING NUMBERS
	  tmp$broodstock$BROOD_1$hatchery_survival<- 
	    plogis(rnorm(nrow(tmp$broodstock$BROOD_1), 
	                 log(tmp$phi0_Hcap_mean/(1-tmp$phi0_Hcap_mean)),
	                 tmp$phi0_Hcap_er))
	  if(nrow(tmp$broodstock$BROOD_1)>0)
	  {
	    tmp$broodstock$BROOD_1$remaining <-
	      max(rbinom(1, tmp$broodstock$BROOD_1$no_offspring, 
	                 tmp$broodstock$BROOD_1$hatchery_survival),
	          tmp$broodstock$BROOD_1$number)-tmp$broodstock$BROOD_1$number
	  }
	  if(nrow(tmp$broodstock$BROOD_1)==0)
	  {
	    tmp$broodstock$BROOD_1$remaining<-tmp$broodstock$BROOD_1$number
	  }
	  tmp$broodstock$BROOD_1<-tmp$broodstock$BROOD_1[,c("mother", "father", 
	                                                    "hatchery", "remaining")]
	  ## NOTE: AGE IS NOT INCLUDED AS WE HAVE ASSUMED ALL YEARLINGS WILL  
	  ## BE 15 MONTHS OLD IN SEPTEMBER, BUT THIS WILL NOT ALWAYS BE THE   
	  ## CASE GIVEN THE DATA WE HAVE; ALSO STOCKING BY BASIN AND ORIGIN  
	  ## SHOWS STOCKING OCCURS IN VARIOUS OTHER MONTHS... MAY NEED TO 
	  ## REVISE TO ACCOUNT FOR
	}
	
	# GENETICS INPUTS
	#if(genetics)
	#{
	#  tmp$genetics_info$fingerling<-input$geneticsInput[[basin]]$fingerling
	#  tmp$genetics_info$yearling<-input$geneticsInput[[basin]]$yearling
	# ## HATCHERY FISH INPUTS W/ GENETICS
	#   tmp$hatchery_info<- rbind(input$geneticsInput[[basin]]$age1plus, 
	#                             input$geneticsInput[[basin]]$age0)
	#  
	#}
	# HATCHERY FISH INPUTS W/O GENETICS
	# if(!genetics & hatchery_name)
	# {
	#   tmp$hatchery_info<-aggregate(number~hatchery+age,
	#                                input$stockingHistory[[basin]], 
	#                                sum)
	#   # tmp$hatchery_info<-aggregate(no_stocked~hatchery+age,
	#   #                              input$geneticsInput[[basin]]$age1plus, 
	#   #                              sum) 
	#   # tmp2<-aggregate(no_stocked~hatchery+age,
	#   #                 input$geneticsInput[[basin]]$age0,
	#   #                 sum) 
	#   # tmp$hatchery_info<-rbind(tmp$hatchery_info, tmp2)
	#   # rm(tmp2)
	# }
	
	return(tmp)
}
