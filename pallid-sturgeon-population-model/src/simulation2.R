

### CORE FUNCTION TO DO SIMULATIONS	
sim<- function(inputs=NULL, dyn=NULL,
               recruitmentFreq=0,
               sizeStructure=FALSE, 
               demographicOnly=FALSE,
               weightCalc=TRUE)
	{
	inputs$recruitmentFreq<-recruitmentFreq
	inputs$sizeStructure<-sizeStructure
	inputs$demographicOnly<-demographicOnly
	inputs$weightCalc<-weightCalc

	# BUILD LESLIE MATRIX
	A<-matrix(0,inputs$maxage,inputs$maxage)
	## SURVIVAL RATES
	A[cbind(2:inputs$maxage,1:(inputs$maxage-1))]<-inputs$phi[1:(length(inputs$phi)-1)]
	## FECUNDITIES
	### SPAWNING PROBABILITY
	yps<-matrix(0, inputs$maxage-inputs$age_mat_50, inputs$maxage-inputs$age_mat_50)
	p<-plogis(-5+2.55*1:nrow(yps))
	for(j in 1:ncol(yps)){yps[1,j]<-plogis(-5+2.55*j)}
	for(j in 1:(ncol(yps)-1)){yps[j+1,j]<-1-plogis(-5+2.55*j)}
	#### FLATE RATE
	sss<-Re(eigen(yps)$vectors[,1])/sum(Re(eigen(yps)$vectors[,1]))
	spawnFreq<-sum(sss*p)
	# #SAME AS:
	# np<-rep(1,length(p))
	# y<-1:length(p)
	# for(n in 2:length(np)){np[n]<-prod(1-plogis(-5+2.55*y[1:n-1]))}
	# exY<-sum(y*p*np)
	# spawnFreq<-1/exY
	#### BY AGE CLASS
	mat_yrs<-inputs$age_mat_min:inputs$maxage
	#first_spawn_dist<-c(sss,0) #IF USED THEN EQUIVALENT TO FLAT RATE
	first_spawn_dist<-rep(0, length(mat_yrs))
	first_spawn_dist[1:5]<-c(0.1,0.5,0.2,0.1,0.1)
	#PLACEHOLDER; COULD ALSO BUILD FROM MATURATION FUNCTION & 
	#FIRST SPAWN INFO
	PropSpawn<-matrix(0,length(mat_yrs)-1,length(mat_yrs))
	PropSpawn[1,1]<-first_spawn_dist[1]
	for(i in 2:length(mat_yrs)){PropSpawn[,i]<-
	  c(first_spawn_dist[i], rep(0,length(mat_yrs)-2))+yps%*%PropSpawn[,i-1]}
	PropSpawn<-PropSpawn[1,]
	### FECUNDITY
	# IF WE HAVE AN EXPECTED LENGTH DISTRIBUTION FOR ADULTS, IN 
	# PARTICULAR ONE FOR EACH AGE CLASS, THEN WE CAN CALCULATE THE 
	# EXPECTED NUMBER OF EGGS USING THE FECUNDITY FUNCTION.  
	# TRYING TO CALCULATE THIS EXPLICITLY GIVEN THE VB PARAM BIVARIATE 
	# DIST, AGE, AND SIZE AT HATCH...WORK IN PROGRESS...FOR NOW ASSUME 
	# ALL AGE CLASSES THE SAME WITH TRUNCATED NORMAL DISTRIBUTION:
	#### BY AGE CLASS
	if(inputs$basin=="upper")
	{
	  L_min<- (300 - 1260.167)/277.404 #NORMALIZED MIN OF ALK FOR FISH AGE 8+; CALCULATE MIN BASED ON BIVARIATE?
	  L_max<- (1200 - 1260.167)/277.404 #PLACEHOLDER; NEED TO CALCULATE AND USE MAX OF LINFS
	}
	if(inputs$basin=="lower")
	{
	  L_min<- (400 - 1260.167)/277.404 #NORMALIZED MIN OF ALK FOR FISH AGE 8+; CALCULATE MIN BASED ON BIVARIATE?
	  L_max<- (1200 - 1260.167)/277.404 #PLACEHOLDER; NEED TO CALCULATE AND USE MAX OF LINFS
	}
	L_mu<-(750- 1260.167)/277.404#inputs$L_mu
	L_sig<- 200/277.404#inputs$L_sig
	C1<-exp((L_sig^2*inputs$fec_b^2+2*inputs$fec_b*L_mu+2*inputs$fec_a+
	           inputs$fec_er^2)/2)
	C2<-pnorm(L_max, L_mu, L_sig)-pnorm(L_min, L_mu, L_sig)
	fec<-C1/C2*(pnorm((L_max-L_sig^2*inputs$fec_b-L_mu)/L_sig)-
	              pnorm((L_min-L_sig^2*inputs$fec_b-L_mu)/L_sig))
	#NOTE THIS FECUNDITY ALIGNS WITH FECUNDITY FUNCTION BUT GIVES A 
	#HIGHER VALUE THAN THE AVERAGE EGGS INPUT FOR THE UB DRIFT 
	#ANALYSES--MAY NEED TO ADJUST INPUT FECUNDITY VALUES IF THIS IS 
	#NOT DESIRED
	#### BY AGE CLASS
	#TO DO
	A[1,inputs$age_mat_50:inputs$maxage] <- inputs$sexratio*PropSpawn*fec*
	  inputs$phi0
	## RETURN LEADING EIGENPAIR
	lambda<-as.numeric(eigen(A)$values[1])
	sad<-Re(eigen(A)$vectors[,1])
	sad<-sad/sum(sad)
	rm(A, yps, p, sss, spawnFreq, mat_yrs, first_spawn_dist, PropSpawn,
	   L_min, L_max, L_mu, L_sig, C1, C2, fec)
	if(!demographicOnly)
	{
	  ## ASSIGN OBJECTS FROM DYN INPUT
	  k_H<-dyn$k_H
	  k_N<-dyn$k_N
	  
	  Linf_H<-dyn$Linf_H
	  Linf_N<-dyn$Linf_N
	  
	  LEN_H<-dyn$LEN_H
	  LEN_N<-dyn$LEN_N
	  
	  if(weightCalc)
	  {
	    WGT_H<-dyn$WGT_H
	    WGT_N<-dyn$WGT_N
	  }
	  
	  Z_H<-dyn$Z_H
	  Z_N<-dyn$Z_N
	  
	  AGE_H<-dyn$AGE_H
	  AGE_N<-dyn$AGE_N
	  
	  MAT_H<-dyn$MAT_H 
	  MAT_N<-dyn$MAT_N
	  
	  MPS_H<-dyn$MPS_H 
	  MPS_N<-dyn$MPS_N 
	  
	  SEX_H<-dyn$SEX_H
	  SEX_N<-dyn$SEX_N
	  
	  SPN_H<-dyn$SPN_H
	  SPN_N<-dyn$SPN_N
	  
	  EGGS_H<-dyn$EGGS_H #ALL ZEROS
	  EGGS_N<-dyn$EGGS_N #ALL ZEROS
	  
	  AGE_0_N_BND<-dyn$AGE_0_N_BND
	  AGE_0_H_BND<-dyn$AGE_0_H_BND
	  
	  m<-dyn$m
	  
	  
	  if(inputs$spatial==TRUE)
	  {
	    BEND_N <- dyn$BEND_N
	    BEND_H <- dyn$BEND_H
	  }
	  
	  #rm(list='dyn')	
	  
	  # SET UP SUMMARIES
	  RECRUITS<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	  
	  MEANLENGTH_N<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	  MEANLENGTH_H<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	  MEANLENGTH<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	  
	  if(weightCalc)
	  {
	    MEANWEIGHT_N<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	    MEANWEIGHT_H<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	    MEANWEIGHT<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	    
	    BIOMASS_N<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	    BIOMASS_H<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	    BIOMASS<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	  }
	  
	  N_NAT<- matrix(0,nrow=length(m),ncol=inputs$nreps)
	  N_HAT<- matrix(0,nrow=length(m),ncol=inputs$nreps)
	  
	  
	  if(inputs$sizeStructure==TRUE){ss<- list()}
	  # END SUMMARIES
	  
	  
	  # SET UP INDICES PRIOR TO RUNNING MODEL
	  indx_H<- lapply(1:inputs$nreps,
	                  function(x){which(Z_H[,x]==1)}) # ROW INDICES
	  tmp<- unlist(lapply(1:inputs$nreps,
	                      function(x) rep(x,length(indx_H[[x]])))) # COLUMN INDICES
	  indx_H<- cbind(unlist(indx_H),tmp)#row,column
	  
	  indx_N<- lapply(1:inputs$nreps,
	                  function(x){which(Z_N[,x]==1)}) # ROW INDICES
	  tmp<- unlist(lapply(1:inputs$nreps,
	                      function(x) rep(x,length(indx_N[[x]])))) # COLUMN INDICES
	  indx_N<- cbind(unlist(indx_N),tmp)#row,column
	  
	  
	  # PROGRESS BAR
	  pb<-txtProgressBar(min=1,max=length(m),initial=0,char="*",style=3)
	  
	  
	  
	  
	  
	  # SIMULATE POPULATION DYNAMICS GIVEN INITIAL STATES
	  for(i in 1:length(m)) 
	  {
	    setTxtProgressBar(pb, i)
	    
	    Z_H[indx_H]<- dSurvival(phi_age=inputs$phi,
	                            age=AGE_H[indx_H],
	                            maxAge=inputs$maxage)
	    AGE_H<- (AGE_H+1/12)*Z_H ### UPDATE AGE FOR SURVIVORS	
	      #AGE=0 IF DEAD
	    
	    
	    Z_N[indx_N]<- dSurvival(phi_age=inputs$phi,
	                            age=AGE_N[indx_N],
	                            maxAge=inputs$maxage)
	    AGE_N<- (AGE_N+1/12)*Z_N ### UPDATE AGE FOR SURVIVORS
	    
	    ### ZERO OUT FISH THAT DIED
	    LEN_H<- LEN_H*Z_H
	    LEN_N<- LEN_N*Z_N
	    
	    if(weightCalc)
	    {
	      WGT_H<- WGT_H*Z_H
	      WGT_N<- WGT_N*Z_N
	    }
	    
	    ### ADD IN AGE-1 RECRUITS
	    if(sum(AGE_0_N_BND)>0 & m[i]==6)
	    {
	      indx_0<- lapply(1:inputs$nreps,function(x)
	      {
	        which(Z_N[,x]==0)[1:AGE_0_N_BND[,x]] #OVERWRITES DEAD FISH AND UTILIZES NEW ROWS
	      }) # ROW INDICES
	      tmp<- unlist(lapply(1:inputs$nreps,
	                          function(x) rep(x,length(indx_0[[x]])))) # COLUMN INDICES
	      indx_0<- cbind(unlist(indx_0),tmp)#row,column
	      ## ADD NEW AGE-1 RECRUITS TO THE POPULATION    
	      Z_N[indx_0]<-1    #####FIXME##### NEED TO ADD LENGTH AND SO ON
	      AGE_N[indx_0]<-12  
	      ###ALL ROWS NEED LENGTH, WEIGHT, AND SEX
	      ###NEW ROWS WILL HAVE MAT, MPS, SPN = 0, AS DESIRED
	      ###NEW ROWS WILL HAVE A UNIQUE, RANDOM VB k AND Linf, AS DESIRED
	      ###OVERWRITTEN ROWS NEED NEW VB GROWTH PARAMS AND ZEROED OUT MAT, MPS, AND SPN 
	      print(AGE_0_N_BND)
	      ### WHAT WOULD BE FASTER TO WORK WITH indx_0 FOR ALL CATEGORIES, OR TO KEEP A COUNTER SO
	      ### THAT ONLY NEW ROWS ARE UTILIZED???
	    }
	    ## SO EACH YEAR THE INPUT AMOUNT IS ADDED...PROBABLY WANT TO ADD SOME VARIATION TO THIS!!!
	    
	    ### INDICES FOR FISH THAT ARE ALIVE
	    ### AND SUBJECT TO DEMOGRAPHIC PROCESSES
	    indx_H<- lapply(1:inputs$nreps,
	                    function(x){which(Z_H[,x]==1)}) # ROW INDICES
	    tmp<- unlist(lapply(1:inputs$nreps,
	                        function(x) rep(x,length(indx_H[[x]])))) # COLUMN INDICES
	    indx_H<- cbind(unlist(indx_H),tmp)#row,column
	    
	    indx_N<- lapply(1:inputs$nreps,
	                    function(x){which(Z_N[,x]==1)}) # ROW INDICES
	    tmp<- unlist(lapply(1:inputs$nreps,
	                        function(x) rep(x,length(indx_N[[x]])))) # COLUMN INDICES
	    indx_N<- cbind(unlist(indx_N),tmp)#row,column
	    
	    
	    
	    
	    ### UPDATE TOTAL LENGTH 
	    LEN_H[indx_H]<-dLength(k=k_H[indx_H],
	                           linf=Linf_H[indx_H],
	                           dT=1/12,
	                           length1=LEN_H[indx_H])
	    LEN_N[indx_N]<-dLength(k=k_N[indx_N],
	                           linf=Linf_N[indx_N],
	                           dT=1/12,
	                           length1=LEN_N[indx_N])			
	    
	    
	    
	    ### UPDATE WEIGHT
	    ###	GIVEN NEW LENGTH
	    ### STATUS: DONE BUT SLOW
	    if(weightCalc)
	    {
	      WGT_H[indx_H]<-dWeight(len=LEN_H[indx_H],
	                             a=inputs$a,
	                             b=inputs$b,
	                             er=inputs$lw_er)
	      WGT_N[indx_N]<-dWeight(len=LEN_N[indx_N],
	                             a=inputs$a,
	                             b=inputs$b,
	                             er=inputs$lw_er)
	    }
	    
	    ## RECRUITMENT
	    #month<<-m[i]
	    #source("./src/recruitment-dynamics.R")
	    ### WAIT A SECOND WE JUST HAD RECRUITMENT ABOVE...CHECK ON THIS!!!
	    
	    
	    if(inputs$spatial==TRUE)
	    {	
	      source("./src/spatial-dynamics.R")
	    }
	    if(!(is.null(inputs$stocking)))  #####fixme#####
	    {
	      source("./src/stocking-dynamics.R")
	    }
	    
	    
	    # END POPULATION DYNAMICS
	    
	    
	    ## SUMMARIES 
	    
	    ### ABUNDANCE AGE-1+
	    N_NAT[i,]<-colSums(Z_N) 
	    N_HAT[i,]<-colSums(Z_H) 
	    
	    if(weightCalc)
	    {
	      ### BIOMASS IN KILOGRAMS
	      BIOMASS_N[i,]<- colSums(WGT_N)/1000
	      BIOMASS_H[i,]<- colSums(WGT_H)/1000
	      BIOMASS[i,]<- colSums(WGT_H)+colSums(WGT_N)/1000 
	    
	      ### MEAN WEIGHT IN KILOGRAMS
	      MEANWEIGHT_N[i,]<- (colSums(WGT_N)/colSums(Z_N))/1000 
	      MEANWEIGHT_H[i,]<- (colSums(WGT_H)/colSums(Z_H))/1000 
	      MEANWEIGHT[i,]<-((colSums(WGT_H)+colSums(WGT_N))/(colSums(Z_H)+colSums(Z_N)))/1000 
	    }
	    
	    ### MEAN LENGTH IN MM
	    MEANLENGTH_N[i,]<- colSums(LEN_N)/colSums(Z_N)	
	    MEANLENGTH_H[i,]<- colSums(LEN_H)/colSums(Z_H)
	    MEANLENGTH[i,]<-(colSums(LEN_H)+ colSums(LEN_N))/(colSums(Z_H)+colSums(Z_N))
	    
	    
	    ### AGE-1 RECRUITS; NATURAL ORIGIN
	    indx_R<- lapply(1:inputs$nreps,function(x)
	    {
	      out<- which(AGE_N[,x]>0 & AGE_N[,x]<24 & Z_N[,x]==1)
	    }) 		
	    RECRUITS[i,]<- sapply(1:inputs$nreps,
	                          function(x)
	                          { 
	                            length(indx_R[[x]])
	                          })
	    
	    ### SIZE STRUCTURE
	    if(inputs$sizeStructure==TRUE)
	    {
	      ss[[i]]<-process_size_structure(year=i/(length(m))*inputs$nyears,
	                                      L_N=LEN_N,L_H=LEN_H)
	    }
	    
	  }# end i 
	  
	  
	  out<-list(
	    total_N=N_NAT,
	    total_H=N_HAT,
	    months=m,
	    recruits=RECRUITS,
	    mean_length_n=MEANLENGTH_N,
	    mean_length_h=MEANLENGTH_H,
	    mean_length=MEANLENGTH,
	    post_length=c(LEN_H[LEN_H[,1]>0,1],LEN_N[LEN_N[,1]>0,1]), # LENGTH DISTRIBUTION POST SIMULATION
	    years=inputs$startYear+cumsum(rep(1,length(m))/12),
	    #init_summary=init_summary, ####fixme#####
	    LeslieMatrix=list(lambda=lambda, stable_age_dist=sad),
	    inputs=inputs)
	  if(weightCalc)
	  {
	    out$biomass_n <- BIOMASS_N
	    out$biomass_h <- BIOMASS_H
	    out$biomass <- BIOMASS
	    out$mean_weight_n <- MEANWEIGHT_N
	    out$mean_weight_h <- MEANWEIGHT_H
	    out$mean_weight <- MEANWEIGHT
	    out$post_weight <- c(WGT_H[WGT_H[,1]>0,1],WGT_N[WGT_N[,1]>0,1]) # WEIGHT DISTRIBUTION POST SIMULATION
	  }
	}
	if(demographicOnly)
	{
	  out<-list(lambda=lambda, stable_age_dist=sad)
	}
		
	# COMPILE UP ANNUAL SIZE STRUCTURE TALLIES FOR OUTPUT
	##out$size_structure<- do.call("rbind",ss)
	
	return(out)	
	}
