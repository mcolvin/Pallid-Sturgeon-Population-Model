

### CORE FUNCTION TO DO SIMULATIONS	
sim<- function(inputs=NULL, dyn=NULL,
               recruitmentFreq=1,
               sizeStructure=FALSE, 
               demographicOnly=FALSE,
               weightCalc=TRUE)
	{
	inputs$recruitmentFreq<-recruitmentFreq
	  # INSTEAD OF RECRUITMENT FREQUENCY, PERHAPS WE WANT RECRUITMENT FAILURE
	  # EVEN MORE SPECIFICALLY, WE MAY WANT SPAWNING FAILURE AND SURVIVAL 
	  # FAILURE:  IN THE CASE OF SPAWNING FAILURE, SPN<-0 FOR ALL FISH AND
	  # MPS FOR MATURE FISH WOULD INCREASE.  DOES THIS MAKE SENSE?  IF A FISH
	  # PRODUCES EGGS, WILL SHE  ALWAYS SPAWN OR CAN SHE REABSORB?  IF THE 
	  # LATTER CAN TAKE PLACE HOW DOES THIS EFFECT THE SPAWNING PROBABILITY 
	  # FOR THE FOLLOWING YEAR?
    # IN THE CASE OF SURVIVAL FAILURE PHI0<- 0 FOR THAT TIME STEP
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
	yps[1,]<-p
	for(j in 1:(ncol(yps)-1)){yps[j+1,j]<-1-plogis(-5+2.55*j)}
	#### FLATE RATE
	sss<-Re(eigen(yps)$vectors[,1])/sum(Re(eigen(yps)$vectors[,1]))
	for(j in 1:length(sss)){if(abs(sss[j])<10^-24){sss[j]<-0}}
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
	  inputs$pr_embryo*inputs$phi_embryo*inputs$phi_free_embryo*
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
	  
	  EGGS_H<-dyn$EGGS_H 
	  EGGS_N<-dyn$EGGS_N 
	  
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
	      ## THIS CAN BE CALCULATED FROM OTHER STORED ITEMS
	    
	    BIOMASS_N<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	    BIOMASS_H<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	    BIOMASS<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	      ## THIS CAN BE CALCULATED FROM BIOMASS_N AND BIOMASS_H
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
	    
	    #[1] UPDATE SURVIVAL
	    Z_H[indx_H]<- dSurvival(phi_age=inputs$phi,
	                            age=AGE_H[indx_H],
	                            maxAge=inputs$maxage)
	    Z_N[indx_N]<- dSurvival(phi_age=inputs$phi,
	                            age=AGE_N[indx_N],
	                            maxAge=inputs$maxage)
	    
	    
	    #[6] UPDATE AGE IN MONTHS (AND ZERO OUT DEAD FISH)
	    AGE_H[indx_H]<- (AGE_H[indx_H]+1)*Z_H[indx_H]
	    AGE_N[indx_N]<- (AGE_N[indx_N]+1)*Z_N[indx_N]
	    
	    
	    #[3] UPDATE TOTAL LENGTH (AND ZERO OUT DEAD FISH) 
	    LEN_H[indx_H]<-dLength(k=k_H[indx_H],
	                           linf=Linf_H[indx_H],
	                           dT=1/12,
	                           length1=LEN_H[indx_H])*Z_H[indx_H]
	    LEN_N[indx_N]<-dLength(k=k_N[indx_N],
	                           linf=Linf_N[indx_N],
	                           dT=1/12,
	                           length1=LEN_N[indx_N])*Z_N[indx_N]	
	
	    ### RECRUITMENT AND SPAWNING
	    if(inputs$recruitmentFreq>0 & m[i]==6)
	    {
	      ### ZERO OUT FISH THAT DIED
	      MAT_H<-MAT_H*Z_H 
	      MAT_N<-MAT_N*Z_N
	      
	      MPS_H<-MPS_H*Z_H 
	      MPS_N<-MPS_N*Z_N 
	      
	      SPN_H<-SPN_H*Z_H
	      SPN_N<-SPN_N*Z_N
	      
	      EGGS_H<-EGGS_H*Z_H
	      EGGS_N<-EGGS_N*Z_N
	      
	      ### AGE-1 RECRUITMENT
	      #### SURVIVAL FROM AGE-0 TO AGE-1    
	      AGE_0_N_BND[]<- rbinom(inputs$nreps,
	                             AGE_0_N_BND,
	                             inputs$phi0)
	      AGE_0_H_BND[]<- rbinom(inputs$nreps,
	                             AGE_0_H_BND,
	                             inputs$phi0) #SHOULD phi0_H DIFFER FROM phi0_N????
        #### ADD SURVIVING FISH TO POPULATION
	      ##### HATCHERY STOCKED FISH
	      if(sum(AGE_0_H_BND)>0)
	      {
	        # INDEX OF OPEN SLOTS
	        indxr<- unlist(sapply(1:inputs$nreps,function(x)
	        {
	          which(Z_H[,x]==0)[1:sum(AGE_0_H_BND[,x])]
	        }))		
	        indxr<- cbind(c(indxr),sort(rep(1:inputs$nreps,colSums(AGE_0_H_BND))))
	        # ADD NEW 1 YEAR OLD RECRUITS
	        Z_H[indxr]<-1 
	        # ADD AGE OF RECRUITS
	        AGE_H[indxr]<-12 	
	        # UPDATE GROWTH COEFFICIENTS
	        tmp<- ini_growth(n=sum(AGE_0_H_BND),
	                         mu_ln_Linf=inputs$ln_Linf_mu,
	                         mu_ln_k=inputs$ln_k_mu,
	                         vcv=inputs$vcv,
	                         maxLinf=inputs$maxLinf) 
	        Linf_H[indxr]<-tmp$linf
	        k_H[indxr]<-tmp$k
	        # ADD  INITIAL LENGTH OF RECRUITS
	        ## METHOD 1: RECRUIT DISTRIBUTION
	        LEN_H[indxr]<-rnorm(length(indxr[,1]),
	                                inputs$recruit_mean_length,
	                                inputs$recruit_length_sd)				
	        ### METHOD 2: LENGTH FROM AGE AND VB GROWTH 
	        #LEN_H[indxr]<-dLength(k=k_H[indxr], 
	        #                           linf=Linf_H[indxr],
	        #                           length1=7,
	        #                           dT=1)
	        # ASSIGN SEX
	        SEX_H[indxr]<-ini_sex(n=length(indxr[,1]),
	                              prob_F=inputs$sexratio)
	        # ASSIGN LOCATION OF RECRUITS
	        if(inputs$spatial==TRUE)
	        {
	          RKM[indxr]<- bend2rkm(c(unlist(sapply(1:inputs$nreps,
	                                                    function(x){rep(1:inputs$n_bends,AGE_0_H_BND[,x])}))))
	        }	
	      }
	      ##### NATURALLY SPAWNED FISH
	      if(sum(AGE_0_N_BND)>0)
	      {
	        #if(min((nrow(dyn$Z)-colSums(dyn$Z))-dyn$AGE_0_N_BND)<0) {do not add}# NUMBER OF SLOTS OPEN
	        # INDEX OF OPEN SLOTS
	        indxr<- unlist(sapply(1:inputs$nreps,function(x)
	        {
	          if(sum(AGE_0_N_BND[,x])>0)
	          {
	            which(Z_N[,x]==0)[1:sum(AGE_0_N_BND[,x])]
	          }
	        }))		
	        indxr<- cbind(c(indxr),sort(rep(1:inputs$nreps,colSums(AGE_0_N_BND))))
	        # ADD NEW 1 YEAR OLD RECRUITS
	        Z_N[indxr]<-1    
	        # ADD AGE OF RECRUITS
	        AGE_N[indxr]<-12 	
	        # UPDATE GROWTH COEFFICIENTS
	        tmp<- ini_growth(n=sum(AGE_0_N_BND),
	                         mu_ln_Linf=inputs$ln_Linf_mu,
	                         mu_ln_k=inputs$ln_k_mu,
	                         vcv=inputs$vcv,
	                         maxLinf=inputs$maxLinf) 
	        Linf_N[indxr]<-tmp$linf
	        k_N[indxr]<-tmp$k
	        # ADD  INITIAL LENGTH OF RECRUITS
	        ## METHOD 1: RECRUIT LENGTH DISTRIBUTION
	        LEN_N[indxr]<-rnorm(length(indxr[,1]),
	                                inputs$recruit_mean_length,
	                                inputs$recruit_length_sd)				
	        ## METHOD 2: LENGTH FROM AGE AND VB GROWTH 
	        #LEN_N[indxr]<-dLength(k=k_N[indxr], 
	        #                         linf=Linf_N[indxr],
	        #                          length1=7,
	        #                           dT=1)
	        # ASSIGN SEX
	        SEX_N[indxr]<-ini_sex(n=length(indxr[,1]),
	                              prob_F=inputs$sexratio)
	        # ASSIGN LOCATION OF RECRUITS
	        if(inputs$spatial==TRUE)
	        {
	          RKM[indxr]<- bend2rkm(c(unlist(sapply(1:inputs$nreps,
	                                                function(x){rep(1:inputs$n_bends,AGE_0_N_BND[,x])}))))
	        }		
	      }
	      #### ZERO OUT AGE-0's AFTER THEY MOVE TO AGE-1
	      AGE_0_N_BND[]<-0 
	      AGE_0_H_BND[]<-0	
	      
	      ### SPAWNING AND NATURAL RECRUITMENT TO AGE-0
	      # NOTE: SOME AGE-1's MAY BE INCLUDED IN INDX, BUT MAT, MPS,
	      #       SPN, AND EGGS SHOULD START OFF ZERO AND STAY ZERO
	      #### UPDATE MATURITY OF LIVE FISH GIVEN AGE AND LAST YEAR'S MATURITY
	      tmp_H<-dMaturity(mature = MAT_H[indx_H],
	                       age = AGE_H[indx_H],
	                       live = Z_H[indx_H],
	                       mat_dist = inputs$mat_dist)
	      tmp_N<-dMaturity(mature = MAT_N[indx_N],
	                       age = AGE_N[indx_N],
	                       live = Z_N[indx_N],
	                       mat_dist = inputs$mat_dist)
	      
	      MAT_H[indx_H] <- tmp_H$mature
	      MAT_N[indx_N] <- tmp_N$mature
	      
	      ### UPDATE MONTHS SINCE SPAWNING
	      MPS_H[indx_H] <- dMPS(mps = MPS_H[indx_H],
	                            spawn = SPN_H[indx_H],
	                            mature = MAT_H[indx_H])
	      MPS_N[indx_N] <- dMPS(mps = MPS_N[indx_N],
	                            spawn = SPN_N[indx_N],
	                            mature = MAT_N[indx_N])
	      
	      ### UPDATE SPAWNING [NO|YES]
	      ### GIVEN SEXUAL MATURITY AND 
	      ### TIME SINCE LAST SPAWNING EVENT
	      SPN_H[indx_H] <- spawn(mps = MPS_H[indx_H],
	                             a=-5,b=2.55,
	                             mature = MAT_H[indx_H],
	                             FirstSpawn = tmp_H$FirstSpawn)
	      SPN_N[indx_N] <- spawn(mps = MPS_N[indx_N],
	                             a=-5,b=2.55,
	                             mature = MAT_N[indx_N],
	                             FirstSpawn = tmp_N$FirstSpawn)
	      
	      ### UPDATE THE NUMBER OF EGGS IN A FEMALE 
	      ### GIVEN SEX AND SPAWNING STATUS
	      EGGS_H[indx_H]<-fecundity(fl=LEN_H[indx_H],
	                                a=inputs$fec_a,
	                                b=inputs$fec_b,
	                                er=inputs$fec_er,
	                                sex=SEX_H[indx_H],
	                                spawn=SPN_H[indx_H],
	                                mature=MAT_H[indx_H])	
	      EGGS_N[indx_N]<-fecundity(fl=LEN_N[indx_N],
	                                a=inputs$fec_a,
	                                b=inputs$fec_b,
	                                er=inputs$fec_er,
	                                sex=SEX_N[indx_N],
	                                spawn=SPN_N[indx_N],
	                                mature=MAT_N[indx_N])
	     
	      ## NUMBER OF EGGS IN EACH BEND
	      AGE_0_N_BND<- matrix(colSums(EGGS_N)+colSums(EGGS_H),nrow=1) 
	      
	      
	      ### eggs --> embryos	
	      ### STATUS: DONE NEEDS TO BE MODIFIED WITH DENSITY
	      AGE_0_N_BND[]<- rbinom(inputs$nreps,
	                           AGE_0_N_BND,
	                           inputs$pr_embryo) 
	      
	      ### embryos --> free embryos
	      AGE_0_N_BND[]<- rbinom(inputs$nreps,
	                            AGE_0_N_BND,
	                            inputs$phi_embryo)
	      ### free embryos --> age0
	      AGE_0_N_BND[]<- rbinom(inputs$nreps,
	                            AGE_0_N_BND,
	                            inputs$phi_free_embryo) 
	      
	      
	      ### ADJUST FOR THE AGE-0 THAT WERE INTERCEPTED AND RETAINED IN
	      ### THE BASIN
	      if(!is.null(inputs$p_retained))
	      {
	        AGE_0_N_BND[]<- rbinom(inputs$nreps,
	                              AGE_0_N_BND,
	                              inputs$p_retained)           
	      }
	    }
	    
	    ### UPDATE INDICES FOR FISH THAT ARE ALIVE; INCLUDING RECRUITS
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
	    
	    
	    #[4] UPDATE WEIGHT GIVEN LENGTH
	    ### STATUS: DONE BUT SLOW
	    if(weightCalc)
	    {
	      ### ZERO OUT FISH THAT DIED
	      WGT_H<- WGT_H*Z_H
	      WGT_N<- WGT_N*Z_N
	      
	      ### UPDATE LIVING FISH
	      WGT_H[indx_H]<-dWeight(len=LEN_H[indx_H],
	                             a=inputs$a,
	                             b=inputs$b,
	                             er=inputs$lw_er)
	      WGT_N[indx_N]<-dWeight(len=LEN_N[indx_N],
	                             a=inputs$a,
	                             b=inputs$b,
	                             er=inputs$lw_er)
	    }

	    
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
	      BIOMASS[i,]<- (colSums(WGT_H)+colSums(WGT_N))/1000 
	        ## THIS CAN BE CALCULATED FROM BIOMASS_N AND BIOMASS_H
	    
	      ### MEAN WEIGHT IN KILOGRAMS
	      MEANWEIGHT_N[i,]<- (colSums(WGT_N)/colSums(Z_N))/1000 
	      MEANWEIGHT_H[i,]<- (colSums(WGT_H)/colSums(Z_H))/1000 
	      MEANWEIGHT[i,]<-((colSums(WGT_H)+colSums(WGT_N))/(colSums(Z_H)+colSums(Z_N)))/1000 
	      ## THIS CAN BE CALCULATED FROM MEANWEIGHT_N, MEANWEIGHT_H, 
	      ##    N_NAT, AND N_HAT:
	      ## (MEANWEIGHT_N*N_NAT+MEANWEIGHT_H*N_HAT)/(N_NAT+N_HAT) 
	    }
	    
	    ### MEAN LENGTH IN MM
	    MEANLENGTH_N[i,]<- colSums(LEN_N)/colSums(Z_N)	
	    MEANLENGTH_H[i,]<- colSums(LEN_H)/colSums(Z_H)
	    MEANLENGTH[i,]<-(colSums(LEN_H)+ colSums(LEN_N))/(colSums(Z_H)+colSums(Z_N))
	    ## THIS CAN BE CALCULATED FROM MEANLENGTH_N, MEANLENGTH_H, 
	    ##    N_NAT, AND N_HAT:
	    ## (MEANLENGTH_N*N_NAT+MEANLENGTH_H*N_HAT)/(N_NAT+N_HAT) 
	    
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
