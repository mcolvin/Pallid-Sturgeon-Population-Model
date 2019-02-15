

### CORE FUNCTION TO DO SIMULATIONS	
sim<- function(inputs=NULL, dyn=NULL,
               recruitmentFreq=1,
               stockingFreq=1,
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
	inputs$stockingFreq<-stockingFreq
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
	if(!inputs$spatial){ret<-inputs$p_retained}
	if(inputs$spatial)
	{
	  ret<-rep(0, length(inputs$bend_lengths))
	  ret[inputs$spn_bends]<-inputs$spn_bnd_probs
	  ret<-sum(ret*inputs$p_retained)
	}
	
	#NOTE THIS FECUNDITY ALIGNS WITH FECUNDITY FUNCTION BUT GIVES A 
	#HIGHER VALUE THAN THE AVERAGE EGGS INPUT FOR THE UB DRIFT 
	#ANALYSES--MAY NEED TO ADJUST INPUT FECUNDITY VALUES IF THIS IS 
	#NOT DESIRED
	#### BY AGE CLASS: TO DO
	A[1,inputs$age_mat_50:inputs$maxage] <- inputs$sexratio*PropSpawn*fec*
	  inputs$pr_embryo*inputs$phi_embryo*inputs$phi_free_embryo*ret*
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
	  
	  
	  if(inputs$spatial)
	  {
	    BEND_N <- dyn$BEND_N
	    BEND_H <- dyn$BEND_H
	    
	    # INITIAL SUMMARIES
	    N_BND_HAT_INI<- aggregate(Z_H[,1], by=list(id=BEND_H[,1]), sum)
	    names(N_BND_HAT_INI)[2]<-"rep_1"
	    for(i in 2:inputs$nreps)
	    {
	      app<- aggregate(Z_H[,i], by=list(id=BEND_H[,i]), sum)
	      names(app)[2]<- paste("rep", i, sep="_")
	      N_BND_HAT_INI<- merge(N_BND_HAT_INI,app, by="id", all=TRUE)
	    }
	    N_BND_HAT_INI<- merge(N_BND_HAT_INI, 
	                          data.frame(id=0:length(inputs$bend_lengths)),
	                          by="id", all=TRUE)
	    N_BND_HAT_INI<- N_BND_HAT_INI[order(N_BND_HAT_INI$id),]
	    N_BND_HAT_INI<- matrix(unname(unlist(c(N_BND_HAT_INI[-1,-1]))), 
	                           nrow=length(inputs$bend_lengths),
	                           ncol=inputs$nreps)
	    N_BND_HAT_INI[is.na(N_BND_HAT_INI)]<-0
	    
	    N_BND_NAT_INI<- aggregate(Z_N[,1], by=list(id=BEND_N[,1]), sum)
	    names(N_BND_NAT_INI)[2]<-"rep_1"
	    for(i in 2:inputs$nreps)
	    {
	      app<- aggregate(Z_N[,i], by=list(id=BEND_N[,i]), sum)
	      names(app)[2]<- paste("rep", i, sep="_")
	      N_BND_NAT_INI<- merge(N_BND_NAT_INI,app, by="id", all=TRUE)
	    }
	    N_BND_NAT_INI<- merge(N_BND_NAT_INI, 
	                          data.frame(id=0:length(inputs$bend_lengths)),
	                          by="id", all=TRUE)
	    N_BND_NAT_INI<- N_BND_NAT_INI[order(N_BND_NAT_INI$id),]
	    N_BND_NAT_INI<- matrix(unname(unlist(c(N_BND_NAT_INI[-1,-1]))), 
	                           nrow=length(inputs$bend_lengths),
	                           ncol=inputs$nreps)
	    N_BND_NAT_INI[is.na(N_BND_NAT_INI)]<-0
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
	    
	    # IF JANUARY, UPDATE MATURITY AND PROJECTED JUNE 
	    # MPS & SPAWNING STATUS
	    if(m[i]==1)
	    {
	      tmp_H<-dMaturity(mature = MAT_H[indx_H],
	                       age = AGE_H[indx_H],
	                       live = Z_H[indx_H],
	                       cond_mat_dist = inputs$pMatC)
	      tmp_N<-dMaturity(mature = MAT_N[indx_N],
	                       age = AGE_N[indx_N],
	                       live = Z_N[indx_N],
	                       cond_mat_dist = inputs$pMatC)
	      
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
	    }

	    # IF JUNE, RECRUITMENT AND SPAWNING MODULES
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
	        chk<-min(nrow(Z_H)-colSums(Z_H)-colSums(AGE_0_H_BND))
	        if(chk<0)
	        {
	          z0<-matrix(0,ncol=ncol(Z_H), nrow=abs(chk))
	          Z_H<-rbind(Z_H,z0)
	          AGE_H<-rbind(AGE_H,z0)
	          MAT_H<-rbind(MAT_H,z0)
	          MPS_H<-rbind(MPS_H,z0)
	          SPN_H<-rbind(SPN_H,z0)
	          SEX_H<-rbind(SEX_H,z0)
	          EGGS_H<-rbind(EGGS_H,z0)
	          k_H<-rbind(k_H,z0)
	          Linf_H<-rbind(Linf_H,z0)
	          LEN_H<-rbind(LEN_H,z0)
	          if(weightCalc)
	          {
	            WGT_H<-rbind(WGT_H,z0)
	          }
	          if(inputs$spatial)
	          {
	            BEND_H<-rbind(BEND_H,z0)
	          }
	        }
	        # INDEX OF OPEN SLOTS
	        indxr<- unlist(sapply(1:inputs$nreps,function(x)
	        {
	          tmp<-NULL
	          if(sum(AGE_0_H_BND[,x])>0)
	          {
	            tmp<-which(Z_H[,x]==0)[1:sum(AGE_0_H_BND[,x])]
	          }
	          return(tmp)
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
	        if(inputs$spatial)
	        {
	          indxB<-which(AGE_0_H_BND!=0, arr.ind=TRUE)
	          BEND_H[indxr]<-rep(indxB[,1], AGE_0_H_BND[indxB])
	          #sample this vector to make it more random???
	        }	
	      }
	      ##### NATURALLY SPAWNED FISH
	      if(sum(AGE_0_N_BND)>0)
	      {
	        chk<-min(nrow(Z_N)-colSums(Z_N)-colSums(AGE_0_N_BND))
	        if(chk<0)
	        {
	          z0<-matrix(0,ncol=ncol(Z_N), nrow=abs(chk))
	          Z_N<-rbind(Z_N,z0)
	          AGE_N<-rbind(AGE_N,z0)
	          MAT_N<-rbind(MAT_N,z0)
	          MPS_N<-rbind(MPS_N,z0)
	          SPN_N<-rbind(SPN_N,z0)
	          SEX_N<-rbind(SEX_N,z0)
	          EGGS_N<-rbind(EGGS_N,z0)
	          k_N<-rbind(k_N,z0)
	          Linf_N<-rbind(Linf_N,z0)
	          LEN_N<-rbind(LEN_N,z0)
	          if(weightCalc)
	          {
	            WGT_N<-rbind(WGT_N,z0)
	          }
	          if(inputs$spatial)
	          {
	            BEND_N<-rbind(BEND_N,z0)
	          }
	        }
	        # INDEX OF OPEN SLOTS
	        indxr<- unlist(sapply(1:inputs$nreps,function(x)
	        {
	          tmp<-NULL
	          if(sum(AGE_0_N_BND[,x])>0)
	          {
	            tmp<-which(Z_N[,x]==0)[1:sum(AGE_0_N_BND[,x])]
	          }
	          return(tmp)
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
	        if(inputs$spatial)
	        {
	          indxB<-which(AGE_0_N_BND!=0, arr.ind=TRUE)
	          BEND_N[indxr]<-rep(indxB[,1], AGE_0_N_BND[indxB])
	          #RKM[indxr]<- bend2rkm(c(unlist(sapply(1:inputs$nreps,
	          #                                      function(x){rep(1:inputs$n_bends,AGE_0_N_BND[,x])}))))
	        }		
	      }
	      #### ZERO OUT AGE-0's AFTER THEY MOVE TO AGE-1
	      AGE_0_N_BND[]<-0 
	      AGE_0_H_BND[]<-0	
	      
	      ### UPDATE THE NUMBER OF EGGS IN A FEMALE 
	      ### GIVEN SEX AND SPAWNING STATUS
	      EGGS_H[indx_H]<-fecundity(fl=LEN_H[indx_H],
	                                a=inputs$fec_a,
	                                b=inputs$fec_b,
	                                er=inputs$fec_er,
	                                sex=SEX_H[indx_H],
	                                spawn=SPN_H[indx_H])	
	      EGGS_N[indx_N]<-fecundity(fl=LEN_N[indx_N],
	                                a=inputs$fec_a,
	                                b=inputs$fec_b,
	                                er=inputs$fec_er,
	                                sex=SEX_N[indx_N],
	                                spawn=SPN_N[indx_N])
	      
	      ## NUMBER OF EGGS
	      if(!inputs$spatial)
	      {
	        AGE_0_N_BND<- matrix(colSums(EGGS_N)+colSums(EGGS_H),nrow=1)
	      }
	      if(inputs$spatial)
	      {
	        # EGGS OF NATURAL ORIGIN FISH BY BEND
	        EGG_N_BND<- aggregate(EGGS_N[,1], by=list(id=BEND_N[,1]), sum)
	        names(EGG_N_BND)[2]<-"rep_1"
	        for(j in 2:inputs$nreps)
	        {
	          app<- aggregate(EGGS_N[,j], by=list(id=BEND_N[,j]), sum)
	          names(app)[2]<- paste("rep", j, sep="_")
	          EGG_N_BND<- merge(EGG_N_BND, app, by="id", all=TRUE)
	        }
	        EGG_N_BND<- merge(EGG_N_BND, 
	                          data.frame(id=0:length(inputs$bend_lengths)),
	                          by="id", all=TRUE)
	        EGG_N_BND<- EGG_N_BND[order(EGG_N_BND$id),]
	        EGG_N_BND<- matrix(unname(unlist(c(EGG_N_BND[-1,-1]))), 
	                           nrow=length(inputs$bend_lengths),
	                           ncol=inputs$nreps)
	        EGG_N_BND[is.na(EGG_N_BND)]<-0
	        # EGGS OF HATCHERY ORIGIN FISH BY BEND
	        EGG_H_BND<- aggregate(EGGS_H[,1], by=list(id=BEND_H[,1]), sum)
	        names(EGG_H_BND)[2]<-"rep_1"
	        for(j in 2:inputs$nreps)
	        {
	          app<- aggregate(EGGS_H[,j], by=list(id=BEND_H[,j]), sum)
	          names(app)[2]<- paste("rep", j, sep="_")
	          EGG_H_BND<- merge(EGG_H_BND, app, by="id", all=TRUE)
	        }
	        EGG_H_BND<- merge(EGG_H_BND, 
	                          data.frame(id=0:length(inputs$bend_lengths)),
	                          by="id", all=TRUE)
	        EGG_H_BND<- EGG_H_BND[order(EGG_H_BND$id),]
	        EGG_H_BND<- matrix(unname(unlist(c(EGG_H_BND[-1,-1]))), 
	                           nrow=length(inputs$bend_lengths),
	                           ncol=inputs$nreps)
	        EGG_H_BND[is.na(EGG_H_BND)]<-0
	        # TOTAL EGGS BY BEND
	        AGE_0_N_BND<- EGG_N_BND+EGG_H_BND
	      }
	      
	      ### eggs --> embryos	
	      ### STATUS: DONE NEEDS TO BE MODIFIED WITH DENSITY<-DENSITY DEPENDENT SURVIVAL OR JUST BY BEND??? (NOW DONE BY BEND)
	      AGE_0_N_BND[]<- rbinom(length(AGE_0_N_BND),
	                             c(AGE_0_N_BND),
	                             inputs$pr_embryo) 
	      
	      ### embryos --> free embryos
	      AGE_0_N_BND[]<- rbinom(length(AGE_0_N_BND),
	                             c(AGE_0_N_BND),
	                             inputs$phi_embryo)
	      ### free embryos --> exogenously feeding age0's
	      AGE_0_N_BND[]<- rbinom(length(AGE_0_N_BND),
	                             c(AGE_0_N_BND),
	                             inputs$phi_free_embryo) 
	      
	      
	      ### ADJUST FOR THE AGE-0 THAT WERE INTERCEPTED AND RETAINED IN
	      ### THE BASIN
	      if(!inputs$spatial)
	      {
	        AGE_0_N_BND[]<- rbinom(length(AGE_0_N_BND),
	                               c(AGE_0_N_BND),
	                               inputs$p_retained)
	      }
	      if(inputs$spatial)
	      {
	        AGE_0_N_BND[]<- freeEmbryoDrift(bendAbund=AGE_0_N_BND, 
	                                        driftMatrix=inputs$drift_prob)
	      }
	    }
	    
	    
	    # IF SEPTEMBER (OR OTHER STOCKING MONTHS), STOCKING MODULE
	    if(stockingFreq>0)
	    {
	      ## PALLID STURGEON STOCKING 
	      ### STATUS: DONE 
	      ### NOTE: NEED TO MODIFY TO ALLOW COHORT, PARENT, AND LOCATION INFO
	      ### PROBABLY MORE EFFICIENT TO RBIND TO REDUCED DATASET RATHER THAN BIG MATRIX OF BENDS
	      ### COULD DO ONE FOR NATURAL AND HATHCERY AND KEEP TRACK IN LONG FORMAT...
	      if(any(inputs$fingerling$month==m[i]))
	      {
	        indx<-which(inputs$fingerling$month==m[i])
	        if(any(inputs$fingerling$number[indx] > 0))
	        {
	          # ADD NUMBER OF FISH STOCKED IN A BEND
	          if(!inputs$spatial)
	          {
	            AGE_0_H_BND<- AGE_0_H_BND+inputs$fingerling$number[indx]
	          }
	          if(inputs$spatial)
	          {
	           AGE_0_H_BND[inputs$fingerling$bend[indx],]<- 
	              AGE_0_H_BND[inputs$fingerling$bend[indx],]+inputs$fingerling$number[indx]
	          }
	        }
	        rm(indx)
	      }
	      
	      ### YEARLING STOCKING (AGE-1+)
	      ### STOCK INDIVIDUAL FISH INTO BENDS
	      ### STATUS: NEEDS TO BE UPDATED FOR SPATIAL
	      if(any(inputs$yearling$month==m[i]))
	      {
	        indx<-which(inputs$yearling$month==m[i])
	        if(any(inputs$yearling$number[indx]>0))
	        {
	          chk<-min(nrow(Z_H)-colSums(Z_H)-sum(inputs$yearling$number[indx]))
	          if(chk<0)
	          {
	            z0<-matrix(0,ncol=ncol(Z_H), nrow=abs(chk))
	            Z_H<-rbind(Z_H,z0)
	            AGE_H<-rbind(AGE_H,z0)
	            MAT_H<-rbind(MAT_H,z0)
	            MPS_H<-rbind(MPS_H,z0)
	            SPN_H<-rbind(SPN_H,z0)
	            SEX_H<-rbind(SEX_H,z0)
	            EGGS_H<-rbind(EGGS_H,z0)
	            k_H<-rbind(k_H,z0)
	            Linf_H<-rbind(Linf_H,z0)
	            LEN_H<-rbind(LEN_H,z0)
	            if(weightCalc)
	            {
	              WGT_H<-rbind(WGT_H,z0)
	            }
	            if(inputs$spatial)
	            {
	              BEND_H<-rbind(BEND_H,z0)
	            }
	          }
	          ### GET INDEXES OF OPEN SLOTS TO STICK STOCKED INDIVIDUALS
	          indx_R<- lapply(1:inputs$nreps,
	                          function(x){out<- which(Z_H[,x]==0)[1:sum(inputs$yearling$number[indx])]}) 
	          indx_R<- cbind(unlist(indx_R),
	                         rep(1:inputs$nreps,each=sum(inputs$yearling$number[indx])))
	        
	          ### ADD NEWLY STOCKED INDIVIDUALS TO Z_H	
	          Z_H[indx_R]<- 1
	          ### INITIALIZE LENGTH
	          LEN_H[indx_R]<- rnorm(sum(inputs$yearling$number[indx])*inputs$nreps,
	                                rep(rep(inputs$yearling$length_mn[indx], 
	                                        inputs$yearling$number[indx]),
	                                    inputs$nreps),
	                                rep(rep(inputs$yearling$length_sd[indx],
	                                        inputs$yearling$number[indx]),
	                                    inputs$nreps))
	          LEN_H[indx_R]<-ifelse(LEN_H[indx_R]<0, 100, LEN_H[indx_R])
	        
	          ### ASSIGN AGE	
	          AGE_H[indx_R]<- rep(rep(inputs$yearling$age[indx], 
	                                  inputs$yearling$number[indx]), 
	                              inputs$nreps) 
	          ### ASSIGN MATURATION, MPS, AND SPAWNING STATUS 
	          MAT_H[indx_R]<- 0	
	          MPS_H[indx_R]<- 0 
	          SPN_H[indx_R]<- 0 
	        
	          ### ASSIGN SEX
	          SEX_H[indx_R]<-rbinom(length(indx_R[,1]), 1, p=0.5)
	          
	          ### ASSIGN BEND LOCATION
	          if(inputs$spatial)
	          {
	            BEND_H[indx_R]<- rep(rep(inputs$yearling$bend[indx], 
	                                     inputs$yearling$number[indx]), 
	                                 inputs$nreps) 
	          }
	        }
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
	    
	    
	    # MOVEMENT:  UPDATE LOCATION
	    if(inputs$spatial)
	    {	
	      ### ZERO OUT FISH THAT DIED
	      BEND_H<- BEND_H*Z_H
	      BEND_N<- BEND_N*Z_N
	      ### SIMULATE ADULT MOVEMENT AMONG BENDS 
	      ### DEPENDING ON WHAT BEND FISH IS IN
	      ### AND IF FISH IS SPAWNING
	      ### STATUS:  DONE BUT SLOW
	      BEND_H[indx_H]<- adultMovement(
	        previousLocation=BEND_H[indx_H],
	        month=m[i],
	        spn=SPN_H[indx_H],
	        fromToMatrix=inputs$adult_mov_prob,
	        spnMatrix=inputs$spn_mov_prob)
	      BEND_N[indx_N]<- adultMovement(
	        previousLocation=BEND_N[indx_N],
	        month=m[i],
	        spn=SPN_N[indx_N],
	        fromToMatrix=inputs$adult_mov_prob,
	        spnMatrix=inputs$spn_mov_prob)
	      #source("./src/spatial-dynamics.R")
	    }
	    
	    
	    #[3] UPDATE TOTAL LENGTH (AND ZERO OUT DEAD FISH) 
	    LEN_H[indx_H]<-dLength(k=k_H[indx_H],
	                           linf=Linf_H[indx_H],
	                           dT=1/12,
	                           length1=LEN_H[indx_H])*Z_H[indx_H]
	    LEN_N[indx_N]<-dLength(k=k_N[indx_N],
	                           linf=Linf_N[indx_N],
	                           dT=1/12,
	                           length1=LEN_N[indx_N])*Z_N[indx_N]
	    
	    
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
	    
	    ## END POPULATION DYNAMICS
	    
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
	  
	  if(inputs$spatial)
	  {
	    # ENDING BEND ABUNDANCE SUMMARIES
	    N_BND_HAT_POST<- aggregate(Z_H[,1], by=list(id=BEND_H[,1]), sum)
	    names(N_BND_HAT_POST)[2]<-"rep_1"
	    for(j in 2:inputs$nreps)
	    {
	      app<- aggregate(Z_H[,j], by=list(id=BEND_H[,j]), sum)
	      names(app)[2]<- paste("rep", j, sep="_")
	      N_BND_HAT_POST<- merge(N_BND_HAT_POST,app, by="id", all=TRUE)
	    }
	    N_BND_HAT_POST<- merge(N_BND_HAT_POST, 
	                           data.frame(id=0:length(inputs$bend_lengths)),
	                           by="id", all=TRUE)
	    N_BND_HAT_POST<- N_BND_HAT_POST[order(N_BND_HAT_POST$id),]
	    N_BND_HAT_POST<- matrix(unname(unlist(c(N_BND_HAT_POST[-1,-1]))), 
	                            nrow=length(inputs$bend_lengths),
	                            ncol=inputs$nreps)
	    N_BND_HAT_POST[is.na(N_BND_HAT_POST)]<-0
	    
	    N_BND_NAT_POST<- aggregate(Z_N[,1], by=list(id=BEND_N[,1]), sum)
	    names(N_BND_NAT_POST)[2]<-"rep_1"
	    for(j in 2:inputs$nreps)
	    {
	      app<- aggregate(Z_N[,j], by=list(id=BEND_N[,j]), sum)
	      names(app)[2]<- paste("rep", j, sep="_")
	      N_BND_NAT_POST<- merge(N_BND_NAT_POST,app, by="id", all=TRUE)
	    }
	    N_BND_NAT_POST<- merge(N_BND_NAT_POST, 
	                           data.frame(id=0:length(inputs$bend_lengths)),
	                           by="id", all=TRUE)
	    N_BND_NAT_POST<- N_BND_NAT_POST[order(N_BND_NAT_POST$id),]
	    N_BND_NAT_POST<- matrix(unname(unlist(c(N_BND_NAT_POST[-1,-1]))), 
	                            nrow=length(inputs$bend_lengths),
	                            ncol=inputs$nreps)
	    N_BND_NAT_POST[is.na(N_BND_NAT_POST)]<-0
	  }
	  
	  
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
	  if(inputs$spatial)
	  {
	    out$bend_N_initial<- N_BND_NAT_INI
	    out$bend_H_initial<- N_BND_HAT_INI
	    out$bend_N_post<- N_BND_NAT_POST
	    out$bend_H_post<- N_BND_HAT_POST
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
