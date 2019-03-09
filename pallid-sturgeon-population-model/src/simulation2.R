

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
	  AGE_0_H<-inputs$hatchery_age0
	  
	  if(inputs$spatial | inputs$migration)
	  {
	    BEND_N <- dyn$BEND_N
	    BEND_H <- dyn$BEND_H
	  }
	  
	  inBasinH<-1
	  inBasinN<-1
	  
	  if(inputs$genetics)
	  {
	    MOM_H<-dyn$MOM_H
	    DAD_H<-dyn$DAD_H
	    TAG_N<-dyn$TAG_N
	    bank_F<-rep(list(inputs$broodstock$bank_F), inputs$nreps)
	    bank_M<-rep(list(inputs$broodstock$bank_M), inputs$nreps)
	  }
	  
	  if(inputs$hatchery_name)
	  {
	    HATCH<-dyn$HATCH
	  }
	  
	  m<-dyn$m

	  
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
	  
	  if(inputs$spatial)
	  {
	    # INITIAL BEND ABUNDANCE
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

	  if(inputs$genetics)
	  {
	    N_e<- matrix(0,nrow=length(which(m==12)),ncol=inputs$nreps)
	    indxY<-0
	  }
	  
	  
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
	    
	    ## IF MARCH, BROODSTOCK MODULE
	    ### HATCHERY DATA
	    if(inputs$genetics & m[i]==3)
	    {
	      ### ZERO OUT FISH THAT DIED
	      SPN_H<-SPN_H*Z_H
	      SPN_N<-SPN_N*Z_N
	      
	      SEX_H<-SEX_H*Z_H
	      SEX_N<-SEX_N*Z_N
	      
	      ## FEMALES
	      BRST_F<-lapply(1:inputs$nreps, function(j)
	      {
	        # NATURAL ORIGIN FEMALE SPAWNERS
	        indx<-which(SPN_N[,j]==1 & SEX_N[,j]==1)
	        if(inputs$migration)
	        {
	          indx<-intersect(indx, which(BND_N[,j]<=inputs$n_bends))
	        }
	        # FEMALE CATCH
	        indx<-sample(indx, rbinom(1, length(indx), inputs$broodstock$cp))
	        # WHICH FEMALES HAVE NOT BEEN BRED BEFORE
	        tags<-TAG_N[indx,j]
	        priority<-ifelse(tags %in% bank_F[[j]], 0, 1)
	        # USE ONLY FISH THAT WERE NOT PREVIOUSLY BRED
	        ## ENOUGH NEW CATCH TO SATISFY GOAL
	        if(sum(priority)>inputs$broodstock$breeder_no)
	        {
	          #out<-indx[sample(which(priority==1),inputs$broodstock$breeder_no)]
	          out<-indx[which(priority==1)[1:inputs$broodstock$breeder_no]]
	        }
	        ## NOT ENOUGH NEW CATCH TO SATISFY GOAL
	        if(sum(priority)<=inputs$broodstock$breeder_no)
	        {
	          out<-indx
	          # NO FISH AVAILABLE
	          if(length(out)==0){out<-rep(NA,inputs$broodstock$breeder_no)}
	          # USE AVAILABLE NEW BREEDERS IN MULTIPLE FAMILY LOTS
	          need<- inputs$broodstock$breeder_no-length(out)
	          if(need>0)
	          {
	            W<-floor(need/length(out))
	            R<-need-W*length(out)
	            out<-c(rep(out,W+1), sample(out,R))
	          }
	        }
#           if(length(priority)<inputs$broodstock$breeder_no)
# 	        {
#             need <- inputs$broodstock$breeder_no-length(out)
#             if(length(which(priority==0))>=need)
#             {
#               tmp<-indx[sample(which(priority==0),need)]
#             }
#             if(length(which(priority==0))<need)
#             {
#               tmp<- indx[which(priority==0)]
#               need <- need-length(which(priority==0))
#               tmp<- c(tmp, sample(indx,need))
#             }
#             tmp<-indx[sample(which(priority==0),inputs$broodstock$breeder_no-length(out))]
# 	          }
# 	        if(sum(priority)<inputs$broodstock$breeder_no)
# 	        {
# 	          if(length(priority)>=inputs$broodstock$breeder_no)
# 	          {
# 	            tmp<-indx[sample(which(priority==0),inputs$broodstock$breeder_no-length(out))]
# 	          }
# 	          #out<-indx[which(priority==1)]
# 	          #tmp<-which(bank_F[[j]]$tag %in% tags & bank_F[[j]]$times<2)
# 	          #tmp<-sample(bank_F[[j]]$tag[tmp], min(inputs$broodstock$breeder_no-length(out), length(tmp)))
# 	          #tmp<-which(tags %in% tmp)
# 	          #tmp<-indx[tmp]
# 	          out<-c(out,tmp)
# 	        }
	        out<-cbind(out, rep(j,inputs$broodstock$breeder_no))
	        #out<-cbind(out, rep(j,length(out)))
	        return(out)
	      })
	      BRST_F<-do.call(rbind, BRST_F)
	      ### UPDATE FEMALE BROODSTOCK BANK
	      bank_F<-lapply(1:inputs$nreps, function(j)
	      {
	        out<-unique(c(bank_F[[j]], TAG_N[BRST_F[which(BRST_F[,2]==j),1],j]))
	        return(out)
	      })

	      ## MALES
	      BRST_M<-lapply(1:inputs$nreps, function(j)
	      {
	        # NATURAL ORIGIN MALE SPAWNERS
	        indx<-which(SPN_N[,j]==1 & SEX_N[,j]==0)
	        if(inputs$migration)
	        {
	          indx<-intersect(indx, which(BND_N[,j]<=inputs$n_bends))
	        }
	        # MALE CATCH
	        indx<-sample(indx, rbinom(1, length(indx), inputs$broodstock$cp))
	        # WHICH MALES HAVE NOT BEEN BRED BEFORE
	        tags<-TAG_N[indx,j]
	        priority<-ifelse(tags %in% bank_M[[j]], 0, 1)
	        # USE ONLY FISH THAT WERE NOT PREVIOUSLY BRED
	        ## ENOUGH NEW CATCH TO SATISFY GOAL
	        if(sum(priority)>inputs$broodstock$breeder_no)
	        {
	          out<-indx[sample(which(priority==1),inputs$broodstock$breeder_no)]
	        }
	        ## NOT ENOUGH NEW CATCH TO SATISFY GOAL
	        if(sum(priority)<=inputs$broodstock$breeder_no)
	        {
	          out<-indx
	          # NO FISH AVAILABLE
	          if(length(out)==0){out<-rep(NA,inputs$broodstock$breeder_no)}
	          # USE AVAILABLE NEW BREEDERS IN MULTIPLE FAMILY LOTS
	          need<- inputs$broodstock$breeder_no-length(out)
	          if(need>0)
	          {
	            W<-floor(need/length(out))
	            R<-need-W*length(out)
	            out<-c(rep(out,W+1), sample(out,R))
	          }
	        }
	        out<-cbind(out, rep(j,inputs$broodstock$breeder_no))
	        #out<-cbind(out, rep(j,length(out)))
	        return(out)
	      })
	      BRST_M<-do.call(rbind, BRST_M)
	      ### UPDATE MALE BROODSTOCK BANK
	      bank_M<-lapply(1:inputs$nreps, function(j)
	      {
	        out<-unique(c(bank_M[[j]], TAG_N[BRST_M[which(BRST_M[,2]==j),1],j]))
	        return(out)
	      })
	      ## STORE INITIAL BROODSTOCK DATA
	      tmp<-rbind(BRST_F, BRST_M)
	      tmp<- tmp[!is.na(tmp[,1]),]
	      BRST_DAT<-data.frame(k=k_N[tmp],
	                           Linf=Linf_N[tmp],
	                           Length=LEN_N[tmp],
	                           Age=AGE_N[tmp],
	                           Sex=SEX_N[tmp],
	                           Tag=TAG_N[tmp],
	                           rep=tmp[,2])
	      if(weightCalc){BRST_DAT$Weight<-WGT_N[tmp]}
	      if(inputs$spatial){BRST_DAT$Bend<-BEND_N[tmp]}
	      ## ZERO OUT BROODSTOCK FISH FROM RIVER POPULATION
	      Z_N[tmp]<-0
	      AGE_N[tmp]<-0
	      LEN_N[tmp]<-0
	    }

	    # IF JUNE, RECRUITMENT AND NATURAL SPAWNING MODULES
	    ### RECRUITMENT AND SPAWNING
	    if(recruitmentFreq>0 & m[i]==6)
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
	      AGE_0_N_BND[]<- rbinom(length(AGE_0_N_BND),
	                             AGE_0_N_BND,
	                             inputs$phi0)
	      AGE_0_N_OUT[]<- rbinom(length(AGE_0_N_OUT),
	                             AGE_0_N_OUT,
	                             inputs$phi0)
	      if(sum(AGE_0_H$number)>0)
	      {
	        AGE_0_H_DAT<- matrix(rbinom(inputs$nreps*nrow(AGE_0_H),
	                                    AGE_0_H$number,
	                                    AGE_0_H$survival_est),
	                             ncol=inputs$nreps,
	                             nrow=nrow(AGE_0_H))
	      }
	      if(sum(AGE_0_H$number)==0)
	      {
	        AGE_0_H_DAT<- matrix(0, ncol=inputs$nreps, nrow=1)
	      }
	      
	      #### ADD SURVIVING FISH TO POPULATION
	      ##### HATCHERY STOCKED FISH
	      if(sum(AGE_0_H_DAT)>0)
	      {
	        chk<-min(nrow(Z_H)-colSums(Z_H)-colSums(AGE_0_H_DAT))
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
	          if(inputs$spatial | inputs$migration)
	          {
	            BEND_H<-rbind(BEND_H,z0)
	          }
	          if(inputs$genetics)
	          {
	            MOM_H<-rbind(MOM_H,z0)
	            DAD_H<-rbind(DAD_H,z0)
	          }
	          if(inputs$hatchery_name)
	          {
	            HATCH<-rbind(HATCH,z0)
	          }
	        }
	        # INDEX OF OPEN SLOTS
	        indxr<- unlist(sapply(1:inputs$nreps,function(x)
	        {
	          tmp<-NULL
	          if(sum(AGE_0_H_DAT[,x])>0)
	          {
	            tmp<-which(Z_H[,x]==0)[1:sum(AGE_0_H_DAT[,x])]
	          }
	          return(tmp)
	        }))		
	        indxr<- cbind(c(indxr),sort(rep(1:inputs$nreps,colSums(AGE_0_H_DAT))))
	        # ADD NEW 1 YEAR OLD RECRUITS
	        Z_H[indxr]<-1 
	        # ADD AGE OF RECRUITS
	        AGE_H[indxr]<-12 	
	        # UPDATE GROWTH COEFFICIENTS
	        tmp<- ini_growth(n=sum(AGE_0_H_DAT),
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
	        #LEN_H[indxr]<-rnorm(length(indxr[,1]),
	        #                    rep(rep(AGE_0_H$length_mn,inputs$nreps), 
	        #                        as.vector(AGE_0_H_DAT)),
	        #                    rep(rep(AGE_0_H$length_sd,inputs$nreps), 
	        #                        as.vector(AGE_0_H_DAT)))
	        #Linf_H[indxr]<-ifelse(LEN_H[indxr]<Linf_H[indxr], 
	        #                      Linf_H[indxr],
	        #                      LEN_H[indxr]*1.1)
	        #LEN_H[indxr]<-dLength(k=k_H[indxr], 
	        #                      linf=Linf_H[indxr],
	        #                      length1=LEN_H[indxr]
	        #                      dT=0.5)
	        # ASSIGN SEX
	        SEX_H[indxr]<-ini_sex(n=length(indxr[,1]),
	                              prob_F=inputs$sexratio)
	        # ASSIGN LOCATION OF RECRUITS
	        if(!inputs$spatial & inputs$migration)
	        {
	          BEND_H[indxr]<-1
	        }
	        if(inputs$spatial)
	        {
	          BEND_H[indxr]<-rep(rep(AGE_0_H$bend, inputs$nreps),
	                             as.vector(AGE_0_H_DAT))
	          #CAN ADD IN DISPERSAL HERE
	          #BEND_H[indxr]<- dispersal stuff
	        }
	        if(inputs$genetics)
	        {
	          MOM_H[indxr]<-rep(rep(AGE_0_H$mother, inputs$nreps),
	                            as.vector(AGE_0_H_DAT))
	          DAD_H[indxr]<-rep(rep(AGE_0_H$father, inputs$nreps),
	                            as.vector(AGE_0_H_DAT))
	        }
	        if(inputs$hatchery_name)
	        {
	          HATCH[indxr]<-rep(rep(AGE_0_H$hatchery, inputs$nreps),
	                            as.vector(AGE_0_H_DAT))
	        }
	      }
	      ##### NATURALLY SPAWNED FISH WITHIN MISSOURI RIVER
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
	          if(inputs$spatial | inputs$migration)
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
	        if(!inputs$spatial & inputs$migration)
	        {
	          BEND_N[indxr]<-1
	        }	
	        if(inputs$spatial)
	        {
	          indxB<-which(AGE_0_N_BND!=0, arr.ind=TRUE)
	          BEND_N[indxr]<-rep(indxB[,1], AGE_0_N_BND[indxB])
	          #RKM[indxr]<- bend2rkm(c(unlist(sapply(1:inputs$nreps,
	          #                                      function(x){rep(1:inputs$n_bends,AGE_0_N_BND[,x])}))))
	        }		
	      }
	      ##### NATURALLY SPAWNED FISH OUTSIDE OF FLOWING MISSOURI RIVER
	      if(sum(AGE_0_N_OUT)>0)
	      {
          # BATCH AGE-1's FOR EACH REPLICATE
	        tmp<-merge(data.frame(Origin=0, # 1 FOR HATCHERY, 0 FOR NATURAL
	                              Age=12), 
	                   data.frame(Number=AGE_0_N_OUT[1,],
	                              Rep=1:inputs$nreps))
	        # # ADD GROWTH COEFFICIENTS
	        # tmp2<- ini_growth(n=nrow(tmp),
	        #                   mu_ln_Linf=inputs$ln_Linf_mu,
	        #                   mu_ln_k=inputs$ln_k_mu,
	        #                   vcv=inputs$vcv,
	        #                   maxLinf=inputs$maxLinf) 
	        # tmp$Linf<-tmp2$linf
	        # tmp$k<-tmp2$k
	        # # ADD  INITIAL LENGTHS
	        # ## METHOD 1: LENGTH DISTRIBUTION (SEE ABOVE FOR METHOD 2)
	        # tmp$Length<-rnorm(nrow(tmp),
	        #                   inputs$recruit_mean_length,
	        #                   inputs$recruit_length_sd)				
	        # # ASSIGN SEX
	        # tmp$Sex<-ini_sex(n=nrow(tmp),
	        #                  prob_F=inputs$sexratio)
	        DRIFT_FISH<-rbind.fill(DRIFT_FISH, tmp)
	      }
	      #### ZERO OUT AGE-0's AFTER THEY MOVE TO AGE-1
	      AGE_0_N_BND[]<-0
	      AGE_0_N_OUT[]<-0
	      AGE_0_H_DAT[]<-0
	      AGE_0_H<-NULL
	      
	      ##### NATURAL REPRODUCTION
	      ### UPDATE THE NUMBER OF EGGS IN A FEMALE 
	      ### GIVEN SEX AND SPAWNING STATUS
	      if(inputs$migration)
	      {
	        inBasinH<-ifelse(BEND_H[indx_H] %in% 1:inputs$n_bends, 1, 0)
	        inBasinN<-ifelse(BEND_N[indx_N] %in% 1:inputs$n_bends, 1, 0)
	      }
	      EGGS_H[indx_H]<-fecundity(fl=LEN_H[indx_H],
	                                a=inputs$fec_a,
	                                b=inputs$fec_b,
	                                er=inputs$fec_er,
	                                sex=SEX_H[indx_H],
	                                spawn=SPN_H[indx_H])*inBasinH
	      EGGS_N[indx_N]<-fecundity(fl=LEN_N[indx_N],
	                                a=inputs$fec_a,
	                                b=inputs$fec_b,
	                                er=inputs$fec_er,
	                                sex=SEX_N[indx_N],
	                                spawn=SPN_N[indx_N])*inBasinN
	      
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
	      ### THE BASIN AND KEEP TRACK OF HOW MANY DRIFTED OUT OF THE BASIN
	      AGE_0_N_OUT <- AGE_0_N_BND
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
	      AGE_0_N_OUT <- colSums(AGE_0_N_OUT)-colSums(AGE_0_N_BND)
	    }
	    
	    
	    # IF JUNE, BROODSTOCK REPRODUCTION 
	    ### RECRUITMENT AND SPAWNING
	    if(stockingFreq>0 & m[i]==6)
	    {
	      if(inputs$genetics)
	      {
	        ##### GENERATE GENETICS STOCKING DATA
	        ### UPDATE BROODSTOCK LENGTH AND AGE
	        BRST_DAT$Age<-BRST_DAT$Age+3
	        BRST_DAT$Length<-dLength(k=BRST_DAT$k,
	                                 linf=BRST_DAT$Linf,
	                                 dT=1/4,
	                                 length1=BRST_DAT$Length)
	        ### EGGS PRODUCED
	        BRST_DAT$Eggs<-fecundity(fl=BRST_DAT$Length,
	                                 a=inputs$fec_a,
	                                 b=inputs$fec_b,
	                                 er=inputs$fec_er,
	                                 sex=BRST_DAT$Sex,
	                                 spawn=1)
	        ### RANDOM BREEDING
	        BROOD<-lapply(1:inputs$nreps, function(j)
	        {
	          out<-NULL
	          if(length(which(BRST_DAT$rep==j))==2*inputs$broodstock$breeder_no)
	          {
	            indxF<-sample(which(BRST_DAT$Sex==1 & BRST_DAT$rep==j),inputs$broodstock$breeder_no)
	            indxM<-sample(which(BRST_DAT$Sex==0 & BRST_DAT$rep==j),inputs$broodstock$breeder_no)
	            out<-data.frame(hatchery=sample(c("Neosho", "Gavins"), 
	                                            inputs$broodstock$breeder_no, replace=TRUE),
	                            mother=BRST_DAT[indxF,]$Tag,
	                            father=BRST_DAT[indxM,]$Tag,
	                            no_offspring=BRST_DAT[indxF,]$Eggs,
	                            hatchery_survival=plogis(rnorm(inputs$broodstock$breeder_no, 
	                                                           log(inputs$phi0_Hcap_mean/(1-inputs$phi0_Hcap_mean)),
	                                                           inputs$phi0_Hcap_er)),
	                            rep=j)
	          }
	          return(out)
	          #THIS IS GENETICS_INFO$FINGERLINGS MINUS SURVIVAL TO AGE 3 
	          #MONTHS AND NEXT YEARS GENETICS_INFO$YEARLINGS AFTER 
	          #FINGERLING STOCKING AND SURVIVAL TO 15 MONTHS
	        })
	        BROOD<-do.call("rbind", BROOD)
	        
	        ### RELEASE ADULTS BACK INTO POPULATION
	        chk<-min(nrow(Z_N)-colSums(Z_N)-(2*inputs$broodstock$breeder_no))
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
	        indx0<- lapply(1:inputs$nreps,function(x)
	        {
	          tmp<-which(Z_N[,x]==0)[1:length(which(BRST_DAT$rep==x))]
	          tmp<-cbind(tmp,rep(x,length(which(BRST_DAT$rep==x))))
	          return(tmp)
	        })
	        indx0<-do.call(rbind, indx0) 
	        indxb<- unlist(lapply(1:inputs$nreps,function(x)
	        {
	          tmp<-which(BRST_DAT$rep==x)[1:length(which(indx0[,2]==x))]
	          tmp<-sample(tmp)
	          return(tmp)
	        }))
	        Z_N[indx0]<-1    
	        AGE_N[indx0]<-BRST_DAT$Age[indxb] 	
	        Linf_N[indx0]<-BRST_DAT$Linf[indxb]
	        k_N[indx0]<-BRST_DAT$k[indxb]
	        LEN_N[indx0]<-BRST_DAT$Length[indxb]				
	        SEX_N[indx0]<-BRST_DAT$Sex[indxb]
	        MAT_N[indx0]<-1
	        MPS_N[indx0]<-0
	        SPN_N[indx0]<-1
	        if(weightCalc){WGT_N[indx0]<-BRST_DAT$Weight[indxb]}
	        # ASSIGN RELEASE LOCATION AS CAPTURE LOCATION
	        if(inputs$spatial){BEND_N[indx0]<-BRST_DAT$Bend[indxb]}	
	        # ZERO OUT BROODSTOCK
	        BRST_DAT<-NULL
	      }
	    }
	    
	    
	    # IF SEPTEMBER, STOCKING MODULE
	    ## NEED TO UPDATE IF WANT TO INCLUDE OTHER MONTHS
	    if(stockingFreq>0 & m[i]==9)
	    {
	      ### YEARLING STOCKING (AGE-1)
	      if(any(inputs$yearling$month==m[i]))
	      {
	        yearling<-inputs$yearling[which(inputs$yearling$month==m[i]),]
	        if(any(yearling$stocking_no>0))
	        {
	          #### CREATE ENOUGH OPEN SPACE FOR NEW AGE-1 FISH
	          chk<-min(nrow(Z_H)-colSums(Z_H)-sum(yearling$stocking_no))
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
	            if(inputs$genetics)
	            {
	              MOM_H<-rbind(MOM_H,z0)
	              DAD_H<-rbind(DAD_H,z0)
	            }
	            if(inputs$hatchery_name)
	            {
	              HATCH<-rbind(HATCH,z0)
	            }
	          }
	          if(inputs$genetics)
	          {
	            #### DETERMINE HOW MANY FISH OF EACH GENETIC TYPE TO STOCK
	            yearling$proportion<-ifelse(sum(inputs$yearling$stocking_no)>0,
	                                        yearling$stocking_no/
	                                          sum(inputs$yearling$stocking_no),
	                                        0)
	            yearling$desired_no<-floor(yearling$stocking_no/inputs$broodstock$breeder_no)
	            yearling <- as.data.frame(
	              lapply(yearling,function(x) rep(x,inputs$nreps)))
	            yearling$rep<-1:inputs$nreps
	            yearling<-merge(yearling, 
	                            BROOD_1[,c("mother", "father", "hatchery", 
	                                       "rep","yearlings")], 
	                            by="rep", all=TRUE)
	            yearling$available_no<-floor(yearling$proportion*yearling$yearlings)
	            yearling$number<-ifelse(yearling$desired_no<=yearling$available_no,
	                                    yearling$desired_no,
	                                    yearling$available_no)
	            #### GET INDEXES OF OPEN SLOTS TO STICK STOCKED INDIVIDUALS
	            indx_R<- lapply(1:inputs$nreps,
	                            function(x){out<- which(Z_H[,x]==0)[1:sum(yearling[which(yearling$rep==x),]$number)]}) 
	            indx_R<- cbind(unlist(indx_R),
	                           rep(1:inputs$nreps, sum(yearling[which(yearling$rep==x),]$number)))
	            
	          }
	          if(!inputs$genetics)
	          {
	            names(yearling)[3]<-"number"
	            #### GET INDEXES OF OPEN SLOTS TO STICK STOCKED INDIVIDUALS
	            indx_R<- lapply(1:inputs$nreps,
	                            function(x){out<- which(Z_H[,x]==0)[1:sum(yearling$number)]}) 
	            indx_R<- cbind(unlist(indx_R),
	                           rep(1:inputs$nreps, sum(yearling$number)))
	            

	          }
	          #### ADD IN NEWLY STOCKED INDIVIDUALS	
	          Z_H[indx_R]<- 1
	          ##### INITIALIZE LENGTH
	          LEN_H[indx_R]<- rnorm(sum(yearling$number),
	                                rep(yearling$length_mn, 
	                                    yearling$number),
	                                rep(yearling$length_sd,
	                                    yearling$number))
	          LEN_H[indx_R]<-ifelse(LEN_H[indx_R]<0, 100, LEN_H[indx_R])
	          
	          ##### AGE	
	          AGE_H[indx_R]<- rep(yearling$age, yearling$number)
	          ##### GROWTH COEFFICIENTS
	          tmp<- ini_growth(n=sum(yearling$number),
	                           mu_ln_Linf=inputs$ln_Linf_mu,
	                           mu_ln_k=inputs$ln_k_mu,
	                           vcv=inputs$vcv,
	                           maxLinf=inputs$maxLinf) 
	          Linf_H[indx_R]<-tmp$linf
	          k_H[indx_R]<-tmp$k
	          ##### ASSIGN MATURATION, MPS, SPAWNING STATUS, & EGGS
	          MAT_H[indx_R]<- 0	
	          MPS_H[indx_R]<- 0 
	          SPN_H[indx_R]<- 0 
	          EGGS_H[indx_R]<- 0 
	          
	          ##### ASSIGN SEX
	          SEX_H[indx_R]<-rbinom(length(indx_R[,1]), 1, p=0.5)
	          
	          ##### ASSIGN BEND LOCATION
	          if(inputs$spatial)
	          {
	            BEND_H[indx_R]<- rep(yearling$bend, yearling$number)
	          }
	          ##### ASSIGN PARENTS AND HATCHERY
	          if(inputs$genetics)
	          {
	            MOM_H[indx_R]<-rep(yearling$mother, yearling$number)
	            DAD_H[indx_R]<-rep(yearling$father, yearling$number)
	          }
	          if(inputs$hatchery_name)
	          {
	            HATCH[indx_R]<-rep(yearling$hatchery, yearling$number)
	          }
	        }
	      }
	      ### ZERO OUT AGE-1 FISH IN HATCHERY
	      BROOD_1<-NULL #OK UNLESS USING MULTIPLE STOCKING MONTHS OR AGES ABOVE AGE-1
	      yearling<-NULL
	      ### FINGERLING STOCKING (AGE-0)
	      #### UPDATE NUMBER OF OFFSPRING AVAILABLE FOR STOCKING
	      if(inputs$genetics)
	      {
	        BROOD$fingerlings<-rbinom(nrow(BROOD), 
	                                  BROOD$no_offspring, 
	                                  (BROOD$hatchery_survival)^(1/4))
	      }
	      #### DETERMINE NUMBER OF FISH STOCKED IN EACH BEND
	      if(any(inputs$fingerling$month==m[i]))
	      {
	        fingerling<-inputs$fingerling[which(inputs$fingerling$month==m[i]),]
	        if(inputs$genetics)
	        { 
	          if(any(fingerling$stocking_no>0))
	          {
	            fingerling$proportion<-ifelse(sum(inputs$fingerling$stocking_no)>0,
	                                          fingerling$stocking_no/
	                                          sum(inputs$fingerling$stocking_no),
	                                          0)
	            fingerling$desired_no<-floor(fingerling$stocking_no/inputs$broodstock$breeder_no)
	            fingerling <- as.data.frame(
	              lapply(fingerling,function(x) rep(x,inputs$nreps)))
	            fingerling$rep<-1:inputs$nreps
	            ### STOCKED AGE-0's
	            AGE_0_H<-merge(fingerling, 
	                           BROOD[,c("mother", "father", "hatchery", 
	                                    "rep","fingerlings")], 
	                           by="rep", all=TRUE)
	            AGE_0_H$available_no<-floor(AGE_0_H$proportion*
	                                          AGE_0_H$fingerlings)
	            AGE_0_H$number<-ifelse(AGE_0_H$desired_no<=AGE_0_H$available_no,
	                                   AGE_0_H$desired_no,
	                                   AGE_0_H$available_no)
	            AGE_0_H$survival_est<-plogis(rnorm(nrow(AGE_0_H), 
	                                               log(AGE_0_H$phi0_mn/(1-AGE_0_H$phi0_mn)),
	                                               AGE_0_H$phi0_sd))
	            ### NEXT YEAR's AVAILABLE YEARLINGS FOR STOCKING
	            # OKAY IF ONLY 1 MONTH; NEEDS  UPDATING FOR MULTIPLE MONTHS
	            BROOD_1<-aggregate(number~mother+father+hatchery+rep, AGE_0_H, sum)
	            names(BROOD_1)[5]<-"stocked"
	            BROOD_1<-merge(BROOD_1, BROOD[,c("mother", "father", "hatchery", 
	                                             "rep", "fingerlings")],
	                           by=c("mother", "father", "hatchery", "rep"),
	                           all.x=TRUE)
	            BROOD_1$hatchery_survival<-plogis(rnorm(nrow(BROOD_1), 
	                                                    log(inputs$yearling$phi_mn/(1-inputs$yearling$phi_mn)),
	                                                    inputs$yearling$phi_sd))
	            BROOD_1$yearlings<-rbinom(nrow(BROOD_1),
	                                      BROOD_1$fingerlings-BROOD_1$stocked,
	                                      BROOD_1$hatchery_survival)
	          }
	          if(all(fingerling$stocking_no==0))
	          {
	            ### STOCKED AGE-0's
	            AGE_0_H<-data.frame(number=0)
	            ### NEXT YEAR's AVAILABLE YEARLINGS FOR STOCKING
	            # OKAY IF ONLY 1 MONTH; NEEDS  UPDATING FOR MULTIPLE MONTHS
	            BROOD_1<-BROOD[,c("mother", "father", "hatchery", "rep", 
	                              "fingerlings")]
	            BROOD_1$hatchery_survival<-plogis(rnorm(nrow(BROOD_1), 
	                                                    log(inputs$yearling$phi_mn/(1-inputs$yearling$phi_mn)),
	                                                    inputs$yearling$phi_sd))
	            BROOD_1$yearlings<-rbinom(nrow(BROOD_1),
	                                      BROOD_1$fingerlings,
	                                      BROOD_1$hatchery_survival)
	          }
	          ### ZERO OUT AGE-0 BROODSTOCK DATA
	          # OKAY IF ONLY 1 MONTH; NEEDS  UPDATING FOR MULTIPLE MONTHS
	          BROOD<-NULL
	          fingerling<-NULL
	        }
	        if(!inputs$genetics)
	        {
	          ### STOCKED AGE-0's
	          AGE_0_H<-data.frame(number=sum(fingerling$stocking_no))
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
	    if(!inputs$spatial & inputs$migration)
	    {
	      ### ZERO OUT FISH THAT DIED
	      BEND_H<- BEND_H*Z_H
	      BEND_N<- BEND_N*Z_N
	      ### DETERMINE NEW FISH LOCATION
	      
	    }
	    if(inputs$spatial)
	    {	
	      ### ZERO OUT FISH THAT DIED
	      BEND_H<- BEND_H*Z_H
	      BEND_N<- BEND_N*Z_N
	      ### SIMULATE ADULT MOVEMENT AMONG BENDS 
	      ### DEPENDING ON WHAT BEND FISH IS IN
	      ### AND IF FISH IS SPAWNING
	      ### STATUS:  DONE BUT SLOW
	      BEND_H[indx_H]<- adultMovement(previousLocation=BEND_H[indx_H],
	                                     month=m[i],
	                                     spn=SPN_H[indx_H],
	                                     fromToMatrix=inputs$adult_mov_prob,
	                                     spnMatrix=inputs$spn_mov_prob)
	      BEND_N[indx_N]<- adultMovement(previousLocation=BEND_N[indx_N],
	                                     month=m[i],
	                                     spn=SPN_N[indx_N],
	                                     fromToMatrix=inputs$adult_mov_prob,
	                                     spnMatrix=inputs$spn_mov_prob)
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
	      
	      ### UPDATE LIVING FISH WITHIN BASIN
	      indxH<-indx_H
	      indxN<-indx_N
	      if(inputs$migration)
	      {
	        indxH<-indx_H[which(BEND_H[indx_H] %in% 1:inputs$n_bends),]
	        indxN<-indx_N[which(BEND_N[indx_N] %in% 1:inputs$n_bends),]
	      }
	      WGT_H[indxH]<-dWeight(len=LEN_H[indxH],
	                             a=inputs$a,
	                             b=inputs$b,
	                             er=inputs$lw_er)
	      WGT_N[indxN]<-dWeight(len=LEN_N[indxN],
	                             a=inputs$a,
	                             b=inputs$b,
	                             er=inputs$lw_er)
	    }
	    
	    ## END POPULATION DYNAMICS
	    
	    ## SUMMARIES 
	    ### ABUNDANCE AGE-1+
	    if(inputs$migration)
	    {
	      inBasinH<-sapply(1:inputs$nreps, function(x)
	      {
	        out<-ifelse(BEND_H[,x] %in% 1:inputs$n_bends, 1, 0)
	        return(out)
	      })
	      inBasinN<-sapply(1:inputs$nreps, function(x)
	      {
	        out<-ifelse(BEND_N[,x] %in% 1:inputs$n_bends, 1, 0)
	        return(out)
	      }) 
	    }
	    N_NAT[i,]<-colSums(Z_N*inBasinN) 
	    N_HAT[i,]<-colSums(Z_H*inBasinH) 
	    
	    if(weightCalc)
	    {
	      ### BIOMASS IN KILOGRAMS
	      BIOMASS_N[i,]<- colSums(WGT_N*inBasinN)/1000
	      BIOMASS_H[i,]<- colSums(WGT_H*inBasinH)/1000
	      BIOMASS[i,]<- (colSums(WGT_H*inBasinH)+colSums(WGT_N*inBasinN))/1000 
	        ## THIS CAN BE CALCULATED FROM BIOMASS_N AND BIOMASS_H
	    
	      ### MEAN WEIGHT IN KILOGRAMS
	      MEANWEIGHT_N[i,]<- (colSums(WGT_N*inBasinN)/colSums(Z_N*inBasinN))/1000 
	      MEANWEIGHT_H[i,]<- (colSums(WGT_H*inBasinH)/colSums(Z_H*inBasinH))/1000 
	      MEANWEIGHT[i,]<-((colSums(WGT_H*inBasinH)+colSums(WGT_N*inBasinN))/(colSums(Z_H*inBasinH)+colSums(Z_N*inBasinN)))/1000 
	      ## THIS CAN BE CALCULATED FROM MEANWEIGHT_N, MEANWEIGHT_H, 
	      ##    N_NAT, AND N_HAT:
	      ## (MEANWEIGHT_N*N_NAT+MEANWEIGHT_H*N_HAT)/(N_NAT+N_HAT) 
	    }
	    
	    ### MEAN LENGTH IN MM
	    MEANLENGTH_N[i,]<- colSums(LEN_N*inBasinN)/colSums(Z_N*inBasinN)	
	    MEANLENGTH_H[i,]<- colSums(LEN_H*inBasinH)/colSums(Z_H*inBasinH)
	    MEANLENGTH[i,]<-(colSums(LEN_H*inBasinH)+ colSums(LEN_N*inBasinN))/(colSums(Z_H*inBasinH)+colSums(Z_N*inBasinN))
	    ## THIS CAN BE CALCULATED FROM MEANLENGTH_N, MEANLENGTH_H, 
	    ##    N_NAT, AND N_HAT:
	    ## (MEANLENGTH_N*N_NAT+MEANLENGTH_H*N_HAT)/(N_NAT+N_HAT) 
	    
	    ### AGE-1 RECRUITS; NATURAL ORIGIN
	    indx_R<- lapply(1:inputs$nreps,function(x)
	    {
	      if(!inputs$migration)
	      {
	        out<- which(AGE_N[,x]>0 & AGE_N[,x]<24 & Z_N[,x]==1)
	      }
	      if(inputs$migration)
	      {
	        out<- which(AGE_N[,x]>0 & AGE_N[,x]<24 & Z_N[,x]==1 & inBasinN[,x]==1)
	      }
	    }) 		
	    RECRUITS[i,]<- sapply(1:inputs$nreps,
	                          function(x)
	                          { 
	                            length(indx_R[[x]])
	                          })
	    
	    if(inputs$genetics & m[i]==12)
	    {
	      ### UPDATE YEAR COUNT
	      indxY<-indxY+1
	      ### ZERO OUT FISH THAT DIED
	      MAT_H<-MAT_H*Z_H 
	      ### CALCULATE EFFECTIVE POPULATION SIZE
	      EffectivePop<-lapply(1:inputs$nreps, function(x)
	      {
	        tmp<-aggregate(MAT_H[,x]*inBasinH[,x], by=list(id=MOM_H[,x]), sum)
	        if(any(tmp$id=="0"))
	        {
	          tmp<- tmp[-which(tmp$id=="0"),]
	        }
	        mu_f <- mean(tmp[,2])
	        V_f <- var(tmp[,2])
	        N_f <- nrow(tmp)
	        out <- (N_f*mu_f-1)/(mu_f-1+V_f/mu_f) #Saltzgiver Eqn. 4
	        return(data.frame(N_ef=out, rep=x))
	      })
	      EffectivePop<-do.call(rbind, EffectivePop)
	      
	      N_em<-lapply(1:inputs$nreps, function(x)
	      {
	        tmp<-aggregate(MAT_H[,x]*inBasinH[,x], by=list(id=DAD_H[,x]), sum)
	        if(any(tmp$id=="0"))
	        {
	          tmp<- tmp[-which(tmp$id=="0"),]
	        }
	        mu_m <- mean(tmp[,2])
	        V_m <- var(tmp[,2])
	        N_m <- nrow(tmp)
	        out <- (N_m*mu_m-1)/(mu_m-1+V_m/mu_m) #Saltzgiver Eqn. 5
	        return(data.frame(N_em=out, rep=x))
	      })
	      N_em<-do.call(rbind, N_em)
	      EffectivePop<-merge(EffectivePop, N_em, by="rep")
	      EffectivePop$N_e <- 4*EffectivePop$N_ef*EffectivePop$N_em/
	        (EffectivePop$N_ef+EffectivePop$N_em)  #Saltzgiver Eqn. 6
	      EffectivePop<-EffectivePop[order(EffectivePop$rep),]
	      N_e[indxY,]<-EffectivePop$N_e
	    }
	    
	    ### SIZE STRUCTURE
	    #### NOT UPDATED
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
	  if(inputs$genetics)
	  {
	    out$N_e <- N_e
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
