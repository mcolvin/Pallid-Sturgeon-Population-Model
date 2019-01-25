

	
# DYNAMICS PLUGINS
## [1] MONTHLY SURVIVAL
dSurvival<- function(phi_age,age,maxAge)
{
  # phi_age: vector of age specific survival from age 1 to maxAge
  # age: vector of age in months for live individuals
  phi<- phi_age^(1/12)# convert annual to monthly
  a<- floor(age/12)
    #WE SHOULD NOT HAVE ANY AGE ZERO FISH IN THE VECTOR FOR WHICH THIS
    #FUNCTION IS APPLIED IS USED
  if(any(a==0)){return(print("Age-0 Fish Present in Vector."))}
  out<- rbinom(length(a),1,phi[a])
  out<- ifelse(age+out>maxAge*12,0,out) 
    #FISH OLDER THAN MAXAGE ARE REMOVED FROM THE POPULATION
  return(out)
}

## [3] CHANGE IN LENGTH 
dLength<- function(k, linf,length1,dT)
{# FABENS MODEL WITH MODFICATION	
  # k    growth coefficient
  # linf    length at infinity
  # length1    length at t-dt
  # dT    change in time
  length2<-((linf-length1)*(1-exp(-k*dT))+ length1) 
  return(length2)# return the predicted length
}


## [4] CHANGE IN WEIGHT
dWeight<- function(len,a=0.0001,b=3,er=0.1)
{
  ypred<- log(a)+b*log(len)
  out<-exp(rnorm(length(len),ypred,er))
  return(out)
}

## [6] CHANGE IN AGE


## [7] MATURATION OF LIVING FISH
dMaturity<- function(mature, age, live, mat_dist)
{
  # RETURNS A LIST OF 2 VECTORS OF 0'S AND 1'S:
  ## mature (1 if mature and 0 if not)
  ## FirstSpawn (1 if the fish just matured, i.e. first spawning, and 0 otherwise)
  a <- floor(age/12)
    #WE SHOULD NOT HAVE ANY FISH GREATER THAN MAXAGE 
    #IN THE VECTOR FOR WHICH THIS FUNCTION IS APPLIED
  if(any(a>inputs$maxage)){return(print("Senescent Fish Present in Vector."))}
  mat_dist<-c(0, mat_dist) #ADD IN ZERO MATURATIONS FOR AGE-0 FISH
  pr<-mat_dist[a+1]*live
  indx<-which(mature==1)
  pr[indx]<-1
  M2<- rbinom(length(age),1,pr)
  FS<-ifelse(M2-mature==1,1,0)
  return(list(mature=M2, FirstSpawn=FS))
}

## [8] TIME IN MONTHS SINCE SPAWNING
dMPS<- function(mps,spawn,mature)
{
  (mps*(1-spawn)+12)*mature
}


## [9] SPAWNING
spawn<- function(mps,a=-5,b=2.55,mature,FirstSpawn)
{
  # FUNCTION RETURNING A 1 IF A FISH SPAWNS  
  ## THIS CAN BE THOUGHT OF AS THE FEMALE PROBABILITY, 
  ## AS ONLY THE FEMALES THAT SPAWN ADD TO THE POPULATION,
  ## AND THE NUMBER OF MALES IS ARBITRARY AND ASSUMED TO BE ENOUGH
  pr<- plogis(a+b*mps/12) 
  # IF WE ARE CAPPING AT 4 THEN:
  # pr[which(ceiling(mps/12)==4)]<-1
  pr[which(FirstSpawn==1)]<-1
  out<- rbinom(length(mps),1, pr*mature)
  return(out)	
}


## [10] LOCATION
loc2<- function(loc1, # LOCATIONS OF INDIVIDUALS
                dist, # MOVEMENT DISTRIBUTION
                er,   # SD OF DISTRIBUTION 
                bend_lengths, # VECTOR BEND LENGTHS IN RKMs 
                month, 
                spn_months,   # VECTOR OF MONTHS WHEN SPAWNERS MOVE
                spn)  # INDIVIDUAL SPAWNING STATUS
{
  library(truncnorm)
  # FUNCTION TO SIMULATE MOVEMENT FROM ONE MONTH TO THE NEXT
  # INDIVIDUALS W/O A LOCATION NEED CANNOT BE INPUT TO THIS FUNCTION
  ## TRANSLATE BEND NUMBER TO RKM
  vals<-c(0, cumsum(bend_lengths), Inf)
  loc1<-runif(1, vals[i], vals[i+1])
  ## UPDATE EACH INDIVIDUALS RKM 
  #out<- rnorm(length(loc1),loc1,er)
  out<-rtruncnorm(length(loc1), 0, sum(bend_lengths), loc1, er)
    # CAN ADJUST BASED ON PROBABILITY OF LEAVING
  ## UPDATE SPAWNING FISH LOCATION
  if(month %in% SpawningMovMonths)
  {
    indx<-which(spn==1)
    out[indx]<-SpnSite1
  }
  out<-sapply(out, function(x){return(min(which(vals>x))-1)})
  out<-ifelse(out==0, "DS",
              ifelse(out==length(bend_lengths)+1, "T", out))
  return(out)
}	


adultMovement<- function(previousLocation=NULL, 
                         month=NULL, 
                         spn=NULL,
                         fromToMatrix=NULL, 
                         spnMatrix=NULL)
{
  ## MONTHS WITHOUT SPAWNING MOVEMENT
  if(!(month %in% c(6))) #TO EXPAND NEED TO CHANGE WHEN SPAWNING IS UPDATED
  {
    out<-lapply(unique(previousLocation) ,function(x)
    {
      indx<- which(previousLocation==x)
      new<-sample(1:ncol(fromToMatrix),
                  length(indx),
                  prob=fromToMatrix[x,],
                  replace=TRUE)
      return(cbind(indx,new))
    })
    out<-do.call("rbind",out)# convert list to dataframe
    out<- out[order(out[,1]),]
  }
  if(month %in% c(6)) #TO EXPAND NEED TO CHANGE WHEN SPAWNING IS UPDATED
  {
    indxS<-which(spn==1)
    locS<-previousLocation[indxS]
    locNS<-previousLocation[-indxS]
    outNS<-lapply(unique(locNS) ,function(x)
    {
      indx<- which(previousLocation==x & spn==0)
      new<-sample(1:ncol(fromToMatrix),
                  length(indx),
                  prob=fromToMatrix[x,],
                  replace=TRUE)
      return(cbind(indx,new))
    })
    outNS<-do.call("rbind",outNS)# convert list to dataframe
    outS<-lapply(unique(locS) ,function(x)
    {
      indx<- which(previousLocation==x & spn==1)
      new<-sample(1:ncol(spnMatrix),
                  length(indx),
                  prob=spnMatrix[x,],
                  replace=TRUE)
      return(cbind(indx,new))
    })
    outS<-do.call("rbind",outS)# convert list to dataframe
    out<-rbind(outNS, outS)
    out<- out[order(out[,1]),]
  }
  return(out[,2])
}


freeEmbryoDrift<- function(bendAbund=NULL, 
                           driftMatrix=NULL)
{
  ## ERROR CHECK
  if(nrow(driftMatrix)!=nrow(bendAbund) 
     | ncol(driftMatrix)!=(ncol(bendAbund)+1))
  {
    return(print("driftMatrix must have 1 row for every bend, 1 column
                 for every bend, plus a last additional column with the 
                 probability of drifting out of the basin."))
  }
  indx<-which(rowSums(bendAbund)>0)
  out<-lapply(indx, function(x)
  {
    new<-apply(bendAbund[x,,drop=FALSE], 2, 
               rmultinom, n=1, 
               prob=driftMatrix[x,])
    return(new)
  })
  out<-Reduce('+', out)# combine drifting recruits from each spawning location
  return(out[-nrow(out),])
}



fecundity<- function(fl,a,b,er,sex,spawn,mature)
{
  N<-length(fl)
  fl_normalized<- (fl - 1260.167)/277.404
  eggs<- rpois(N,exp(rnorm(N,a + b*fl_normalized,er)))*spawn*sex*mature
  return(eggs)
}



# #PARAMETERS OF LOG LAMBDA~N(a+b*fl_normalized, er)
# a<-
# b<-
# er<-
# #ASSUME FL IS NORMALY DISTRIBUTED (SINCE USING fl_normalized mu=0 and sigma=1?)
# mu<-0
# sigma<-1
# Umin<-(Lmin-sigma^2*b-mu)/sigma
# Umax<-(Lmax-sigma^2*b-mu)/sigma
# ExEggs<-exp((sigma^2*b^2+2*b*mu+2*a+er^2)/2)/
#   (pnorm(Lmax, mu,  sigma)-pnorm(Lmin, mu,  sigma))*
#   (pnorm(Umax)-pnorm(Umin))



### THESE PLUGINS ACCEPT MATRICES AND AN
### INDEX [x], INDEXING INDVIDUAL REPLICATES
### THESE ARE INTENDED TO BE FLEXIBLE AND THE 
### ACTUAL FUNCTIONS CAN BE SWAPPED OR MODIFIED
### BY USERS
dFreeEmbryoDrift<- function(x,nbends,loc,prob)
	{
	# EMBRYO DRIFT 
	indx<-x
	ppp<-sapply(1:nbends, function(x)
	  {
		  ppp<-rmultinom(1, loc[x,indx],prob[x,])
	  })
	ppp<-rowSums(ppp)
	}
	
dFEtoEFL<- function(x,n,total,phi)
	{
	tmp<-rbinom(n,total[,x],phi)
	return(tmp)
	}	



	