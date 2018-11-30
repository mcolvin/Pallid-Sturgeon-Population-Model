

	
# DYNAMICS PLUGINS
## [1] MONTHLY SURVIVAL
dSurvival<- function(phi_age,age,maxAge)
{
  # phi_age: vector of age specific survival from age 1 to maxAge
  # age: vector of age in months for live individuals
  phi<- c(0,phi_age^(1/12))# convert annual to monthly, add age-0 survival
  a<- floor(age/12)
  out<- rbinom(length(a),1,phi[a+1])
  #out<- ifelse(a+out>maxAge,0,out) #SENESCENCE, WHAT DO WE WANT TO TRACK, MAY NEED TO CHANGE???
  #MAY NEED TO ZERO OUT UNALIVE FISH W/ AGE ZERO... PERHAPS USE AGE???
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


## [7] MATURATION
dMaturity<- function(mature, age, live, mat_dist)
{
  # RETURNS A LIST OF 2 VECTORS OF 0'S AND 1'S:
  ## mature (1 if mature and 0 if not)
  ## FirstSpawn (1 if the fish just matured, i.e. first spawning, and 0 otherwise)
  indx<-which(mature==1)
  pr<-mat_dist[age]
  pr[indx]<-1
  M2<- rbinom(length(age),1,pr*live)
  FS<-ifelse(M2-mature==1,1,0)
  return(list(mature=M2, FirstSpawn=FS))
}

## [8] TIME IN MONTHS SINCE SPAWNING
dMPS<- function(mps,spawn,mature)
{
  (mps*(1-spawn)+12)*mature*live
}
#dMPS<- function(x,mps,mature,live) 
#	{
#	(mps[,x]+1)*mature[,x]*live[,x]
#	}


## [9] SPAWNING
spawn<- function(mps,a=-5,b=2.55,mature,FirstSpawn)
{
  # FUNCTION RETURNING A 1 IF A FISH SPAWNS
  pr<- plogis(a+b*mps/12)
  # IF WE ARE CAPPING AT 4 THEN:
  # pr[which(ceiling(mps/12)==4)]<-1
  pr[which(FirstSpawn==1)]<-1
  out<- rbinom(length(mps),1, pr*mature)
  return(out)	
}


## [10] LOCATION
loc2<- function(loc1,er,month)
{
  # FUNCTION TO SIMULATE MOVEMENT FROM ONE MONTH TO THE NEXT
  ## LOC1 IS LOCATIONS OF INDIVIDUALS
  out<- rnorm(length(loc1),loc1,er)
  return(out)
}	


adultMovement<- function(previousLocation,fromToMatrix)
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
  return(out[,2])
}



fecundity<- function(fl,a,b,er,sex,spawn,mature)
{
  N<-length(fl)
  fl_normalized<- (fl - 1260.167)/277.404
  #yy<- exp(a + b*fl_normalized) + exp(rnorm(N,0,er))
  #y<- exp(rnorm(N,a + b*fl_normalized,er))
  #eggs<- rpois(N,y)*spawn*sex*mature
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



	