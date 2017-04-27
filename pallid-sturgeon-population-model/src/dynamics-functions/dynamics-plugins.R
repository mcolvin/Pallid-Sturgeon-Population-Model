

	
## DYNAMICS PLUGINS
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
	

	
dLength<- function(k, linf,length1,dT)
	{# FABENS MODEL WITH MODFICATION	
	# k    growth coefficient
	# linf    length at infinity
	# length1    length at t-dt
	# dT    change in time
	length2<-((linf-length1)*(1-exp(-k*dT))+ length1) 
	return(length2)# return the predicted length
	}

#dMPS<- function(x,mps,mature,live) 
#	{
#	(mps[,x]+1)*mature[,x]*live[,x]
#	}
	

	
dMaturity<- function(maturity,mat_k,age,age_mat,live)
	{
	y<- -mat_k*(age-age_mat)
	M2<- rbinom(length(y),1,1/(1+exp(y))*live)
	return(M2)
	}
	
spawn<- function(mps,a=-5,b=2.55,mature)
	{
	# FUNCTION RETURNING A 1 IF A FISH SPAWNS IN THE 
	# NEXT YEAR
	pr<- plogis(a+b*mps/12)*mature
	out<- rbinom(length(mps),1, pr)
	return(out)	
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
	
loc2<- function(loc1,er,month)
	{
	# FUNCTION TO SIMULATE MOVEMENT FROM ONE MONTH TO THE NEXT
	## LOC1 IS LOCATIONS OF INDIVIDUALS
	out<- rnorm(length(loc1),loc1,er)
	return(out)
	}	

	