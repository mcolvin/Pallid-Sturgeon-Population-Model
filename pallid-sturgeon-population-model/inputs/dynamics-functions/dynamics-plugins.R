dSurvival<- function(x,phi_age,age)
	{
	# phi_age: vector of age specific survival
	# age: vector of age in months for live individuals
	phi<- c(phi_age^(1/12))# convert annual to monthly, add 0 to zero out survival of unalive fish
	a<- floor(age/12)
	out<- rbinom(length(a),1,phi[a])
	return(out)
	}


	
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
	
dWeight<- function(len,a=0.0001,b=3,er=0.1)
	{
	#out<-rlnorm(length(len),log(a*len^b),er)
	out<-exp(rnorm(length(len),a+b*len,er))
	return(out)
	}
	
dWeight_v<- function(x,a=0.0001,b=3,er=0.1)
	{
	rlnorm(1,log(a*x^b),er) ####fixme####
	}
	
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
	y<- exp(a + b * ((fl - 1260.167)/277.404)+rnorm(length(fl),0,er))
	eggs<- rpois(length(fl),y)*spawn*sex*mature
	return(eggs) 
	}
	
loc2<- function(loc1,er,month)
	{
	# FUNCTION TO SIMULATE MOVEMENT FROM ONE MONTH TO THE NEXT
	## LOC1 IS LOCATIONS OF INDIVIDUALS
	out<- rnorm(length(loc1),loc1,er)
	return(out)
	}	

	