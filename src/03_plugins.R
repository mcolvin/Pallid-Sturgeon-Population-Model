
	
##### PLUGINS

### HELPER FUNCTIONS

### FUNCTION TO INIITIALIZE LENGTH DISTRIBUTION
lower_len_init<- approxfun(len_init[len_init$basin=="lower",]$x,len_init[len_init$basin=="lower",]$len)
upper_len_init<- approxfun(len_init[len_init$basin=="upper",]$x,len_init[len_init$basin=="upper",]$len)
	
#### FUNCTIONS TO ASSIGN RKM TO BENDS FOR UPPER AND LOWER
rkm_start<-seq(1,1000,length=317)
bend<- c(1:317)
rkm2bend<- approxfun(rkm_start,bend,method='constant')
bend2rkm<- approxfun(bend,rkm_start,method='constant')


## INITIALIZATION PLUGINS
ini_mps<- function(x,n,mature,live)
	{# months since spawning
	sample(c(1:4),size=n,replace=TRUE)*mature[,x]*live[,x]*12
	}
ini_wgt<- function(x,a,b,len,live)
	{
	(a*len[,x]^b)*live[,x]
	}
ini_sex<- function(x,n,ratio,fill0){
	c(rbinom(n,1,ratio),rep(0,fill0))
	}
ini_Z<- function(x,n,fill0)
	{
	c(rep(1,n),rep(0,fill0))
	}
	
ini_age<- function(x,n,len,linf,k,sizeAtHatch=5,fill0)
	{
	length2<- len[1:n,x]
	length1<-rep(sizeAtHatch,n)
	linf<- linf[1:n,x]
	k<- k[1:n,x]
	age<-sapply(1:n,function(x)
		{solve(-k[x],log(1-((length2[x]-length1[x])/(linf[x]-length1[x]))))})
	return(c(age,rep(0,fill0)))	
	}
	
ini_length<-function(x,n,linf,fill0,basin)
	{
	if(basin=="lower")
		{
		tmp<-lower_len_init(runif(n))
		}
	if(basin=="upper")
		{
		tmp<-upper_len_init(runif(n))
		}
	tmp<- ifelse(tmp> linf[1:n,x],0.9*linf[1:n,x],tmp)
	return(c(tmp,rep(0,fill0)))
	}
ini_maturity<- function(x,k,len,age_mat,live)
	{
	p<- (1/(1+exp(-k*(len[,x]-age_mat))))*live[,x]
	M2<- rbinom(length(p),1,p)		
	return(M2)				
	}
ini_rkm<- function(x,n,fill0)
	{
	# FUNCTION TO INITIALIZE RIVER KILOMETER FOR 
	# INVIDUAL FISH
	c(runif(n,1,1000),rep(0,fill0))
	}
ini_growth<- function(x,n,basin)
	{
	if(basin=="lower")
		{
		tmp<-mvrnorm(x*n,c(6.982160, -2.382711),
			matrix(c(0.0894,-0.1327,-0.1327,0.3179),2,2,byrow=TRUE))
		tmp<- exp(tmp)
		}
	if(basin=="upper")
		{
		tmp<-mvrnorm(x*n,c(7.136028770, -3.003764445),
			matrix(c(0.2768,-0.364,-0.364,0.6342),2,2,byrow=TRUE))
		tmp<- exp(tmp)
		}
	return(list(linf=matrix(tmp[,1],n,x),k=matrix(tmp[,2],n,x)))
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
	
dSurvival<- function(x,n,phi_age,age,live)
	{
	phi<- c(0,phi_age)^(1/12)
	rbinom(n,1,phi[age[,x]+1])*live[,x]
	}
dLength<- function(x, n, k, linf,length1,dT,live)
	{# FABENS MODEL WITH MODFICATION	
	# x    number of replicates, column index used by sapply 
	# n    number in super population
	# k    growth coefficient
	# linf    length at infinity
	# length1    length at t-dt
	# dT    change in time
	# live    is the fish alive or dead?
	length2<-((linf[,x]-length1[,x])*(1-exp(-k[,x]*dT))+ 
		length1[,x]) * live[,x]
	return(length2)# return the predicted length
	}

dMPS<- function(x,mps,mature,live) 
	{
	(mps[,x]+1)*mature[,x]*live[,x]
	}
dWeight<- function(x,n,len,a=0.0001,b=3,er=0.1,live)
	{
	rlnorm(n,log(a*len[,x]^b),er)*live[,x]
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
	
spawn<- function(x,mps,a=-17.5,b=0.35,mature,live)
	{
	y<- rbinom(length(mps[,x]),1,plogis(a+b*mps[,x]))*mature[,x]*live[,x] 
	return(y)	
	}
fecundity<- function(x,fl,a,b,er,sex,live,spawn)
	{
	y<- exp(a + b * ((fl - 1260.167)/277.404)+rnorm(nrow(fl),0,er))
	eggs<- rpois(nrow(fl),y)*spawn[,x]*sex[,x]
	return(eggs) 
	}
dRKM<- function(x, n,loc,live,er)
	{
	# FUNCTION TO SIMULATE MOVEMENT 
	# FROM ONE MONTH TO THE NEXT
	# LOC AND LIVE ARE MATRICES [NINDS,NREPS]
	# INPUTS IS A VECTOR OF INDVIDUALS APPLIED OVER REPS
	# X IS THE COLUMN INDEX FOR EACH REPLICATE
	(loc[,x]+rnorm(n,loc[,x],er))*live[,x]
	}	

	