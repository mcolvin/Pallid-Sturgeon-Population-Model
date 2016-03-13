
### HELPER FUNCTIONS

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
ini_age<- function(x,rel_age,maxage,n,fill0)
	{
	c(sort(sample(c(1:maxage),n,prob=rel_age,replace=TRUE)),
		rep(0,fill0))
	}
ini_length<-function(x,linf,k,t0,age,live)
	{
	(linf*(1-exp(-k*(age[,x]-t0))))*live[,x] #input$vb_er
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
dLength<- function(x,n, k, linf,length1,dT,er,live)
	{# FABENS MODEL WITH MODFICATION
	rlnorm(n,log((linf-length1[,x]*(1-exp(-k*dT))+ length1[,x]) ),er)*live[,x] # rlnorm is slow AF here
	#(linf-length1[,x]*(1-exp(-k*dT))+ length1[,x]) *live[,x]
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
	rlnorm(1,log(a*x^b),er)
	}
dMaturity<- function(maturity,mat_k,age,age_mat,live)
	{
	y<- -mat_k*(age-age_mat)
	y<- -input$mat_k*(indData$age-input$age_mat)
	M2<- rbinom(length(y),1,1/(1+exp(y))*live)
	return(M2)
	}
	
spawn<- function(x,mps,a=-17.5,b=0.35,mature,live)
	{
	y<- rbinom(length(mps[,x]),1,plogis(a+b*mps[,x]))*mature[,x]*live[,x] 
	return(y)	
	}
fecundity<- function(x,len,wgt,a,b,er,sex,live,spawn)
	{
	y<-rlnorm(length(len[,x]),log(a*len[,x]^b),er)*spawn[,x]*sex[,x]
	eggs<-rpois(length(len[,x]),y)
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
	
