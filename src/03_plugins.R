
	
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
ini_mps<- function(n,mature)
	{# months since spawning
	out<-sample(c(1:4),size=n,replace=TRUE)*mature*12
	return(out)
	}
	
	
ini_wgt<- function(a,b,len,er)
	{
	rlnorm(length(len),log(a*len^b),er)
	}
	
	
ini_sex<- function(n,ratio)
	{
	out<-rbinom(n,1,ratio)
	return(out)
	}

	
ini_age<- function(len,linf,k,sizeAtHatch=7,maxAge)
	{
	length2<- len
	length1<-rep(sizeAtHatch,length(len))
	age<-sapply(1:length(len),function(x)
		{solve(-k[x],log(1-((length2[x]-length1[x])/(linf[x]-length1[x]))))})
	out<- ifelse(age>maxAge,maxAge,age)
	return(out*12)	
	}


# FUNCTION TO INIITIALIZE LENGTH OF FISH	
ini_length<-function(linf,basin)
	{
	if(basin=="Lower")
		{
		tmp<-lower_len_init(runif(length(linf)))
		}
	if(basin=="Upper")
		{
		tmp<-upper_len_init(runif(length(linf)))
		}
	tmp<- ifelse(tmp> linf,0.9*linf,tmp)
	return(tmp)
	}
	

# INITIALIZE HOW MANY FISH ARE MATURE
ini_maturity<- function(k,len,age_mat)
	{
	# IS A FISH SEXUALLY MATURE; CONDITIONAL ON BEING ALIVE
	p<- (1/(1+exp(-k*len-age_mat*12))) ####fixme####
	M2<- rbinom(length(len),1,p)	
	return(M2)				
	}
	
# INITIALIZE RIVER LOCATION
ini_rkm<- function(n,type,bend_lengths)
	{
	# FUNCTION TO INITIALIZE RIVER 
	# KILOMETER FOR INVIDUAL FISH
	nbends<- length(bend_lengths)
	# BUILD INVERSE DISTRIBUTATION TO SAMPLE FROM
	if(type=="Uniform")
		{
		y<- rep(1, nbends)*bend_lengths
		y<- cumsum(y)/sum(y)
		cumdist<- approxfun(y,c(1:nbends),rule=2)
		}
	if(type=="Emperical")  ####fixme####
		{
		y<- rep(1, nbends)*bend_lengths
		y<- cumsum(y)/sum(y)
		cumdist<- approxfun(y,c(1:nbends),rule=2)
		}
	x<-cumdist(runif(n))
	return(x)
	}
	
# INITIALIZE GROWTH PARAMETERS (L_INF, K)	
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

## STOCKING FUNCTIONS



	
	
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
	
dSurvival<- function(phi_age,age)
	{
	# phi_age: vector of age specific survival
	# age: vector of age in months for live individuals
	phi<- phi_age^(1/12)# convert annual to monthly
	a<- floor(age/12)
	out<- rbinom(length(a),1,phi[a])
	return(out)
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

dMPS<- function(x,mps,mature,live) 
	{
	(mps[,x]+1)*mature[,x]*live[,x]
	}
	
dWeight<- function(len,a=0.0001,b=3,er=0.1)
	{
	out<-rlnorm(length(len),log(a*len^b),er)
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
	
spawn<- function(mps,a=-17.5,b=0.35,mature)
	{
	# FUNCTION RETURNING A 1 IF A FISH SPAWNS IN THE 
	# NEXT YEAR
	pr<- plogis(a+b*mps)*mature
	out<- rbinom(length(mps),1, pr)
	return(out)	
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

	