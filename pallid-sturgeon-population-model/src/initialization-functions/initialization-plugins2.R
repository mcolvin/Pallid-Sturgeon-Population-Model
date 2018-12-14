
#### FUNCTIONS TO ASSIGN RKM TO BENDS FOR UPPER AND LOWER
#rkm_start<-seq(1,1000,length=317)
#bend<- c(1:317)
#rkm2bend<- approxfun(rkm_start,bend,method='constant')
#bend2rkm<- approxfun(bend,rkm_start,method='constant')


# INITIALIZATION PLUGINS
##[2] INITIALIZE GROWTH PARAMETERS (L_INF, K)	
ini_growth<- function(n,mu_ln_Linf,mu_ln_k,vcv, maxLinf=2100)
{
  ln_B<-eigen(vcv)$vectors%*%matrix(c(sqrt(eigen(vcv)$values[1]),0,
                                      0,sqrt(eigen(vcv)$values[2]))
                                    ,2,2)
  library(truncnorm)
  z1<-rtruncnorm(n, -sqrt(-2*log(0.2)), sqrt(-2*log(0.2)), 
                 mean=0, sd=1)
  z2<-rtruncnorm(n, a=-sqrt(-2*log(0.2)-z1^2), 
                 b=sqrt(-2*log(0.2)-z1^2), 
                 mean=0, sd=1)
  Z<-matrix(c(z1, z2), 2, n, byrow = TRUE)
  X<-t(ln_B%*%Z+c(mu_ln_Linf,mu_ln_k))
  X<-exp(X)
  #POTENTIALLY REMOVE maxLinf CODE EXTREMES
  return(list(linf=X[,1],k=X[,2]))
}


##[3] LENGTH
ini_length<-function(n, basin, origin, spatial=FALSE)
{# A FUNCTION TO INITIALIZE LENGTHS OF INDIVIDUAL FISH
  # origin [0 for natural, 1 for hatchery]
  # CALLS r WHICH IS AN EMPRICAL DISTRIBUTION
  if(tolower(basin)=="lower" & spatial==FALSE)
  {
    tmp<- len_ini_low_hatchery_nospace(runif(n))*origin + # hatchery
      len_ini_low_natural_nospace(runif(n))*(1-origin) # natural
  }
  if(tolower(basin)=="upper" & spatial==FALSE)
  {
    tmp<- len_ini_upp_hatchery_nospace(runif(n))*origin + # hatchery
      len_ini_upp_natural_nospace(runif(n))*(1-origin) # natural
  }
  # SPATIAL COMPONENT
  if(tolower(basin)=="lower" & spatial==TRUE )
  {####fixme####
    tmp<- len_ini_low_hatchery_spatial(runif(n))*origin + # hatchery
      len_ini_low_natural_spatial(runif(n))*(1-origin) # natural				
  }
  if(tolower(basin)=="upper" & spatial==TRUE )
  {####fixme####
    tmp<- len_ini_upp_hatchery_spatial(runif(n))*origin + # hatchery
      len_ini_upp_natural_spatial(runif(n))*(1-origin) # natural
  }
  return(tmp)
}


##[4] WEIGHT
ini_wgt<- function(a,b,len,er)
{
  ypred<- log(a)+b*log(len)
  out<-exp(rnorm(length(len),ypred,er))
  return(out)
}


##[5] SEX
ini_sex<- function(n,ratio)
{
  out<-rbinom(n,1,ratio)
  return(out)
}


##[6] AGE IN MONTHS 
ini_age<- function(len,linf,k,sizeAtHatch=7,maxAge)
{
  age<-ifelse(len==0,0,log(-1*(len-sizeAtHatch)/(linf-sizeAtHatch)+1)/-k)
  age<- ifelse(age>maxAge,maxAge,age)
  return(age*12)	
}


##[7] INITIALIZE WHICH FISH ARE MATURE
ini_maturity<- function(age, mat_cdf)  ##INCLUDE LENGTH IN FUTURE? DO LARGER YOUNGER FISH MATURE FIRST?
{
  # IS A FISH SEXUALLY MATURE; CONDITIONAL ON BEING ALIVE
  a<-floor(age/12) #AGE IN YEARS
  mat_cdf<-c(0, mat_cdf) #ADD IN ZERO MATURATIONS FOR AGE-0 FISH
  pr<-mat_cdf[a+1]
  M2<- rbinom(length(a),1,pr) #GIVES A 1 FOR MATURE FISH
  # WHICH OF THE MATURE FISH ARE FIRST SPAWNERS?
  pr<-sapply(a, FUN=function(x)
    {
      ifelse(mat_cdf[x+1]==0,0,(mat_cdf[x+1]-mat_cdf[x])/mat_cdf[x+1])
    })
  FS<-rbinom(length(M2), 1, pr) #GIVES A 1 IF THE FISH JUST BECAME MATURE
  return(list(mature=M2, FirstSpawn=FS))				
}


##[8] INITIAL TIME SINCE SPAWNING
ini_mps<- function(n, mature, FirstSpawn)
	{# months since spawning
	out<-sample(c(1:4),size=n,replace=TRUE)*mature*12
	out[which(FirstSpawn==1)]<-0
	return(out)
}


##[10] INITIALIZE RIVER LOCATION
initialize_spatial_location<- function(n,nbends,relativeDensity)
{
  x<- sample(c(1:nbends),n,relativeDensity,replace=TRUE)
  return(x)
}