
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
ini_length<-function(n=10, basin="lower",origin=1, spatial=FALSE,linf=2000)
{# A FUNCTION TO INITIALIZE LENGTHS OF INDIVIDUAL FISH
  # origin [0 for natural, 1 for hatchery]
  # CALLS r WHICH IS AN EMPRICAL DISTRIBUTION
  if(tolower(basin)=="lower" & spatial==FALSE)
  {
    tmp<- r(len_ini_low_hatchery_nospace)(n)*origin + # hatchery
      r(len_ini_low_natural_nospace)(n)*(1-origin) # natural
  }
  if(tolower(basin)=="upper" & spatial==FALSE)
  {
    tmp<- r(len_ini_upp_hatchery_nospace)(n)*origin + # hatchery
      r(len_ini_upp_natural_nospace)(n)*(1-origin) # natural
  }
  # SPATIAL COMPONENT
  if(tolower(basin)=="lower" & spatial==TRUE )
  {####fixme####
    tmp<- r(len_ini_low_hatchery_nospace)(n)*origin + # hatchery
      r(len_ini_low_natural_nospace)(n)*(1-origin) # natural				
    
    
    # tmp<- r(len_ini_low_hatchery_nospace)(n)*origin + # hatchery
    #        r(len_ini_low_natural_nospace)(n)*(1-origin) # natural
  }
  if(tolower(basin)=="upper" & spatial==TRUE )
  {####fixme####
    tmp<- r(len_ini_upp_hatchery_nospace)(n)*origin + # hatchery
      r(len_ini_upp_natural_nospace)(n)*(1-origin) # natural
    
    #tmp<-  r(len_ini_upp_hatchery_nospace)(n)*origin + # hatchery
    #       r(len_ini_upp_natural_nospace)(n)*(1-origin) # natural
  }
  tmp<- ifelse(tmp> linf,0.99*linf,tmp)
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


##[6] AGE 
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
  pr<-mat_cdf[age] #BE SURE AGE IS IN THE CORRECT FORM
  M2<- rbinom(length(age),1,pr) #GIVES A 1 FOR MATURE FISH
  # WHICH OF THE MATURE FISH ARE FIRST SPAWNERS?
  pr<-ifelse(mat_cdf[age]==0, 0, 
             M2*(mat_cdf[age]-mat_cdf[age-1])/mat_cdf[age])
  FS<-rbinom(length(M2), 1, pr) #GIVES A 1 IF THE FISH JUST BECAME MATURE
  M2[which(FS==1)]<-0 #ZEROS OUT NEWLY MATURE FISH AND LEAVES A 1 FOR RECRUDECENT MATURE FISH
  return(list(mature=M2, FirstSpawn=FS))				
}


##[8] INITIAL TIME SINCE SPAWNING
ini_mps<- function(n,mature)
	{# months since spawning
	out<-sample(c(1:4),size=n,replace=TRUE)*mature*12
	return(out)
}


##[10] INITIALIZE RIVER LOCATION
initialize_spatial_location<- function(n,nbends,relativeDensity)
{
  x<- sample(c(1:nbends),n,relativeDensity,replace=TRUE)
  return(x)
}