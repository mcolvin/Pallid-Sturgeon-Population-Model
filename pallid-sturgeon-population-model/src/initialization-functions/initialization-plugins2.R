
#### FUNCTIONS TO ASSIGN RKM TO BENDS FOR UPPER AND LOWER
#rkm_start<-seq(1,1000,length=317)
#bend<- c(1:317)
#rkm2bend<- approxfun(rkm_start,bend,method='constant')
#bend2rkm<- approxfun(bend,rkm_start,method='constant')


# INITIALIZATION PLUGINS
##[1A] EXTRAPOLATE HATCHERY DATA
ini_hatchery<-function(stockingHist=NULL,
                       N0_hatchery=NULL,
                       genetics=NULL,
                       hatchery_name=NULL)
{
  stockingHist$N_hat<-rbinom(nrow(stockingHist),
                             stockingHist$number,
                             stockingHist$survival_est)
  stockingHist$pr<-
    stockingHist$N_hat/sum(stockingHist$N_hat)
  
  stockingHist$N0<-rmultinom(1, N0_hatchery, stockingHist$pr)
  # stockingHist$yr1<-ceiling(stockingHist$age/12)*12-
  #   stockingHist$age
  # stockingHist$yr2plus<-ifelse(stockingHist$MPStock>12, 
  #                                     floor((stockingHist$MPStock-
  #                                       stockingHist$yr1)/12),
  #                                     0)
  # stockingHist$remainder<-ifelse(stockingHist$MPStock>12, 
  #                                       stockingHist$MPStock-
  #                                         (stockingHist$yr2plus*
  #                                         12+stockingHist$yr1),
  #                                       0)
  # stockingHist$p1<-c(tmp$phi0,tmp$phi)[
  #   floor(stockingHist$age/12)+1]^(stockingHist$yr1/12)
  # stockingHist$p2<-sapply(1:nrow(stockingHist), function(y)
  # {
  #   p2<-ifelse(stockingHist$yr2plus[y]>0,
  #              prod(tmp$phi[ceiling(stockingHist$age[y]/12):
  #                 (stockingHist$yr2plus[y]+
  #                    ceiling(stockingHist$age[y]/12)-1)]),
  #              1)
  # })
  # stockingHist$p3<-c(tmp$phi0,tmp$phi)[
  #   floor(stockingHist$current_age/12)+1]^(stockingHist$remainder/12)
  # stockingHist$current_number<-rbinom(length(stockingHist$number), 
  #                                            stockingHist$number,
  #                                            stockingHist$p1*stockingHist$p2*stockingHist$p3)
  tmp<-lapply(1:nrow(stockingHist), function(i)
  {
    out<-rnorm(stockingHist$N0[i], 
               stockingHist$length_mn[i],
               stockingHist$length_sd[i])
    return(out)
  })
  ini_H <- data.frame(L=unlist(tmp),
                      A=rep(stockingHist$current_age, 
                            stockingHist$N0),
                      dA=rep(stockingHist$current_age-stockingHist$age,
                             stockingHist$N0))
  ini_H$L <- ifelse(ini_H$L<=0, mean(stockingHist$length_mn), ini_H$L)
  if(genetics)
  {
    ini_H$M<-rep(stockingHist$mother, 
                 stockingHist$N0)
    ini_H$D<-rep(stockingHist$father, 
                 stockingHist$N0)
  }
  if(hatchery_name)
  {
    ini_H$H<-rep(stockingHist$hatchery, 
                 stockingHist$N0)
  }
  return(ini_H)
}

##[1B] EXTRAPOLATE HATCHERY DATA
ini_natural<-function(natHist=NULL,
                      N0_natural=NULL,
                      genetics=NULL)
{
  natHist$Z<-rbinom(nrow(natHist), 1, natHist$pr)
  if(sum(natHist$Z)>N0_natural)
  {
    indx<-sample(which(natHist$Z==1), sum(natHist$Z)-N0_natural, 
                 replace = FALSE)
    natHist$Z[indx]<-0
  }
  ini_N <- data.frame(S=natHist[which(natHist$Z==1),]$sex,
                      L=natHist[which(natHist$Z==1),]$length,
                      A=natHist[which(natHist$Z==1),]$minAge,
                      dT=natHist[which(natHist$Z==1),]$dT)
  ini_N[which(is.na(ini_N$L)),"L"]<-0
  ini_N[which(is.na(ini_N$dT)),"dT"]<-0
  if(genetics)
  {
    ini_N$TAG<-natHist[which(natHist$Z==1),]$individual
  }
  return(ini_N)
}

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
ini_sex<- function(n,prob_F)
{
  out<-rbinom(n,1,prob_F)  #0=M, 1=F
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

# ##[] INITIALIZE HATCHERY AND PARENTAL INFORMATION
# ## NEEDS ADJUSTING
# ini_hatch_info<- function(age=NULL, 
#                           hatchery_info=NULL, 
#                           genetics=NULL)
# {
#   age<-floor(age/12)
#   setA<-setdiff(unique(age),0)
#   out<-lapply(setA, function(i)
#   {
#     indxA<-which(age==i)
#     indxH<-which(hatchery_info$age==i)
#     x<-NULL
#     if(length(indxH)>0)
#     {
#       x<-sample(indxH, size=length(indxA), replace=TRUE, 
#            prob=hatchery_info$no_stocked[indxH]/sum(hatchery_info$no_stocked[indxH]))
#       if(genetics)
#       {
#         x<-cbind(indxA, hatchery_info$hatchery[x], 
#                  hatchery_info$mother[x], hatchery_info$father[x])
#       }
#       if(!genetics)
#       {
#         x<-cbind(indxA, hatchery_info$hatchery[x])
#       }
#     }
#     return(x)
#   })
#   out<-do.call(rbind, out)
#   out<-out[order(out[,1]),]
#   out<-out[,-1]
#   return(out)
# }

