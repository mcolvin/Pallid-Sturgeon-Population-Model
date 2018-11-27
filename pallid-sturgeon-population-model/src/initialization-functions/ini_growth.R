# INITIALIZE GROWTH PARAMETERS (L_INF, K)	
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

