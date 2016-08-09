# INITIALIZE GROWTH PARAMETERS (L_INF, K)	
ini_growth<- function(n,mu_ln_linf,mu_ln_k,vcv,maxLinf=2100)
	{
	tmp<-mvrnorm(n,c(mu_ln_linf, mu_ln_k),
		matrix(vcv,2,2,byrow=TRUE))
	#indx<- which(tmp[,1]>log(maxLinf))
	# FILL VECTOR WITH VALUES THAT ARE LESS THAN MAXLINF
	#if(length(indx)>0)
	#	{
	#	repeat
	#		{
	#		tmp[indx,]<- mvrnorm(length(indx),c(mu_ln_linf, mu_ln_k),
	#			matrix(vcv,2,2,byrow=TRUE))
	#		indx<- which(tmp[,1]>log(maxLinf))
	#		if(length(indx)==0){break}
	#		}
	#	}
	tmp<- exp(tmp)
	return(list(linf=tmp[,1],k=tmp[,2]))
	}
