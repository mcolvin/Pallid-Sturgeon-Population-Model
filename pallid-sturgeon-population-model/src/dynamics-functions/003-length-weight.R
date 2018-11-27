dWeight<- function(len,a=0.0001,b=3,er=0.1)
	{
	ypred<- log(a)+b*log(len)
	out<-exp(rnorm(length(len),ypred,er))
	return(out)
	}

