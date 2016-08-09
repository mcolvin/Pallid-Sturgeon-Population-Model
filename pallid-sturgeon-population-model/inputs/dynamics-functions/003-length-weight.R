dWeight<- function(len,a=0.0001,b=3,er=0.1)
	{
	#out<-rlnorm(length(len),log(a*len^b),er)
	ypred<- log(a)+b*log(len)
	out<-exp(rnorm(length(len),ypred,er))
	return(out)
	}
	
dWeight_v<- function(x,a=0.0001,b=3,er=0.1)
	{
	rlnorm(1,log(a*x^b),er) ####fixme####
	}