tables<- function(n)
	{
	if(n==1)
		{# DISTRIBUTION OF LENGTHS TO INITIALIZE MODEL
		x<-dcast(dat,year~basin+origin,value.var="tmp",sum)
		x$total<- apply(x[,-1],1,sum)
		return(x)
		}
	}
