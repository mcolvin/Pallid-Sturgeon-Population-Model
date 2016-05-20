tables<- function(n){

if(n==1)
	{# DISTRIBUTION OF LENGTHS TO INITIALIZE MODEL
	xx<-ecdf(dat[dat$basin=="lower"& dat$year==2015,]$length)
	y<- 0:max(na.omit(dat[dat$basin=="lower"& dat$year==2015,]$length))
	out<- data.frame(x=xx(y),len=y,basin="lower")
	
	xx<-ecdf(dat[dat$basin=="upper"& dat$year==2015,]$length)
	y<- 0:max(na.omit(dat[dat$basin=="upper"& dat$year==2015,]$length))
	out<- rbind(out,data.frame(x=xx(y),len=y,basin="upper")	)
	


	
	plot(xx)
	
	
	}

}