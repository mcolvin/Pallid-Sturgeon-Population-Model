figures<- function(n)
	{
	if(n==1)
	
	par(mfrow=c(2,1),mar=c(2,2,0,0),oma=c(2,4,1,1))
	x<-na.omit(dat[dat$basin=="upper"& dat$year==2015,]$length)	
	hist(x,xlim=c(0, 2000),main="",prob=TRUE,ylim=c(0, 0.008),las=1,ylab="")
	d <- density(x)
	lines(d,lwd=2, col="grey")
	box()
	panLab("Upper")
	x<-na.omit(dat[dat$basin=="lower"& dat$year==2015,]$length)	
	hist(dat[dat$basin=="lower"& dat$year==2015,]$length,xlim=c(0, 2000),main="",prob=TRUE,
		ylab="",las=1)
	d <- density(x)
	lines(d,lwd=2, col="grey")
	box()		
	mtext(side=2,"Density",outer=TRUE,line=2,cex=1.5)
	mtext(side=1,"Length (mm)",outer=TRUE,line=0.5,cex=1.5)

	panLab("Lower")
	}
	
