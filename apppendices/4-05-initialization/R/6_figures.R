figures<- function(n)
	{
	
	#plot(manunits,axes=TRUE,las=1)
	#points(startlatitude~startlongitude,dat,subset=basin=="upper",col='grey',pch=19)
	#points(startlatitude~startlongitude,dat,subset=basin=="lower",col='black',pch=19)
	#legend("bottomleft", c("Upper basin", "Lower basin"),col=c("grey","black"),bty='n',pch=19)
	
	
	if(n==1)
		{# PLOT OF WHERE SAMPLING OCCURED
		
		plotme<-function(basini,yeari)
			{
			if(basini=="lower")
				{
				plot(manunits[manunits$Unit%in%c("IHMU","CLMU"),],axes=TRUE,las=1,
					xaxt='n',yaxt='n')
				axis(1,at=axTicks(1),labels=FALSE)
				axis(2,at=axTicks(2),labels=FALSE)
				}
			if(basini=="upper")
				{			
				plot(manunits[manunits$Unit%in%c("GPMU"),],axes=TRUE,las=1,
					xaxt='n',yaxt='n')
				axis(1,at=axTicks(1),labels=FALSE)
				axis(2,at=axTicks(2),labels=FALSE)
				}
			panLab(yeari)
			points(startlatitude~startlongitude,dat,subset=(basin==basini & year==yeari),
				col='black',pch=1)	
			}
		par(mfrow=c(5,2),mar=c(1,1,0,0),oma=c(1,1,1,1))
		plotme("lower",2006)
		plotme("lower",2007)
		plotme("lower",2008)
		plotme("lower",2009)
		plotme("lower",2010)
		plotme("lower",2011)
		plotme("lower",2012)
		plotme("lower",2013)
		plotme("lower",2014)
		plotme("lower",2015)
		}
	if(n==2)
		{# PLOT OF WHERE SAMPLING OCCURED
		plotme<-function(basini,yeari)
			{
			if(basini=="lower")
				{
				plot(manunits[manunits$Unit%in%c("IHMU","CLMU"),],axes=TRUE,las=1,
					xaxt='n',yaxt='n')
				axis(1,at=axTicks(1),labels=FALSE)
				axis(2,at=axTicks(2),labels=FALSE)
				}
			if(basini=="upper")
				{			
				plot(manunits[manunits$Unit%in%c("GPMU"),],axes=TRUE,las=1,
					xaxt='n',yaxt='n')
				axis(1,at=axTicks(1),labels=FALSE)
				axis(2,at=axTicks(2),labels=FALSE)
				}
			panLab(yeari)
			points(startlatitude~startlongitude,dat,subset=(basin==basini & year==yeari),
				col='black',pch=1)	
			}
		par(mfrow=c(5,2),mar=c(2,2,0,0),oma=c(3,3,1,1))
		plotme("upper",2006)
		plotme("upper",2007)
		plotme("upper",2008)
		plotme("upper",2009)
		plotme("upper",2010)
		plotme("upper",2011)
		plotme("upper",2012)
		plotme("upper",2013)
		plotme("upper",2014)
		plotme("upper",2015)
		}		
	if(n==3)
		{#plot of length distiributations over time for lower basin		
		plot_func<- function(basin,year)
			{
			x<-na.omit(dat[dat$basin==basin & dat$year==year,]$length)
			x<-hist(x,breaks=brks,plot=FALSE)
			rel_freq<- x$counts/sum(x$counts)
			rel_freq<-ifelse(rel_freq==0,NA,rel_freq)
			plot(x$mids,rel_freq,type='h',xlab="",ylab="",
				las=1,xaxt='n',ylim=c(0,0.1),yaxt='n')
			axis(1,at=axTicks(1),labels=FALSE)
			axis(2,at=axTicks(2),labels=FALSE)
			panLab(year)			
			}	
		brks<-seq(0,2000,25)	
		par(mfrow=c(5,2),mar=c(2,2,0,0),oma=c(3,3,1,1))
		plot_func("lower",2006);axis(2,at=axTicks(2),labels=TRUE,las=1)
		plot_func("lower",2007)
		plot_func("lower",2008);axis(2,at=axTicks(2),labels=TRUE,las=1)
		plot_func("lower",2009)
		plot_func("lower",2010);axis(2,at=axTicks(2),labels=TRUE,las=1)
		plot_func("lower",2011)
		plot_func("lower",2012);axis(2,at=axTicks(2),labels=TRUE,las=1)
		plot_func("lower",2013)
		plot_func("lower",2014);axis(1,at=axTicks(1),labels=TRUE);axis(2,at=axTicks(2),labels=TRUE,las=1)
		plot_func("lower",2015);axis(1,at=axTicks(1),labels=TRUE)
		mtext(side=2,"Relative frequency",line=1.2,outer=TRUE,cex=1.3)
		mtext(side=1,"Length (mm)",line=1,outer=TRUE,cex=1.3)
		}
	if(n==4)
		{#plot of length distiributations over time for upper basin		
		plot_func<- function(basin,year)
			{			
			x<-na.omit(dat[dat$basin==basin & dat$year==year ,]$length)
			x<-hist(x,breaks=brks,plot=FALSE)
			rel_freq<- x$counts/sum(x$counts)
			rel_freq<-ifelse(rel_freq==0,NA,rel_freq)
			plot(x$mids,rel_freq,type='h',xlab="",ylab="",
				las=1,xaxt='n',ylim=c(0,0.25),yaxt='n')
			axis(1,at=axTicks(1),labels=FALSE)
			axis(2,at=axTicks(2),labels=FALSE)
			panLab(year)			
			}	
		brks<-seq(0,2000,25)	
		par(mfrow=c(5,2),mar=c(2,2,0,0),oma=c(3,3,1,1))
		plot_func("upper",2006);axis(2,at=axTicks(2),labels=TRUE,las=1)
		plot_func("upper",2007)
		plot_func("upper",2008);axis(2,at=axTicks(2),labels=TRUE,las=1)
		plot_func("upper",2009)
		plot_func("upper",2010);axis(2,at=axTicks(2),labels=TRUE,las=1)
		plot_func("upper",2011)
		plot_func("upper",2012);axis(2,at=axTicks(2),labels=TRUE,las=1)
		plot_func("upper",2013)
		plot_func("upper",2014);axis(1,at=axTicks(1),labels=TRUE);axis(2,at=axTicks(2),labels=TRUE,las=1)
		plot_func("upper",2015);axis(1,at=axTicks(1),labels=TRUE)
		mtext(side=2,"Relative frequency",line=1.2,outer=TRUE,cex=1.3)
		mtext(side=1,"Length (mm)",line=1,outer=TRUE,cex=1.3)
		}

	if(n==5)
		{# plot of distiributations used 
		# to draw lengths from.
		plot_func<- function(basin,year)
			{			
			x<-na.omit(dat[dat$basin==basin & 
				dat$year==year &
				dat$origin=="natural",]$length)
			x<-hist(x,breaks=brks,plot=FALSE)
			rel_freq<- x$counts/sum(x$counts)
			rel_freq<-ifelse(rel_freq==0,NA,rel_freq)
			plot(x$mids,rel_freq,type='h',xlab="",ylab="",
				las=1,xaxt='n',ylim=c(0,0.25),yaxt='n')
			axis(1,at=axTicks(1),labels=FALSE)
			axis(2,at=axTicks(2),labels=FALSE)
			panLab(year)			
			}	
		brks<-seq(0,2000,25)	
		par(mfrow=c(5,2),mar=c(2,2,0,0),oma=c(3,3,1,1))
		plot_func("lower",2006);axis(2,at=axTicks(2),labels=TRUE,las=1)
		plot_func("lower",2007)
		plot_func("lower",2008);axis(2,at=axTicks(2),labels=TRUE,las=1)
		plot_func("lower",2009)
		plot_func("lower",2010);axis(2,at=axTicks(2),labels=TRUE,las=1)
		plot_func("lower",2011)
		plot_func("lower",2012);axis(2,at=axTicks(2),labels=TRUE,las=1)
		plot_func("lower",2013)
		plot_func("lower",2014);axis(1,at=axTicks(1),labels=TRUE);axis(2,at=axTicks(2),labels=TRUE,las=1)
		plot_func("lower",2015);axis(1,at=axTicks(1),labels=TRUE)
		mtext(side=2,"Relative frequency",line=1.2,outer=TRUE,cex=1.3)
		mtext(side=1,"Length (mm)",line=1,outer=TRUE,cex=1.3)
		}
	if(n==6)
		{# plot of distiributations used 
		# to draw lengths from.
		plot_func<- function(basin,year)
			{			
			x<-na.omit(dat[dat$basin==basin & 
				dat$year==year &
				dat$origin=="hatchery",]$length)
			x<-hist(x,breaks=brks,plot=FALSE)
			rel_freq<- x$counts/sum(x$counts)
			rel_freq<-ifelse(rel_freq==0,NA,rel_freq)
			plot(x$mids,rel_freq,type='h',xlab="",ylab="",
				las=1,xaxt='n',ylim=c(0,0.25),yaxt='n')
			axis(1,at=axTicks(1),labels=FALSE)
			axis(2,at=axTicks(2),labels=FALSE)
			panLab(year)			
			}	
		brks<-seq(0,2000,25)	
		par(mfrow=c(5,2),mar=c(2,2,0,0),oma=c(3,3,1,1))
		plot_func("lower",2006);axis(2,at=axTicks(2),labels=TRUE,las=1)
		plot_func("lower",2007)
		plot_func("lower",2008);axis(2,at=axTicks(2),labels=TRUE,las=1)
		plot_func("lower",2009)
		plot_func("lower",2010);axis(2,at=axTicks(2),labels=TRUE,las=1)
		plot_func("lower",2011)
		plot_func("lower",2012);axis(2,at=axTicks(2),labels=TRUE,las=1)
		plot_func("lower",2013)
		plot_func("lower",2014);axis(1,at=axTicks(1),labels=TRUE);axis(2,at=axTicks(2),labels=TRUE,las=1)
		plot_func("lower",2015);axis(1,at=axTicks(1),labels=TRUE)
		mtext(side=2,"Relative frequency",line=1.2,outer=TRUE,cex=1.3)
		mtext(side=1,"Length (mm)",line=1,outer=TRUE,cex=1.3)
		}
	if(n==7)
		{# plot of distiributations used 
		# to draw lengths from.
		plot_func<- function(basin,year)
			{			
			x<-na.omit(dat[dat$basin==basin & 
				dat$year==year &
				dat$origin=="natural",]$length)
			x<-hist(x,breaks=brks,plot=FALSE)
			rel_freq<- x$counts/sum(x$counts)
			rel_freq<-ifelse(rel_freq==0,NA,rel_freq)
			plot(x$mids,rel_freq,type='h',xlab="",ylab="",
				las=1,xaxt='n',ylim=c(0,0.25),yaxt='n')
			axis(1,at=axTicks(1),labels=FALSE)
			axis(2,at=axTicks(2),labels=FALSE)
			panLab(year)			
			}	
		brks<-seq(0,2000,25)	
		par(mfrow=c(5,2),mar=c(2,2,0,0),oma=c(3,3,1,1))
		plot_func("upper",2006);axis(2,at=axTicks(2),labels=TRUE,las=1)
		plot_func("upper",2007)
		plot_func("upper",2008);axis(2,at=axTicks(2),labels=TRUE,las=1)
		plot_func("upper",2009)
		plot_func("upper",2010);axis(2,at=axTicks(2),labels=TRUE,las=1)
		plot_func("upper",2011)
		plot_func("upper",2012);axis(2,at=axTicks(2),labels=TRUE,las=1)
		plot_func("upper",2013)
		plot_func("upper",2014);axis(1,at=axTicks(1),labels=TRUE);axis(2,at=axTicks(2),labels=TRUE,las=1)
		plot_func("upper",2015);axis(1,at=axTicks(1),labels=TRUE)
		mtext(side=2,"Relative frequency",line=1.2,outer=TRUE,cex=1.3)
		mtext(side=1,"Length (mm)",line=1,outer=TRUE,cex=1.3)
		}
	if(n==8)
		{# plot of distiributations used 
		# to draw lengths from.
		plot_func<- function(basin,year)
			{			
			x<-na.omit(dat[dat$basin==basin & 
				dat$year==year &
				dat$origin=="hatchery",]$length)
			x<-hist(x,breaks=brks,plot=FALSE)
			rel_freq<- x$counts/sum(x$counts)
			rel_freq<-ifelse(rel_freq==0,NA,rel_freq)
			plot(x$mids,rel_freq,type='h',xlab="",ylab="",
				las=1,xaxt='n',ylim=c(0,0.25),yaxt='n')
			axis(1,at=axTicks(1),labels=FALSE)
			axis(2,at=axTicks(2),labels=FALSE)
			panLab(year)			
			}	
		brks<-seq(0,2000,25)	
		par(mfrow=c(5,2),mar=c(2,2,0,0),oma=c(3,3,1,1))
		plot_func("upper",2006);axis(2,at=axTicks(2),labels=TRUE,las=1)
		plot_func("upper",2007)
		plot_func("upper",2008);axis(2,at=axTicks(2),labels=TRUE,las=1)
		plot_func("upper",2009)
		plot_func("upper",2010);axis(2,at=axTicks(2),labels=TRUE,las=1)
		plot_func("upper",2011)
		plot_func("upper",2012);axis(2,at=axTicks(2),labels=TRUE,las=1)
		plot_func("upper",2013)
		plot_func("upper",2014);axis(1,at=axTicks(1),labels=TRUE);axis(2,at=axTicks(2),labels=TRUE,las=1)
		plot_func("upper",2015);axis(1,at=axTicks(1),labels=TRUE)
		mtext(side=2,"Relative frequency",line=1.2,outer=TRUE,cex=1.3)
		mtext(side=1,"Length (mm)",line=1,outer=TRUE,cex=1.3)
		}		
	if(n==9)
		{# plot of emperical length distributions
		par(mfrow=c(4,2),mar=c(1,3,0,0),oma=c(3,3,1,1),las=1)
		x<- c(1:2000)
		y1<-p(len_ini_low_natural_nospace)(x)
		y2<-d(len_ini_low_natural_nospace)(x)
		plot(x,y2,type='n',xaxt='n');axis(1,axTicks(1),labels=FALSE)
		polygon(c(x,rev(x)),c(y2,rep(0,length(x))),col="grey",border="grey")
		panLab("Natural origin RPMA4")		
		plot(x,y1,type='l',xaxt='n');axis(1,axTicks(1),labels=FALSE)

		y1<-p(len_ini_low_hatchery_nospace)(x)
		y2<-d(len_ini_low_hatchery_nospace)(x)
		plot(x,y2,type='n',xaxt='n');axis(1,axTicks(1),labels=FALSE)
		polygon(c(x,rev(x)),c(y2,rep(0,length(x))),col="grey",border="grey")
		panLab("Hatchery origin RPMA4")	
		plot(x,y1,type='l',xaxt='n');axis(1,axTicks(1),labels=FALSE)
		
		y1<-p(len_ini_upp_natural_nospace)(x)
		y2<-d(len_ini_upp_natural_nospace)(x)
		plot(x,y2,type='n',xaxt='n');axis(1,axTicks(1),labels=FALSE)
		polygon(c(x,rev(x)),c(y2,rep(0,length(x))),col="grey",border="grey")
		panLab("Natural origin RPMA2")	
		plot(x,y1,type='l',xaxt='n');axis(1,axTicks(1),labels=FALSE)
		
		y1<-p(len_ini_upp_hatchery_nospace)(x)
		y2<-d(len_ini_upp_hatchery_nospace)(x)
		plot(x,y2,type='n',xaxt='n');axis(1,axTicks(1),labels=TRUE)
		polygon(c(x,rev(x)),c(y2,rep(0,length(x))),col="grey",border="grey")
		panLab("Hatchery origin RPMA2")	
		plot(x,y1,type='l',xaxt='n');axis(1,axTicks(1),labels=TRUE)
		
		}
	if(n==10)
		{
		x<-aggregate(tmp~segment_id+origin,dat,sum)
		plot(tmp~segment_id,x,type='b',subset=origin=="hatchery")
		
				x<-aggregate(tmp~segment_id+origin,dat,sum)
		plot(tmp~segment_id,x,type='b',subset=origin=="natural")
		# plot of length by segment id
		boxplot(length~segment_id*origin,dat,las=2)
					abline(v=13.5,lty=2)
		}
	if(n==11)
		{
		
		
		}
		
	}
	
