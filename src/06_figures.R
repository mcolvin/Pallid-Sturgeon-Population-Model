figures<- function(n)
	{
	if(n==1)
		{# TOTAL ABUNDANCE 
		x<-out$years
		y<-(apply((out$hatchery+out$natural)[-1,],1,mean))/1000
		yup<-(apply((out$hatchery+out$natural)[-1,],1,max))/1000
		ylo<-(apply((out$hatchery+out$natural)[-1,],1,min))/1000
		plot(out$years,y,ylab="Total abundance (x1000)",
			xlab="Year",las=1,type='l')
		polygon(c(x,rev(x)),c(ylo,rev(yup)),col="lightgrey",border="lightgrey")
		points(x,y,type='l')
		#savePlot("./output/2016-001/figure-03.wmf",type='wmf')
		}
	if(n==2)
		{# MEAN WEIGHT 
		x<-out$years
		y<-(apply(out$mn_wght,1,mean))/1000
		yup<-(apply(out$mn_wght,1,max))/1000
		ylo<-(apply(out$mn_wght,1,min))/1000
		plot(out$years,y,ylab="Mean weight (kg)",
			xlab="Year",las=1,type='l',ylim=c(0,max(yup)))
		polygon(c(x,rev(x)),c(ylo,rev(yup)),col="lightgrey",border="lightgrey")
		points(x,y,type='l')
		#savePlot("./output/2016-001/figure-04.wmf",type='wmf')
		}
	if(n==3)
		{# BIOMASS
		x<-out$years
		y<-(apply(out$biomass,1,mean)/(1000*1000))
		yup<-(apply(out$biomass,1,max)/(1000*1000))
		ylo<-(apply(out$biomass,1,min)/(1000*1000))
		plot(x,y,ylab="Biomass (kg; x1000)",
			xlab="Year",las=1,type='l')
		polygon(c(x,rev(x)),c(ylo,rev(yup)),col="lightgrey",border="lightgrey")
		points(x,y,type='l')
		#savePlot("./output/2016-001/figure-02.wmf",type='wmf')
		}
	if(n==4)
		{ # PSD
		indx<-which(rowSums(out$qp)>1)
		tmp<-data.frame(year=c(2015+0:49),
			sq=apply(out$sq[indx,],1,mean),
			qp=apply(out$qp[indx,],1,mean),
			pm=apply(out$pm[indx,],1,mean),
			mt=apply(out$mt[indx,],1,mean),
			tr=apply(out$tr[indx,],1,mean))
		matplot(tmp$year,tmp[,-1],type='l',las=1,ylab="Incrimental PSD value",
			xlab="Year",lwd=2)
		legend("top",legend=c("PSD-SQ","PSD-QP","PSD-PM","PSD-MT","PSD-T"),
			bty="n",lty=c(1:5),col=c(1:5),horiz=TRUE,cex=0.8,lwd=2)
		}
	}