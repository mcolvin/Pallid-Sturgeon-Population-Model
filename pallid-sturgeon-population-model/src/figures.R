figures<- function(n)
	{
	op <- par(no.readonly = TRUE) 
	if(n==0)
	{# NATURAL ORIGIN ABUNDANCE 
	  x<-out$years
	  y<-(apply(out$total_N,1,mean))/1000 
	  yup<-(apply(out$total_N,1,max))/1000
	  ylo<-(apply(out$total_N,1,min))/1000
	  plot(x,y,ylab="Natural-Origin Abundance (1000 fish)",
	       xlab="Year",las=1,type='l',ylim=c(0,max(yup)))
	  polygon(c(x,rev(x)),c(ylo,rev(yup)),col="lightgrey",border="lightgrey")
	  points(x,y,type='l')
	}
	if(n==1)
	{# HATCHERY ORIGIN ABUNDANCE 
	  x<-out$years
	  y<-(apply(out$total_H,1,mean))/1000 
	  yup<-(apply(out$total_H,1,max))/1000
	  ylo<-(apply(out$total_H,1,min))/1000
	  plot(x,y,ylab="Hatchery-Origin Abundance (1000 fish)",
	       xlab="Year",las=1,type='l',ylim=c(min(ylo)-3,max(yup)))
	  polygon(c(x,rev(x)),c(ylo,rev(yup)),col="lightgrey",border="lightgrey")
	  points(x,y,type='l')
	}
	if(n==2)
		{# TOTAL ABUNDANCE 
		x<-out$years
		yy<- out$total_N+ out$total_H
		y<-(apply(yy,1,mean))/1000 
		yup<-(apply(yy,1,max))/1000
		ylo<-(apply(yy,1,min))/1000
		plot(x,y,ylab="Total abundance (x1000)",
			xlab="Year",las=1,type='l',ylim=c(min(ylo)-3,max(yup)))
		polygon(c(x,rev(x)),c(ylo,rev(yup)),col="lightgrey",border="lightgrey")
		points(x,y,type='l')
		}
	if(n==3)
		{# MEAN WEIGHT 
		op <- par(no.readonly = TRUE) 
		par(mfrow=c(2,1),mar=c(1,4,0.25,0),
			oma=c(3,1,1,1),cex.lab=1.3)	
		x<-out$years
		y<-(apply(out$mean_weight,1,mean))
		yup<-(apply(out$mean_weight,1,max))
		ylo<-(apply(out$mean_weight,1,min))
		plot(out$years,y,ylab="Mean weight (kg)",
			xlab="",las=1,
			type='l',ylim=c(0,max(yup)),
			xaxt='n')
		axis(1,at=axTicks(1),labels=FALSE)
		polygon(c(x,rev(x)),c(ylo,rev(yup)),col="lightgrey",border="lightgrey")
		points(x,y,type='l')
		
		# MEAN LENGTH
		x<-out$years
		y<-(apply(out$mean_length,1,mean))
		yup<-(apply(out$mean_length,1,max))
		ylo<-(apply(out$mean_length,1,min))
		plot(out$years,y,ylab="Mean Length (mm)",
			xlab="Year",las=1,type='l',
			ylim=c(0,max(yup)))
		polygon(c(x,rev(x)),c(ylo,rev(yup)),col="lightgrey",border="lightgrey")
		points(x,y,type='l')
		}
		
	if(n==4)
		{# BIOMASS
		op <- par(no.readonly = TRUE) 
		x<-out$years
		yy<- (out$biomass_n+out$biomass_h)/1000
		y<-apply(yy,1,mean)
		yup<-apply(yy,1,max)
		ylo<-apply(yy,1,min)
		plot(x,y,ylab="Biomass (kg; x1000)",
			xlab="Year",las=1,type='l',ylim=c(0, max(c(yup,ylo))))
		polygon(c(x,rev(x)),c(ylo,rev(yup)),col="lightgrey",border="lightgrey")
		points(x,y,type='l')

		}
	if(n==5)
		{# PSD
		op <- par(no.readonly = TRUE) 
		tmp<-data.frame(year=out$years,
			sq=apply(out$stock,1,mean)-
				apply(out$quality,1,mean),
			qp=apply(out$quality,1,mean)-
				apply(out$preferred,1,mean),
			pm=apply(out$preferred,1,mean)-
				apply(out$memorable,1,mean),
			mt=apply(out$memorable,1,mean)-
				apply(out$trophy,1,mean),
			tr=apply(out$trophy,1,mean))
		tmp$sq<- tmp$sq/apply(out$stock,1,mean)*100
		tmp$qp<- tmp$qp/apply(out$stock,1,mean)*100
		tmp$pm<- tmp$pm/apply(out$stock,1,mean)*100
		tmp$mt<- tmp$mt/apply(out$stock,1,mean)*100
		tmp$tr<- tmp$tr/apply(out$stock,1,mean)*100
		matplot(tmp$year,tmp[,-1],type='l',las=1,ylab="Incrimental PSD value",
			xlab="Year",lwd=2)
		legend("top",legend=c("PSD-SQ","PSD-QP","PSD-PM","PSD-MT","PSD-T"),
			bty="n",lty=c(1:5),col=c(1:5),horiz=TRUE,cex=0.8,lwd=2)
		}
	if(n==6)
		{
		op <- par(no.readonly = TRUE) 
		trans_grey<- rgb(0,0,0,
			alpha=10,
			maxColorValue=255)# make grey color with some transparency
		#par(mfrow=c(2,1))
		#plot(log(out$init_summary$linf),log(out$init_summary$k),
		#	las=1,xlab="Log Length at infinity",col=trans_grey,pch=19,
		#	ylab="Log growth coefficient",
		#	cex.lab=1.3)
		plot((out$init_summary$linf),(out$init_summary$k),
			las=1,xlab="Length at infinity",col=trans_grey,pch=19,
			ylab="Growth coefficient",
			cex.lab=1.3)
		}
	if(n==7)
		{# PLOT OF INITIAL LENGTHS
		op <- par(no.readonly = TRUE) 
		par(oma=c(3,1,1,1),cex.lab=1.3)	
		# LENGTH
		x1<-out$init_summary$len
		x2<-out$post_length		
		brks<- c(seq(0,2000,
			by=100),10000)
		labs<- c(paste(brks[1:(length(brks)-2)],brks[2:(length(brks)-1)],sep="-"),"2000+")
		x1_bin<- cut(x1,breaks=brks,labels=labs	)
		x2_bin<- cut(x2,breaks=brks,labels=labs	)
		x1_relfreq<- table(x1_bin)/length(x1_bin)
		x2_relfreq<- table(x2_bin)/length(x2_bin)
		barplot(t(as.matrix(cbind(x1_relfreq,x2_relfreq))),
			beside=TRUE,
			width=0.4,#space=0.2,
			legend.text=c(floor(min(out$years)),floor(max(out$years))),
			las=1,ylim=c(0,1.1*max(c(x1_relfreq,x1_relfreq))),las=2,
			ylab="Relative frequency",xlab="")	
		box()
		mtext(side=1, "Length group (mm)",line=1,cex=1.3,outer=TRUE)
		}
	if(n==8)
		{
		# PLOT OF INITIAL AND FINAL LENGTHS
		op <- par(no.readonly = TRUE) 
		par(oma=c(1,1,1,1),cex.lab=1.3)				
		x1<-out$init_summary$wg/1000
		x2<-out$post_weight/1000
		brks<- c(seq(0,50,
			by=10),2000)
		labs<- c(paste(brks[1:(length(brks)-2)],brks[2:(length(brks)-1)],sep="-"),"50+")
		x1_bin<- cut(x1,breaks=brks,labels=labs	)
		x2_bin<- cut(x2,breaks=brks,labels=labs	)
		x1_relfreq<- table(x1_bin)/length(x1_bin)
		x2_relfreq<- table(x2_bin)/length(x2_bin)
		barplot(t(as.matrix(cbind(x1_relfreq,x2_relfreq))),
			beside=TRUE,
			width=0.4,#space=0.2,
			legend.text=c(floor(min(out$years)),floor(max(out$years))),
			las=1,ylim=c(0,1.1*max(c(x1_relfreq,x1_relfreq))),las=2,
			ylab="Relative frequency",xlab="")	
		box()
		mtext(side=1, "Weigth group (kg)",line=-1,cex=1.3,outer=TRUE)
		}
	on.exit(par(op))
	}
	
	
	
	
	
	
	