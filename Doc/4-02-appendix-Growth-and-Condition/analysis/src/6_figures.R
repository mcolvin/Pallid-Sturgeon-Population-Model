figures<- function(n){

if(n==1)
	{# PLOT OF WHERE SAMPLING OCCURED
	plot(manunits,axes=TRUE,las=1)
	points(startlatitude~startlongitude,dat,subset=basin=="upper",col='grey',pch=19)
	points(startlatitude~startlongitude,dat,subset=basin=="lower",col='black',pch=19)
	legend("bottomleft", c("Upper basin", "Lower basin"),col=c("grey","black"),bty='n',pch=19)
	}

if(n==2)
	{# PLOT OF LENGTH-WEIGHT RELATIONSHIP
	par(mfrow=c(2,1),mar=c(1,3,0,0),
		oma=c(3,1,1,1))
	trans_black<- rgb(0,0,0,alpha=40,maxColorValue=255)# make grey color with some transparency
	plot(weight/1000~length,dat,subset=basin=="upper",las=1,xlim=c(0,2000),col=trans_black,bg=trans_black,
		pch=21,xaxt="n")
	axis(1, at=axTicks(1), label=FALSE)
	panLab("a) Upper basin")
	plot(weight/1000~length,dat,subset=basin=="lower",las=1,xlim=c(0,2000),col=trans_black,bg=trans_black,pch=21)
	panLab("b) Lower basin")
	mtext(side=2,"Weight (kg)",outer=TRUE,line=-0.5, cex=1.5)
	mtext(side=1,"Length (mm)",outer=TRUE,line=1.5, cex=1.5)
	}
	
if(n==3)
	{
	# PLOT OF L1 AND L2 FOR UPPER AND LOWER BASIN FISH
	#plotdat<-tables(2)
	
	plotdat<- tables(4)
	trans_grey<- rgb(0,0,0,alpha=40,maxColorValue=255)
	par(mfrow=c(2,1),mar=c(2,3,0,0),
		oma=c(2,1,1,1))
	# UPPER BASIN
	plot(dl~dt,plotdat,type='p',
		ylab="",xlab="",las=1,subset=basin=="upper", col=trans_grey,pch=19,
		xlim=c(0,5500),xaxt='n')
	axis(1, at=axTicks(1),labels=FALSE)
	panLab("Upper basin")		
	plot(dl~dt,plotdat,type='p',
		ylab="",xlab="",las=1,subset=basin=="lower",col=trans_grey,pch=19,xlim=c(0,5500))	
	panLab("Lower basin")
	mtext(side=2,"Length change (mm)",outer=TRUE,line=0, cex=1.5)
	mtext(side=1,"Time (Days)",outer=TRUE,line=0.5, cex=1.5)
	}
	

if(n==5)
	{# PLOT TIME AND GROWTH
	plotdat<- tables(4)
	trans_grey<- rgb(0,0,0,alpha=40,maxColorValue=255)
	par(mfrow=c(2,1),mar=c(2,3,0,0),oma=c(2,2,1,1))
	low<-subset(plotdat,basin=="upper")
	plot(c(min(plotdat$setdate1),max(plotdat$setdate2)),
		c(min(c(plotdat$l1,plotdat$l2)),max(c(plotdat$l1,plotdat$l2))),
		type="n",las=1,xaxt='n')
	axis(1, at=axTicks(1),labels=FALSE)
	for(i in 1:max(low$ind_id))
		{
		segments(low[low$ind_id==i,]$setdate1,
			low[low$ind_id==i,]$l1,
			low[low$ind_id==i,]$setdate2,
			low[low$ind_id==i,]$l2,col=trans_grey,
			lwd=1)
		}
	panLab("Upper")
	low<-subset(plotdat,basin=="lower")
	plot(c(min(plotdat$setdate1),max(plotdat$setdate2)),
		c(min(c(plotdat$l1,plotdat$l2)),max(c(plotdat$l1,plotdat$l2))),
		type="n",las=1)
	for(i in 1:max(low$ind_id))
		{
		segments(low[low$ind_id==i,]$setdate1,
			low[low$ind_id==i,]$l1,
			low[low$ind_id==i,]$setdate2,
			low[low$ind_id==i,]$l2,col=trans_grey,
			lwd=1)
		}		
	mtext(text="Date",side=1,outer=TRUE,line=0,cex=1.3)
	mtext(text="Length (mm)",side=2,outer=TRUE,line=0,cex=1.3)
	panLab("Lower")	
	}
if(n==6)
	{
	}
	
if(n==7)
	{
	}
if(n==8)
	{


	}
if(n==9)
		{
		prec<- tables(3)
		par(mfrow=c(2,1), oma=c(1,1,1,1),mar=c(3,3,0,0))
		plot(dw_prop~dt,prec, 
			xlab="", ylab="",ylim=c(0.5,1.5),
			las=1, subset=basin=="upper",xaxt='n')
		panLab("a) Upper basin")
		axis(1, at=axTicks(1), labels=FALSE)
		plot(dw_prop~dt,prec, 
			xlab="",ylab="",ylim=c(0.5,1.5),
			las=1, subset=basin=="lower")
		panLab("b) Lower basin")
		mtext(side=2, "Proportional change in weight",
			outer=TRUE, line=0, cex=1.3)
		mtext(side=1, "Days since last capture",
			outer=TRUE, line=0, cex=1.3)	
		}	
	}
	
