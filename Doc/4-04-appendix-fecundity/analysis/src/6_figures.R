figures<- function(n){

if(n==1)
	{
	par(mfrow=c(2,1),mar=c(2,4,0,0),oma=c(2,1,1,1),xpd=NA)
	plot(EGGS/1000~FL,dat,type='n',las=1,xlab="",
		ylab="Eggs (x1000)",cex.lab=1.3,xaxt='n')
	axis(1, at=axTicks(1),labels=FALSE)
	points(EGGS/1000~FL,dat,subset=BASIN=="Lower",pch=19)
	points(EGGS/1000~FL,dat,subset=BASIN=="Upper",pch=17)
	legend("topleft",legend=c("RPMA2","RPMA4"),pch=c(17,19),bty='n')
	
	plot(EGGS/1000/W~FL,dat,type='n',xlab="",las=1,
		ylab="Eggs (x1000) per kilogram",cex.lab=1.3)		
	points(EGGS/1000/W~FL,dat,subset=BASIN=="Lower",pch=19)
	points(EGGS/1000/W~FL,dat,subset=BASIN=="Upper",pch=17)	
	#legend("topleft",legend=c("RPMA2","RPMA4"),pch=c(17,19),bty='n')
	mtext(side=1, "Fork length (mm)",outer=TRUE, line=0,cex=1.3)
	}

if(n==2)
	{# PREDICTED EGGS BY LENGTH AND CONDITION
	
	pdat<- expand.grid(fl=seq(750,1200,50),
		kn=seq(0.7,2.1,0.2),RPMA=4,loc=1)
	pdat<-rbind(pdat, expand.grid(fl=seq(1300,1700,50),
		kn=seq(0.7,2.1,0.2),RPMA=2,loc=2))	
	pdat$ws<- (10^-6.2561)*(pdat$fl^3.2932)
	pdat$w<- pdat$ws * pdat$kn / 1000 # weight in kg
	
	
	pdat$ln_eggs_m3<-out3$BUGSoutput$mean$a[pdat$loc] + out3$BUGSoutput$mean$b_fec[pdat$loc]*pdat$fl + log(pdat$w)
	pdat$ln_eggs_m1<-out1$BUGSoutput$mean$a + out1$BUGSoutput$mean$b_fec*pdat$fl + log(pdat$w)
	pdat$ln_eggs_m0<-out0$BUGSoutput$mean$a + out0$BUGSoutput$mean$b_fec*pdat$fl + log(pdat$w)
	pdat$ln_eggs_m2<- out2$BUGSoutput$mean$a[pdat$loc] + out2$BUGSoutput$mean$b_fec*pdat$fl + log(pdat$w)
	
	w<- as.numeric(tables(1)[c(1:4),5])
	w<-w/sum(w)
	pdat$eggs<- exp(pdat$ln_eggs_m3*w[1] +pdat$ln_eggs_m1*w[2] +pdat$ln_eggs_m0*w[3] + pdat$ln_eggs_m2*w[4])
	
	plot(eggs/1000~fl,pdat,ylim=c(0,260),las=1,cex.lab=1.3,
		xlab="Fork length (mm)",
		ylab="Eggs (x1000)", type='n')
	knn<-seq(0.7,2.1,0.2)
	plotCol<- grey(seq(0,0.9,length.out=length(knn)))
	for(i in 1:length(knn))
		{
		points(eggs/1000~fl,pdat,lty=1,subset=kn==knn[i]& loc==1,type='l',col=plotCol[i],lwd=3)	
		points(eggs/1000~fl,pdat,lty=1,subset=kn==knn[i] & loc==2,type='l',col=plotCol[i],lwd=3)			
		}
	legend("topleft",legend=knn,title="Condition factor",lty=1,ncol=2,col=plotCol,lwd=3)
	text(880,40,"RPMA4")
	text(1400,120,"RPMA2")

	
	}

}