figures<- function(n){

if(n==1){
	# lower basin
	fit1a<-glmer(EGGS~W+(1|id),dat,
		family=poisson(link="log"),subset=BASIN=="Lower")
	# PLOT OF ESTIMATED FECUNDITY AND OBSERVED
	x<- runif(5000,0,7)
	y<-exp(fixef(fit1a)[1] + fixef(fit1a)[2]*x+rnorm(5000,0,0.28))
	EGGS<- rpois(5000,y)
	trans<- rgb(0,0,0,alpha=40,maxColorValue=255)
	plot(x,EGGS,type='n')
	op<-par()
	par(fg="transparent")
	points(x,EGGS,pch=21,bg=trans)
	points(EGGS~W,dat,subset=BASIN=="Lower",pch=19)
	
	}

}