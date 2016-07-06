

# lower basin
fit1a<-glmer(EGGS~W+(1|id),dat,
	family=poisson(link="log"),subset=BASIN=="Lower")
fit2a<-glmer(EGGS~FL+(1|id),dat,
	family=poisson(link="log"),subset=BASIN=="Lower")

	
fit1a<-glmer(EGGS~w_std:BASIN+(1|id)+(1|BASIN),dat,	family=poisson(link="log"))
AIC(fit1a)
fit1a<-glmer(EGGS~w_std:BASIN+(1|id),dat,	family=poisson(link="log"))
AIC(fit1a)
dat$eggs_w<- dat$EGGS/dat$W
dat$eggs_w_ln<- log(dat$eggs_w)
boxplot(eggs_w~BASIN,dat)
plot(eggs_w_ln~FL,dat)
points(eggs_w_ln~FL,dat,subset=BASIN=="Lower",col="red")
plot(EGGS~W,dat)
points(EGGS~W,dat,subset=BASIN=="Lower",col="red")
	
fit2a<-glmer(EGGS~fl_std+(1|id/BASIN),dat,
	family=poisson(link="log"),subset=BASIN=="Lower")
fit2a<-glmer(EGGS~fl_std+(1|id),dat,
	family=poisson(link="log"),subset=BASIN=="Upper")

	
low<-subset(dat,BASIN=="Lower")
fit1a<-glm(EGGS~fl_std + 1,
	low,
	offset=low$W,
	family=poisson(link="log"))		
fit2a<-glmer(EGGS~fl_std+(1|id),
	low,
	offset=low$W,
	family=poisson(link="log"))	
out<-cbind(low$EGGS, fitted(fit1a),fitted(fit2a))
logLik(fit1a)
sum(dpois(out[,1],out[,2],log=TRUE))
logLik(fit2a)
sum(dpois(out[,1],out[,3],log=TRUE))
AIC(fit1a)
AIC(fit2a)







# Upper
fit1a<-glmer(EGGS~W+(1|id),dat,
	family=poisson(link="log"),subset=BASIN=="Upper")
fit2a<-glmer(EGGS~FL+(1|id),dat,
	family=poisson(link="identity"),subset=BASIN=="Upper")
AIC(fit1a)
AIC(fit2a)

AIC(fit1a)
AIC(fit2a)

plot(EGGS~W,dat,subset=BASIN=="Upper")
plot(EGGS~FL,dat,subset=BASIN=="Upper")





x<- runif(500,14,30)
y<-fixef(fit2a)[1] + fixef(fit2a)[2]*x+rnorm(500,0,62458)
EGGS<- rpois(500,y)
plot(EGGS~W,dat,subset=BASIN=="Upper",pch=19)

plot(x,EGGS,col="red")
points(EGGS~W,dat,subset=BASIN=="Upper",pch=19)

