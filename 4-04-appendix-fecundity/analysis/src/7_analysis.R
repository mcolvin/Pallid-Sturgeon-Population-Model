

# lower basin
fit1a<-glmer(EGGS~W+(1|id),dat,
	family=poisson(link="log"),subset=BASIN=="Lower")
fit2a<-glmer(EGGS~FL+(1|id),dat,
	family=poisson(link="log"),subset=BASIN=="Lower")

	
	
fit2a<-glmer(EGGS~fl_std+(1|id),dat,
	family=poisson(link="log"),subset=BASIN=="Lower")
fit2a<-glmer(EGGS~fl_std+(1|id),dat,
	family=poisson(link="log"),subset=BASIN=="Upper")

	
	
AIC(fit1a)
AIC(fit2a)

plot(EGGS/1000~FL,dat,subset=BASIN=="Upper")
plot(EGGS~FL,dat,subset=BASIN=="Lower")


x<- runif(500,0,7)
y<-exp(fixef(fit1a)[1] + fixef(fit1a)[2]*x+rnorm(500,0,0.28))
EGGS<- rpois(500,y)
plot(x,EGGS,col="red")
points(EGGS~W,dat,subset=BASIN=="Lower",pch=19)


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

