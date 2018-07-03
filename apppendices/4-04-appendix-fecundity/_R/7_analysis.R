
	
dat_complete<- subset(dat, is.na(FL)==FALSE)
adat<- list(
	X=as.matrix(
		cbind(dat_complete$W, 
			dat_complete$FL,
			ifelse(dat_complete$BASIN=="Lower",1,2),#basin
			dat_complete$EGGS)),# response
	n=nrow(dat_complete))

mod00<- function()
	{
	# WEIGHT AS AN OFFSET
	for(i in 1:n)
		{
		log(fec_mu[i])<- a + b_fec*X[i,2] + log(X[i,1]) #+ disp[i]
		#disp[i]~dnorm(0,prec_sigma)
		X[i,4]~dpois(fec_mu[i])	
		}
	# PRIORS
	a~dnorm(0,0.001)
	b_fec~dnorm(0,0.001)
	#sigma~dunif(0.00001, 5)
	#prec_sigma[1] <-pow(sigma,-2)
	}
	

inits<- function(t)
	{	
	list(a=0.1,b_fec=0.0005)
	list(a=0.1,b_fec=0.0005)
	list(a=0.1,b_fec=0.0005)
	}

out00 <- jags(data=adat,
	inits=inits,
	parameters=c("a","b_fec","fec_mu"),	
	model.file=mod00,
	n.chains = 3,	
	n.iter = 50000,	
	n.burnin = 25000, 
	n.thin=2,
	working.directory=getwd())	
	
	
	
mod0<- function()
	{
	# WEIGHT AS AN OFFSET
	for(i in 1:n)
		{
		log(fec_mu[i])<- a + b_fec*X[i,2] + log(X[i,1]) + disp[i]
		disp[i]~dnorm(0,prec_sigma)
		X[i,4]~dpois(fec_mu[i])	
		}
	# PRIORS
	a~dnorm(0,0.001)
	b_fec~dnorm(0,0.001)
	sigma~dunif(0.00001, 5)
	prec_sigma[1] <-pow(sigma,-2)
	}
	

inits<- function(t)
	{	
	list(a=0.1,b_fec=0.0005,sigma=c(.25))
	list(a=0.0,b_fec=0.0005,sigma=c(.25))
	list(a=-0.1,b_fec=0.0005,sigma=c(.25))
	}
params<- c("a","b_fec","fec_mu","sigma")	

out0 <- jags(data=adat,
	inits=inits,
	parameters=params,	
	model.file=mod0,
	n.chains = 3,	
	n.iter = 50000,	
	n.burnin = 25000, 
	n.thin=2,
	working.directory=getwd())	
	
	

mod1<- function()
	{
	# WEIGHT AS AN OFFSET
	for(i in 1:n)
		{
		log(fec_mu[i])<- a + b_fec*X[i,2] + log(X[i,1]) + disp[i]
		disp[i]~dnorm(0,prec_sigma[X[i,3]])
		X[i,4]~dpois(fec_mu[i])	
		}
	# PRIORS
	a~dnorm(0,0.001)
	b_fec~dnorm(0,0.001)
	sigma[1]~dunif(0.00001, 5)
	sigma[2]~dunif(0.00001, 5)
	prec_sigma[1] <-pow(sigma[1],-2)
	prec_sigma[2] <-pow(sigma[2],-2)
	}
	

inits<- function(t)
	{	
	list(a=0.1,b_fec=0.0005,sigma=c(.25,0.25))
	list(a=0.0,b_fec=0.0005,sigma=c(.25,0.25))
	list(a=-0.1,b_fec=0.0005,sigma=c(.25,0.25))
	}
params<- c("a","b_fec","fec_mu","sigma")	

out1 <- jags(data=adat,
	inits=inits,
	parameters=params,	
	model.file=mod1,
	n.chains = 3,	
	n.iter = 50000,	
	n.burnin = 25000, 
	n.thin=2,
	working.directory=getwd())

	
	
mod2<- function()
	{
	# WEIGHT AS AN OFFSET
	for(i in 1:n)
		{
		log(fec_mu[i])<- a[X[i,3]] + b_fec*X[i,2] + log(X[i,1]) + disp[i]
		disp[i]~dnorm(0,prec_sigma[X[i,3]])
		X[i,4]~dpois(fec_mu[i])	
		}
	# PRIORS
	a[1]~dnorm(0,0.001)
	a[2]~dnorm(0,0.001)
	b_fec~dnorm(0,0.001)
	sigma[1]~dunif(0.00001, 5)
	sigma[2]~dunif(0.00001, 5)
	prec_sigma[1] <-pow(sigma[1],-2)
	prec_sigma[2] <-pow(sigma[2],-2)
	}
params<- c("a","b_fec","fec_mu","sigma")		
inits<- function(t)
	{	
	list(a=c(0.1,0),b_fec=0.0005,sigma=c(.25,0.25))
	list(a=c(0.1,0),b_fec=0.0005,sigma=c(.25,0.25))
	list(a=c(0.1,0),b_fec=0.0005,sigma=c(.25,0.25))
	}	
out2 <- jags(data=adat,
	inits=inits,
	parameters=params,	
	model.file=mod2,
	n.chains = 3,	
	n.iter = 50000,	
	n.burnin = 25000, 
	n.thin=2,
	working.directory=getwd())


mod3<- function()
	{
	# WEIGHT AS AN OFFSET
	for(i in 1:n)
		{
		log(fec_mu[i])<- a[X[i,3]] + b_fec[X[i,3]]*X[i,2] + log(X[i,1]) + disp[i]
		disp[i]~dnorm(0,prec_sigma[X[i,3]])
		X[i,4]~dpois(fec_mu[i])	
		}
	# PRIORS
	a[1]~dnorm(0,0.001)
	a[2]~dnorm(0,0.001)
	b_fec[1]~dnorm(0,0.001)
	b_fec[2]~dnorm(0,0.001)
	sigma[1]~dunif(0.00001, 5)
	sigma[2]~dunif(0.00001, 5)
	prec_sigma[1] <-pow(sigma[1],-2)
	prec_sigma[2] <-pow(sigma[2],-2)
	}
params<- c("a","b_fec","fec_mu","sigma")		
inits<- function(t)
	{	
	list(a=c(0.1,0),b_fec=c(0.0005,0),sigma=c(.25,0.25))
	list(a=c(0.1,0),b_fec=c(0.0005,0),sigma=c(.25,0.25))
	list(a=c(0.1,0),b_fec=c(0.0005,0),sigma=c(.25,0.25))
	}	
out3 <- jags(data=adat,
	inits=inits,
	parameters=params,	
	model.file=mod3,
	n.chains = 3,	
	n.iter = 50000,	
	n.burnin = 25000, 
	n.thin=2,
	working.directory=getwd())

save(out00,out0,out1,out2,out3,file="./output/bugs-output.Rdata")


	
	

mod4<- function()
	{
	# WEIGHT AS AN OFFSET
	for(i in 1:n)
		{
		log(fec_mu[i])<- a + b_fec*X[i,1] +log(X[i,2])
		#disp[i]~dnorm(0,prec_sigma)
		X[i,4]~dpois(fec_mu[i])	
		}
	# PRIORS
	a~dnorm(0,0.001)
	b_fec~dnorm(0,0.001)
	#sigma~dunif(0.00001, 5)
	#prec_sigma[1] <-pow(sigma,-2)
	}
	

inits<- function(t)
	{	
	list(a=0.1,b_fec=0.0005)
	list(a=0.1,b_fec=0.0005)
	list(a=0.1,b_fec=0.0005)
	}

out4 <- jags(data=adat,
	inits=inits,
	parameters=c("a","b_fec","fec_mu"),	
	model.file=mod4,
	n.chains = 3,	
	n.iter = 50000,	
	n.burnin = 25000, 
	n.thin=2,
	working.directory=getwd())
	
	
	
out4
out00	
	
	
plot(adat$X[,4],out4$BUGSoutput$mean$fec_mu);abline(0,1)
points(adat$X[,4],out00$BUGSoutput$mean$fec_mu,col='red')
	
sum(dpois(adat$X[,4],out4$BUGSoutput$mean$fec_mu,log=TRUE))*-2
sum(dpois(adat$X[,4],out00$BUGSoutput$mean$fec_mu,log=TRUE))*-2
	
	
	
