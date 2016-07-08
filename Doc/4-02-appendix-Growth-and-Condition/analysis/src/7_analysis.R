adat<- tables(4)

ni<- 500000	#	n.iter
nb<- 200000	#	n.burnin



# inverse logistic
dL=(max dL)/(1+exp(ln(19)((l_i-L50)/(l95-l50))))+error

# scnute model
dL = [L_i^b * exp(-a*dt) = c*(1-exp(-a*dt))]^(1/b) + er  
a != 0 and b !=0 

# linear
dL = b*dt + error

# BUNDLE UP DATA
## LOWER BASIN	
lower<- subset(adat,basin=="lower")
lower<- list(L1=lower$l1,Y=lower$l2,dY=lower$dy,ind_id=lower$ind_id, N_inds= max(lower$ind_id),N=nrow(lower))	

## MODEL 1: VARYING LINF AMONG INDVIDUALS
inits<- function(t)
	{	
	list(k=0.1,Linf=1500,sigma_obs=.25,Linfi=lower$l2*1.5,sigma_Linf=.25)
	list(k=0.05,Linf=1500,sigma_obs=.25,Linfi=lower$l2*1.5,sigma_Linf=.25)
	list(k=0.01,Linf=1500,sigma_obs=.25,Linfi=lower$l2*1.5,sigma_Linf=.25)
	}
params<- c("k","Linf","sigma_obs","sigma_Linf")	

out_lower <- jags(data=lower,
	inits=inits,
	parameters=params,	model.file=mod1,
	n.chains = 3,	
	n.iter = ni,	
	n.burnin = nb,
	n.thin=2,
	working.directory=getwd())
save(out_lower, file = "./output/out_lower-mod1.RData")
## GOMPERTZ MODEL WITH VARYING LINF AMONG INDVIDUALS
out_lower <- jags(data=lower,
	inits=inits,
	parameters=params,	model.file=mod1_gomp,
	n.chains = 3,	
	n.iter = ni,	
	n.burnin = nb,
	n.thin=2,
	working.directory=getwd()) 
save(out_lower, file = "./output/out_lower-mod1-gomp.RData")	

## MODEL 3 WITH CORRELATION BETWEEN Linf AND K
inits<- function(t)
	{	
	list(beta=c(7.5,0),sigma_obs=.25)
	list(beta=c(7.5,0),sigma_obs=.25)
	list(beta=c(7.5,0),sigma_obs=.25)
	}
params<- c("beta","Sigma","sigma_obs")
out_lower <-  jags.parallel(data=lower,
	inits=inits,
	parameters=params,	model.file=mod3,
	n.chains = 3,	
	n.iter = 500000,	
	n.burnin = 200000,
	n.thin=2,
	working.directory=getwd())
save(out_lower, file = "./output/out_lower-mod3.RData")
 






## UPPER BASIN
inits<- function(t)
	{	
	list(k=0.1,Linf=1500,sigma_obs=.25,Linfi=upper$l2*1.5,sigma_Linf=.25)
	list(k=0.05,Linf=1500,sigma_obs=.25,Linfi=upper$l2*1.5,sigma_Linf=.25)
	list(k=0.01,Linf=1500,sigma_obs=.25,Linfi=upper$l2*1.5,sigma_Linf=.25)
	}
upper<- subset(adat,basin=="upper")
dat<- list(L1=upper$l1,Y=upper$l2,dY=upper$dy,N=nrow(upper))
out_upper <- jags(data=dat,
	inits=inits,
	parameters=params,	model.file=mod1,
	n.chains = 3,	
	n.iter = ni,	
	n.burnin = nb,
	n.thin=2,
	working.directory=getwd())
save(out_upper, file = "./output/out_upper-mod1.RData")	

out_upper <- jags(data=dat,
	inits=inits,
	parameters=params,	model.file=mod1_gomp,
	n.chains = 3,	
	n.iter = ni,	
	n.burnin = nb,
	n.thin=2,
	working.directory=getwd())
save(out_upper, file = "./output/out_upper-mod1-gomp.RData")	



# MODEL 2: VARYING LINF AND K
inits<- function(t)
	{	
	list(k=0.066,Linf=1500,sigma_obs=.25,Linfi=lower$l2*1.5,sigma_Linf=.25,Lki=rep(0.066,nrow(lower)),sigma_k=0.3)
	list(k=0.066,Linf=1500,sigma_obs=.25,Linfi=lower$l2*1.5,sigma_Linf=.25,Lki=rep(0.066,nrow(lower)),sigma_k=0.3)
	list(k=0.066,Linf=1500,sigma_obs=.25,Linfi=lower$l2*1.5,sigma_Linf=.25,Lki=rep(0.066,nrow(lower)),sigma_k=0.3)
	}
params<- c("k","Linf","sigma_obs","sigma_Linf","sigma_k")	
## LOWER BASIN
adat<- tables(4)
lower<- subset(adat,basin.x=="lower")
dat<- list(L1=lower$l1,Y=lower$l2,dY=lower$dy,N=nrow(lower))		
out_lower <- jags(data=dat,
	inits=inits,
	parameters=params,	model.file=mod2,
	n.chains = 3,	
	n.iter = ni,	
	n.burnin = nb,
	n.thin=2,
	working.directory=getwd())
save(out_lower, file = "./output/out_lower-mod2.RData")

out_lower <- jags(data=dat,
	inits=inits,
	parameters=params,	model.file=mod2_gomp,
	n.chains = 3,	
	n.iter = ni,	
	n.burnin = nb,
	n.thin=2,
	working.directory=getwd())
save(out_lower, file = "./output/out_lower-mod2-gomp.RData")

## UPPER BASIN	
inits<- function(t)
	{	
	list(k=0.066,Linf=1500,sigma_obs=.25,Linfi=upper$l2*1.5,sigma_Linf=.25,Lki=rep(0.066,nrow(upper)),sigma_k=0.3)
	list(k=0.066,Linf=1500,sigma_obs=.25,Linfi=upper$l2*1.5,sigma_Linf=.25,Lki=rep(0.066,nrow(upper)),sigma_k=0.3)
	list(k=0.066,Linf=1500,sigma_obs=.25,Linfi=upper$l2*1.5,sigma_Linf=.25,Lki=rep(0.066,nrow(upper)),sigma_k=0.3)
	}
upper<- subset(adat,basin=="upper")
dat<- list(L1=upper$l1,Y=upper$l2,dY=upper$dy,N=nrow(upper))
out_upper <- jags(data=dat,
	inits=inits,
	parameters=params,	model.file=mod2,
	n.chains = 3,	
	n.iter = ni,	
	n.burnin = nb,
	n.thin=2,
	working.directory=getwd())
save(out_upper, file = "./output/out_upper-mod2.RData")	
 
out_upper <- jags(data=dat,
	inits=inits,
	parameters=params,	model.file=mod2_gomp,
	n.chains = 3,	
	n.iter = ni,	
	n.burnin = nb,
	n.thin=2,
	working.directory=getwd())
save(out_upper, file = "./output/out_upper-mod2-gomp.RData")	


## MODEL 3 WITH CORRELATION BETWEEN Linf AND K
# BUNDLE UP DATA
## LOWER BASIN	
upper<- subset(adat,basin=="upper")
upper<- list(L1=upper$l1,Y=upper$l2,dY=upper$dy,ind_id=upper$ind_id, 
	N_inds= max(upper$ind_id),N=nrow(upper))	

inits<- function(t)
	{	
	list(beta=c(7.5,0),sigma_obs=.25)
	list(beta=c(7.5,0),sigma_obs=.25)
	list(beta=c(7.5,0),sigma_obs=.25)
	}
params<- c("beta","Sigma","sigma_obs")	
out <-  jags.parallel(data=upper,
	inits=inits,
	parameters=params,	model.file=mod3,
	n.chains = 3,	
	n.iter = 500000,	
	n.burnin = 200000,
	n.thin=2,
	working.directory=getwd())
save(out, file = "./output/out_upper-mod3.RData")




