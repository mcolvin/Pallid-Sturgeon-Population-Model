# DATA TABLE TO FIT FABENS MODEL
adat<- tables(4) 
val<- adat$val
adat<- adat$out

ni<- 500000	#	n.iter
nb<- 200000	#	n.burnin



upper<- subset(adat,basin=="upper")
linf_ini<- log(tapply(upper$l2, upper$ind_id,max)*1.25)  
upper<- list(L1=upper$l1,
	Y=upper$l2,
	maxY=max(upper$l2)+1,
	dY=upper$dy,
    #val_age=val$age,
    val_length=val$length,
    n_val=nrow(val),
	#ind_id=upper$ind_id, 
    #N_inds= max(upper$ind_id),
    N=nrow(upper),
    l_age=rep(NA,25))	
## MODEL 0 FIXED LINF AND VARYING K
inits<- function(t)
	{	
	list(k=0.1,Linf=1600,sigma_obs=100.25)#,sigma_k=.25)
	list(k=0.05,Linf=2000,sigma_obs=200.25)#,sigma_k=.25)
	list(k=0.01,Linf=1800,sigma_obs=500.25)#,sigma_k=.25)
	}
params<- c("k","Linf","sigma_obs","age","l_age")	

out_lower <- jags(data=upper,
	inits=inits,
	parameters=params,	
	model.file=mod00,
	n.chains = 3,	
	n.iter = 500,	
	n.burnin = 300,
	n.thin=2,
	working.directory=getwd())

ppp<- data.frame( 
    parameter= colnames(out_lower$BUGSoutput$sims.matrix),
    lci=apply(out_lower$BUGSoutput$sims.matrix,2,quantile, 0.025),
    est=apply(out_lower$BUGSoutput$sims.matrix,2,quantile, 0.5),
    uci=apply(out_lower$BUGSoutput$sims.matrix,2,quantile, 0.975))

pdat<- ppp[grep("l_age",ppp$parameter),]
pdat$age<-c(1:nrow(pdat))  
    
    
plot(est~age,pdat)
points(lci~age,pdat,type='l',lty=2)
points(uci~age,pdat,type='l',lty=2)


out_lower$BUGSoutput$summary
plot(val$age~out_lower$BUGSoutput$mean$age,ylim=c(0,25),xlim=c(0,25))
abline(0,1,lty=2)



plot(length~age,dat, subset=basin=="lower")
points(est~age,pdat,col="red")



## MODEL 4a ln(k)~a+b*ln(linf)
upper<- subset(adat,basin=="upper")
linf_ini<- log(tapply(upper$l2, upper$ind_id,max)*1.25)  
upper<- list(L1=upper$l1,
	Y=upper$l2,
	dY=upper$dy,
    #val_age=val$age,
    val_length=val$length,
    n_val=nrow(val),
	ind_id=upper$ind_id, 
    N_inds= max(upper$ind_id),
    N=nrow(upper))	
inits<- function(t)
	{	
	list(a=0,b=0,Linf=1500,sigma_obs=.25,Linfi=linf_ini,sigma_Linf=.25)
	list(a=0,b=0,Linf=1500,sigma_obs=.25,Linfi=linf_ini,sigma_Linf=.25)
	list(a=0,b=0,Linf=1500,sigma_obs=.25,Linfi=linf_ini,sigma_Linf=.25)
	}
params<- c("a","b","Linf","prec_obs","sigma_Linf","k_val")
out_upper <-  jags(data=upper,
	inits=inits,
	parameters=params,	
	model.file=mod4a,
	n.chains = 3,	
	n.iter = 300,	
	n.burnin = 100,
	n.thin=2,
	#export_obj_names=c("linf_ini",# need this because it is called in inits
    #    "upper",
    #    "inits",
    #    "params"),# export to cores
	working.directory=getwd())
	
save(out_upper, file = "./output/out_upper-mod3b.RData")


## MODEL 4b: basin effect ln(k)~a+b*ln(linf)

dat<- list(L1=adat$l1,
	Y=adat$l2,
	dY=adat$dy,
	ind=as.matrix(cbind(adat$ind_id,adat$basin_id)), 
	N_inds= max(adat$ind_id),
	xx = as.matrix(aggregate(L1~ind_id+basin_id,adat,length)[,-3]),
	N=nrow(adat))
	
inits<- function(t)
	{	
	list(a=runif(2),
		b=runif(2),
		sigma_obs=runif(2),
		Linfi=log(tapply(dat$Y, dat$ind[,1],max)*1.25),
		Linf=c(6.5,6.5),		
		sigma_Linf=runif(2))		
	}
params<- c("a","b","Linf","prec_obs","sigma_Linf")
out_upper <-  jags.parallel(data=dat,
	inits=inits,
	parameters=params,	
	model.file=mod4b,
	n.chains = 3,	
	n.iter = 300,	
	n.burnin = 100,
	n.thin=2,
	export_obj_names=c("dat","inits","params"),
	working.directory=getwd())
	
save(out_upper, file = "./output/out_upper-mod3b.RData")





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
	parameters=params,	
	model.file=mod1,
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




