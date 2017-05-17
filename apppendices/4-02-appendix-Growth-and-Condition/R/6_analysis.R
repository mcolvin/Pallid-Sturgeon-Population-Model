



mod00 <- function()
	{
    # INVERSE VBGF FOR FISH OF KNOWN AGE
    for(i in 1:n)
        {
        La_hat[i]<- Linf*(1-exp(-k*(age[i]-t0)))
        ln_La_hat[i]<- log(La_hat[i])
        La[i]~dlnorm(ln_La_hat[i], tau)
        }
    # PREDICT VALIDATION HOLDOUTS AGE 0 TO 5
    for(i in 1:v)
        {
        age_val[i]<- t0 - 1/k * log(1-La_val[i]/Linf)
        }    
    
    # PRIORS    
	k~dunif(0.001,0.2)
	Linf~dunif(800,2500)# truncate to largest fish
    t0~ dunif(-10,10)	
	
    ## OBSERVATION
	sigma2  ~ dunif(0, 20)	
	tau <-pow(sigma2,-2)
    

	}
    
ndat<- subset(dat, basin=="lower"& validate==0 & age>0 & age < 20 & length > 200)
vdat<- subset(dat, basin=="lower"& validate==1 & age>0)

d<- list(
    n=nrow(ndat),
    v=nrow(vdat),
    age=ndat$age,
    age_val=rep(NA,nrow(vdat)),
	La=ndat$length,
	La_val=vdat$length)	

## MODEL 0 FIXED LINF AND VARYING K
inits<- function(t)
	{	
	list(k=0.1,Linf=1600,sigma2=0.25,t0=0)
	list(k=0.05,Linf=2000,sigma2=0.25,t0=0)
	list(k=0.01,Linf=1800,sigma2=0.25,t0=0)
	}
params<- c("t0","k","Linf","age_val")	

out_lower <- jags(data=d,
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

pdat<- ppp[grep("age_val",ppp$parameter),]
plot(vdat$age~pdat$est);abline(0,1)

# CONFUSION MATRIX
table(floor(vdat$age),floor(pdat$est))

plot(length~age,ndat)
points(c(1:20),vbgf(linf= 1.731773e+03,t0=-4.989, k=3.412913e-02, age=c(1:20)),col='red')


pdat$age<-c(1:nrow(pdat))  
    
    
plot(length~age,ndat, xlab="True age",
    ylab="Length (mm)")
points(est~age,pdat,col="red",type='l',lwd=2) 
points(lci~age,pdat,type='l',lty=2,col='red')
points(uci~age,pdat,type='l',lty=2,col='red')
legend("topleft", c("Expected value (median)","95% BCI"),
    col="red",lty=c(1,2))
   
   
plot(est~age,pdat)
points(lci~age,pdat,type='l',lty=2)
points(uci~age,pdat,type='l',lty=2)

## HOW DOES THE MODEL DO PREDICTING 
## AGE FROM A GIVEN LENGTH?
pdat<- ppp[grep("age",ppp$parameter),]
pdat<- pdat[-grep("l_age",pdat$parameter),]
pdat$age<-val$age  
plot(age~est,pdat,ylim=c(0,25),xlim=c(0,25),
    ylab="True age", xlab="Predicted age")
segments(pdat$est, pdat$lci, pdat$est, pdat$uci)
abline(0,1,lty=2)


## SAMPLE THE POSTERIOR TO 
## CALCULATE CONDITONAL PROBABILITIES  
ppp<- data.frame( 
    parameter= colnames(out_lower$BUGSoutput$sims.matrix),
    lci=apply(out_lower$BUGSoutput$sims.matrix,2,quantile, 0.025),
    est=apply(out_lower$BUGSoutput$sims.matrix,2,quantile, 0.5),
    uci=apply(out_lower$BUGSoutput$sims.matrix,2,quantile, 0.975))

pdat<- ppp[grep("l_age",ppp$parameter),]
pdat$age<-c(1:nrow(pdat))  
    




library(lattice)
xyplot(length~age|as.factor(year),dat, subset=basin=="upper", xlab="True age",
    ylab="Length (mm)")
xyplot(length~age|as.factor(year),dat, subset=basin=="lower", xlab="True age",
    ylab="Length (mm)")





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




