

mod1<- function()
	{# FABENS MODEL WITH RANDOM L_INF	
	for(i in 1:N)
		{
		# MODEL		
		L2[i]<- (Linfi[i]-L1[i])*(1-exp(-k*dY[i]))+L1[i]
		LL2[i]<-log(L2[i])	# 

		# LIKLIHOOD
		Y[i]~dlnorm(LL2[i],prec_obs)
		Linfi[i]~dnorm(Linf,prec_Linf)
		}
	# HYPER PARAMETER PRIORS
	k~dunif(0.001,0.2)
	
	# Linfinity
	Linf~dunif(800,2000)
	sigma_Linf ~ dunif (0, 10)		
	prec_Linf <-  pow(sigma_Linf,-2)
	
	# OBSERVATION
	sigma_obs  ~ dunif (0, 10)	
	prec_obs <-pow(sigma_obs,-2)
	}
mod2<- function()
	{# FABENS MODEL WITH VARYING L_INF AND K
	for(i in 1:N)
		{
		# MODEL		
		L2[i]<- (Linfi[i]-L1[i])*(1-exp(-Lki[i]*dY[i]))+L1[i]

		LL2[i]<-log(L2[i])	# 

		# LIKLIHOOD
		Y[i]~dlnorm(LL2[i],prec_obs)
		Linfi[i]~dlnorm(LLinf,prec_Linf)
		Lki[i]~dlnorm(LLk,prec_k)
		}
	# HYPER PARAMETER PRIORS
	k~dunif(0.001,0.2)
	LLk<- log(k)
	sigma_k ~ dunif (0, 10)		
	prec_k <-  pow(sigma_k,-2)	
	
	# Linfinity
	Linf~dunif(800,2500)
	LLinf<- log(Linf)
	sigma_Linf ~ dunif (0, 10)		
	prec_Linf <-  pow(sigma_Linf,-2)
	
	# OBSERVATION
	sigma_obs  ~ dunif (0, 10)	
	prec_obs <-pow(sigma_obs,-2)
	}

mod3<- function()
	{# FABENS MODEL WITH VARYING L_INF AND K
	for(i in 1:N)
		{
		# MODEL	
		ki[i]<- exp(v[ind_id[i],2])
		Linf[i]<- exp(v[ind_id[i],1])
		L2[i]<- (Linf[i]-L1[i])*(1-exp(-ki[i]*dY[i]))+L1[i]
		#LL2[i]<- log(L2[i])
		# LIKLIHOOD
		Y[i]~dnorm(L2[i],prec_obs)
		}
	# INDVIDUALS	
	for(j in 1:N_inds)
		{
		#Linfi[i]~dnorm(Linf,prec_Linf) # INDVIDUAL Linf
		#Lki[i]~dnorm(lnk,prec_k)# INDIVIDUAL K		
		v[j,1:2] ~ dmnorm(beta[], prec.Sigma[,]) # 1 = lint, 2 = k
		}
	beta[1] ~ dnorm(0.0,1.0E-3)# overall mean ln(LINF)
	beta[2] ~ dnorm(0.0,1.0E-3)# overall mean ln(k)

	
	Sigma[1:2,1:2] <- inverse(prec.Sigma[,])
	prec.Sigma[1:2, 1:2] ~ dwish(Omega[,], 2)	
	Omega[1,1] <- 10
	Omega[2,2] <- 10
	Omega[1,2] <- 0
	Omega[2,1] <- 0		
		
	# OBSERVATION
	sigma_obs  ~ dgamma(0.001,0.001)
	prec_obs <-pow(sigma_obs,-2)
	}	
	
	
	
	
	
	
# GOMPERTZ
mod1_gomp<- function()
	{# FABENS MODEL WITH RANDOM L_INF	
	for(i in 1:N)
		{
		# MODEL		
		L2[i]<- Linfi[i]*(L1[i]/Linfi[i])*(exp(k*dY[i]))+L1[i]
		LL2[i]<-log(L2[i])	# 

		# LIKLIHOOD
		Y[i]~dlnorm(LL2[i],prec_obs)
		Linfi[i]~dlnorm(LLinf,prec_Linf)
		}
	# HYPER PARAMETER PRIORS
	k~dunif(0.001,0.2)
	
	# Linfinity
	Linf~dunif(800,2000)
	LLinf<- log(Linf)
	sigma_Linf ~ dunif (0, 10)		
	prec_Linf <-  pow(sigma_Linf,-2)
	
	# OBSERVATION
	sigma_obs  ~ dunif (0, 10)	
	prec_obs <-pow(sigma_obs,-2)
	}	
	
	
mod2_gomp<- function()
	{# GOMPERTZ
	for(i in 1:N)
		{
		# MODEL	
		L2[i]<- Linfi[i]*(L1[i]/Linfi[i])*(exp(Lki[i]*dY[i]))+L1[i]
		LL2[i]<-log(L2[i])	# 

		# LIKLIHOOD
		Y[i]~dlnorm(LL2[i],prec_obs)
		Linfi[i]~dlnorm(LLinf,prec_Linf)
		Lki[i]~dlnorm(LLk,prec_k)
		}
	# HYPER PARAMETER PRIORS
	k~dunif(0.001,0.2)
	LLk<- log(k)
	sigma_k ~ dunif (0, 10)		
	prec_k <-  pow(sigma_k,-2)	
	
	# Linfinity
	Linf~dunif(800,2500)
	LLinf<- log(Linf)
	sigma_Linf ~ dunif (0, 10)		
	prec_Linf <-  pow(sigma_Linf,-2)
	
	# OBSERVATION
	sigma_obs  ~ dunif (0, 10)	
	prec_obs <-pow(sigma_obs,-2)
	}