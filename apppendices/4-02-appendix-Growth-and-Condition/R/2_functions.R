# MODEL 1 VBGF: RANDOM LINF
# MODEL 2 VBGF: RANDOM K
# MODEL 3 VBGF: RANDOM K & LINF
# MODEL 4 VBGF: RANDOM K & LINF
# MODEL 4 VBGF: RANDOM K & LINF
# GOMPERTZ
# INVERSE LOGISTIC
# SCHNUTE MODEL
# LINEAR
# POWER
# 

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
	sigma_obs  ~ dunif(0, 10)	
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
		
mod4a<- function()
	{# FABENS MODEL WITH VARYING L_INF AND K=a+b*linf
	for(i in 1:N)
		{
		# MODEL	
		ki[i]<- exp(a + b*Linfi[ind_id[i]])
		Linf_hat[i]<- exp(Linfi[ind_id[i]])
		L2[i]<- L1[i]+ (Linf_hat[i]-L1[i])*(1-exp(-ki[i]*dY[i]))
		# LIKLIHOOD
		Y[i]~dnorm(L2[i],prec_obs)
		}
	# LIINDVIDUALS	
	for(j in 1:N_inds)
		{
		Linfi[j]~dnorm(Linf,prec_Linf)%_%T(,2000) # INDVIDUAL Linf
		}
	
    # PREDICT AGE OF PS  
    t0<- -6.2021 #-2.056 RPMA 4; -6.2021 RPMA 2 SHUMAN   
    for(i in 1:n_val)
        {
        k_val[i]<-(exp(a + b*Linf))
        Linf_val[i]<- Linf
        
        age[i]<-log(1-((val_length[i]/Linf_val[i])))/-k_val[i] + t0
        }
    
    # PRIORS
	Linf~dunif(1000,2000)
	sigma_Linf ~ dunif (0, 10)		
	prec_Linf <-  pow(sigma_Linf,-2)

	#sigma_k ~ dunif (0, 10)		
	#prec_k <-  pow(sigma_k,-2)	
	
	a~dnorm(0,0.001)
	b~dnorm(0,0.001)
	
	# OBSERVATION
	sigma_obs  ~ dgamma(0.001,0.001)
	prec_obs <-pow(sigma_obs,-2)
	}	
		
mod4b<- function()
	{# FABENS MODEL WITH K~a+b*linf[basin]
	for(i in 1:N)
		{
		# MODEL	
		ki[i]<- exp(a[ind[i,2]] + 
			b[ind[i,2]]*Linfi[ind[i,1]])
		Linf_hat[i]<- exp(Linfi[ind[i,1]])
		L2[i]<- L1[i]+ 
			(Linf_hat[i]-L1[i])*
			(1-exp(-ki[i]*dY[i]))
		# LIKLIHOOD
		Y[i]~dnorm(L2[i],prec_obs[ind[i,2]])
		}
	# INDVIDUALS
	
	for(j in 1:N_inds)
		{
		Linfi[j] ~ dnorm(Linf[xx[j,2]],prec_Linf[xx[j,2]])			
		}

	# PRIORS
	for(basin in 1:2)
		{
		Linf[basin] ~dunif(0,10)
		sigma_Linf[basin] ~ dunif (0, 10)		
		prec_Linf[basin] <-  pow(sigma_Linf[basin],-2)
		
		a[basin]~dnorm(0,0.001)
		b[basin]~dnorm(0,0.001)
		
		# OBSERVATION
		sigma_obs[basin]  ~ dgamma(0.001,0.001)
		prec_obs[basin] <-pow(sigma_obs[basin],-2)
		}
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