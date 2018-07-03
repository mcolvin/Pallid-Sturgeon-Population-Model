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




mod0 <- function()
	{# FABENS MODEL WITH VARYING K
	for(i in 1:N)
		{
		# MODEL	
		Lki[i]~dlnorm(LLk,prec_k)  # draw k      
		L2[i]<- (Linf-L1[i])*(1-exp(-Lki[i]*dY[i]))+L1[i]
		LL2[i]<-log(L2[i])	# 

		# LIKLIHOOD
        ## ASSUMES LOG NORMAL
		Y[i]~dlnorm(LL2[i],prec_obs)
		}

        
    # PREDICT AGE OF PS  
    t0<- -6.2021 #-2.056 RPMA 4; -6.2021 RPMA 2 SHUMAN 
    for(i in 1:n_val)
       {
       age[i]<- -1/k*log(1-val_length[i]/Linf) + t0
       }    
       
    # PRIORS    
	## HYPER PARAMETER PRIORS
	k~dunif(0.001,0.2)
	LLk<- log(k)
	sigma_k ~ dunif(0, 10)		
	prec_k <-  pow(sigma_k,-2)	
	
	## Linfinity
	Linf~dunif(maxY,2500)# truncate to largest fish
	
	## OBSERVATION
	sigma_obs  ~ dunif(0, 10)	
	prec_obs <-pow(sigma_obs,-2)
	}

mod00 <- function()
	{
    # FABENS MODEL WITH VARYING K
	for(i in 1:N)
		{
		# MODEL	
		L2[i]<- (Linf-L1[i])*(1-exp(-k*dY[i]))+L1[i]
		LL2[i]<-log(L2[i])	# assume log normal error

		# LIKLIHOOD
        ## ASSUMES LOG NORMAL
		Y[i]~dlnorm(LL2[i],tau)

		}
    for(age in 1:250)
        {
        l_age_mu[age]<- Linf * (1 - exp(-k * (age/10-t0))) # PREDICTED LENGTH AT AGE 0.1 INCREMENT
        log_l_age_mu[age]<- log(l_age_mu[age])
        l_age[age]~dlnorm(log_l_age_mu[age],tau) # FOR PREDICTION INTERVAL
        }
        
    # PREDICT AGE OF PS  
    t0<- -6.2021 #-2.056 RPMA 4; -6.2021 RPMA 2 SHUMAN 
    for(i in 1:n_val)
       {
       age[i]<- -1/k*log(1-val_length[i]/Linf) + t0
       }    
       
    # PRIORS    
	## HYPER PARAMETER PRIORS
	k~dunif(0.001,0.2)
		
	## Linfinity
	Linf~dunif(maxY,2500)# truncate to largest fish
	
	## OBSERVATION
	sigma2  ~ dunif(0, 10)	
	tau <-pow(sigma2,-2)
	}
    
    
  


#mod00 <- function()
#	{# FABENS MODEL WITH VARYING K
#	for(i in 1:N)
#		{
#		# MODEL	
#		L2[i]<- (Linf-L1[i])*(1-exp(-k*dY[i]))+L1[i]
#		LL2[i]<-log(L2[i])	# assume log normal error
		# LIKLIHOOD
        ## ASSUMES LOG NORMAL
#		Y[i]~dlnorm(LL2[i],prec_obs)
#		}        
    # PREDICT AGE OF PS  
#    t0<- -6.2021 #-2.056 RPMA 4; -6.2021 RPMA 2 SHUMAN 
#    for(i in 1:n_val)
#       {
#       age[i]<- -1/k*log(1-val_length[i]/Linf) + t0
#       }       
    # PRIORS    
	## HYPER PARAMETER PRIORS
#	k~dunif(0.001,0.2)		
	## Linfinity
#	Linf~dunif(maxY,2500)# truncate to largest fish	
	## OBSERVATION
#	sigma_obs  ~ dunif(0, 10)	
#	prec_obs <-pow(sigma_obs,-2)
#	}
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
 	## INDVIDUAL K AND LINF	
	for(j in 1:N_inds)
		{
		#Linfi[i]~dnorm(Linf,prec_Linf) # INDVIDUAL Linf
		#Lki[i]~dnorm(lnk,prec_k)# INDIVIDUAL K		
		v[j,1:2] ~ dmnorm(beta[], prec.Sigma[,]) # 1 = lint, 2 = k
		} 

        
    # PREDICT AGE OF PS  
    t0<- -6.2021 #-2.056 RPMA 4; -6.2021 RPMA 2 SHUMAN 
    Linf_val<- exp(beta[1])
    k_val<- exp(beta[2])
    #for(i in 1:n_val)
    #    {
     #   age[i]<- -1/k_val*log(1-val_length[i]/Linf_val) + t0
     #   }    
   
    # DERIVED
    Linf_hat<- exp(beta[1])
    k_hat<- exp(beta[2])   
    
    
    # PRIORS    
	beta[1] ~ dnorm(0.0,1.0E-3)# overall mean ln(LINF)
	beta[2] ~ dnorm(0.0,1.0E-3)# overall mean ln(k)
	
	Sigma[1:2,1:2] <- inverse(prec.Sigma[,])
	prec.Sigma[1:2, 1:2] ~ dwish(Omega[,], 2)	
	Omega[1,1] <- 10
	Omega[2,2] <- 10
	Omega[1,2] <- 0
	Omega[2,1] <- 0		
		
	# OBSERVATION
	sigma_obs ~ dgamma(0.001,0.001)
	prec_obs <- pow(sigma_obs,-2)
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
        Linf_val[i]<- max(Linf, 1.1*val_length[i])
        k_val[i]<- exp(a + b*Linf_val[i])
       # age[i]<- -1/k_val[i]*log(1-val_length[i]/Linf_val[i]) + t0
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