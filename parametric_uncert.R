
library(truncnorm)

ab<- function(mn,variance)
	{
	a<- mn*((mn*(1-mn)/variance)-1)
	b<- (1-mn)*((mn*(1-mn)/variance)-1)
	return(c(a,b))
	}


# ASSESSMENT OF MCGOWAN ET AL.  

x<- 1000

# SURVIVAL
mn_surv<- 0.92
vr_surv<- 0.005
# UNCERTAINTY FOR SURVIVAL
var_mean<-0.01
var_sd<- 0.01

# SAMPLING VARIATION:  MEAN AND VARIANCE VARY AMONG ITERATIONS
vals<-ab(mn_surv,vr_surv)
Phi_mean<- rbeta(1,shape1=vals[1],shape2=vals[2])
var_sampling<- rtruncnorm(1,a=0,b=Inf, mean=var_mean, sd=var_sd)
# TEMPORAL VARIATION: 
nyears=100

vals<-ab(Phi_mean,var_sampling)

Phi_year<- rbeta(nyears,shape1=vals[1],shape2=vals[2])

N<- rep(0, nyears)
N[1]<- 10000
for(i in 2:nyears)
	{
	N[i]<-rbinom(1,N[i-1],Phi_year[i-1])
	}

plot(N)


## SAME AS ABOVE BUT USING A LOGIT LINK