
#### FUNCTIONS TO ASSIGN RKM TO BENDS FOR UPPER AND LOWER
rkm_start<-seq(1,1000,length=317)
bend<- c(1:317)
rkm2bend<- approxfun(rkm_start,bend,method='constant')
bend2rkm<- approxfun(bend,rkm_start,method='constant')


## INITIALIZATION PLUGINS
ini_mps<- function(n,mature)
	{# months since spawning
	out<-sample(c(1:4),size=n,replace=TRUE)*mature*12
	return(out)
	}
	
	
ini_wgt<- function(a,b,len,er)
	{
	rlnorm(length(len),log(a*len^b),er)
	}
	
	
ini_sex<- function(n,ratio)
	{
	out<-rbinom(n,1,ratio)
	return(out)
	}

	
ini_age<- function(len,linf,k,sizeAtHatch=7,maxAge)
	{
	length2<- len
	age<-sapply(1:length(len),function(x)
		{solve(-k[x],log(1-((length2[x]-sizeAtHatch)/(linf[x]-sizeAtHatch))))})
	out<- ifelse(age>maxAge,maxAge,age)
	return(out*12)	
	}
	

# FUNCTION TO INIITIALIZE LENGTH OF FISH	
ini_length<-function(n=10, basin="lower",origin=1, spatial=FALSE,linf=2000)
        {# A FUNCTION TO INITIALIZE LENGTHS OF INDIVIDUAL FISH
        # origin [0 for natural, 1 for hatchery]
        if(tolower(basin)=="lower" & spatial==FALSE)
                {
                tmp<- r(len_ini_low_hatchery_nospace)(n)*origin + # hatchery
                        r(len_ini_low_natural_nospace)(n)*(1-origin) # natural
                }
        if(tolower(basin)=="upper" & spatial==FALSE)
                {
                tmp<- r(len_ini_upp_hatchery_nospace)(n)*origin + # hatchery
                        r(len_ini_upp_natural_nospace)(n)*(1-origin) # natural
                }
        # SPATIAL COMPONENT
        if(tolower(basin)=="lower" & spatial==TRUE )
                {
                ####fixme####
               # tmp<- r(len_ini_low_hatchery_nospace)(n)*origin + # hatchery
                #        r(len_ini_low_natural_nospace)(n)*(1-origin) # natural
                }
        if(tolower(basin)=="upper" & spatial==TRUE )
                {
                ####fixme####
                #tmp<-  r(len_ini_upp_hatchery_nospace)(n)*origin + # hatchery
                 #       r(len_ini_upp_natural_nospace)(n)*(1-origin) # natural
                }
		tmp<- ifelse(tmp> linf,0.99*linf,tmp)
		return(tmp)
        }


# INITIALIZE HOW MANY FISH ARE MATURE
ini_maturity<- function(k,len,age_mat)
	{
	# IS A FISH SEXUALLY MATURE; CONDITIONAL ON BEING ALIVE
	p<- (1/(1+exp(-k*len-age_mat*12))) ####fixme####
	M2<- rbinom(length(len),1,p)	
	return(M2)				
	}
	
# INITIALIZE RIVER LOCATION
ini_rkm<- function(n,type,bend_lengths)
	{
	# FUNCTION TO INITIALIZE RIVER 
	# KILOMETER FOR INVIDUAL FISH
	nbends<- length(bend_lengths)
	# BUILD INVERSE DISTRIBUTATION TO SAMPLE FROM
	if(type=="Uniform")
		{
		y<- rep(1, nbends)*bend_lengths
		y<- cumsum(y)/sum(y)
		cumdist<- approxfun(y,c(1:nbends),rule=2)
		}
	if(type=="Emperical")  ####fixme####
		{
		y<- rep(1, nbends)*bend_lengths
		y<- cumsum(y)/sum(y)
		cumdist<- approxfun(y,c(1:nbends),rule=2)
		}
	x<-cumdist(runif(n))
	return(x)
	}
	
# INITIALIZE GROWTH PARAMETERS (L_INF, K)	
ini_growth<- function(x,n,mu_ln_linf,mu_ln_k,vcv)
	{
	tmp<-mvrnorm(x*n,c(mu_ln_linf, mu_ln_k),
		matrix(vcv,2,2,byrow=TRUE))
	tmp<- exp(tmp)
	return(list(linf=matrix(tmp[,1],n,x),k=matrix(tmp[,2],n,x)))
	}
