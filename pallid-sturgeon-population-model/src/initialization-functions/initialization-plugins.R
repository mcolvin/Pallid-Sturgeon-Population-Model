
#### FUNCTIONS TO ASSIGN RKM TO BENDS FOR UPPER AND LOWER
#rkm_start<-seq(1,1000,length=317)
#bend<- c(1:317)
#rkm2bend<- approxfun(rkm_start,bend,method='constant')
#bend2rkm<- approxfun(bend,rkm_start,method='constant')


## INITIALIZATION PLUGINS
ini_mps<- function(n,mature)
	{# months since spawning
	out<-sample(c(1:4),size=n,replace=TRUE)*mature*12
	return(out)
	}
	
	
ini_wgt<- function(a,b,len,er)
	{
    ypred<- log(a)+b*log(len)
    out<-exp(rnorm(length(len),ypred,er))
    return(out)
  }
	
	
ini_sex<- function(n,ratio)
	{
	out<-rbinom(n,1,ratio)
	return(out)
	}
	

ini_age<- function(len,linf,k,sizeAtHatch=7,maxAge)
	{
	age<-ifelse(len==0,0,log(-1*(len-sizeAtHatch)/(linf-sizeAtHatch)+1)/-k)
	age<- ifelse(age>maxAge,maxAge,age)
	return(age*12)	
	}
	
	
	
	
	

# FUNCTION TO INIITIALIZE LENGTH OF FISH	
ini_length<-function(n=10, basin="lower",origin=1, spatial=FALSE,linf=2000)
        {# A FUNCTION TO INITIALIZE LENGTHS OF INDIVIDUAL FISH
        # origin [0 for natural, 1 for hatchery]
		# CALLS r WHICH IS AN EMPRICAL DISTRIBUTION
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
                {####fixme####
                tmp<- r(len_ini_low_hatchery_nospace)(n)*origin + # hatchery
                        r(len_ini_low_natural_nospace)(n)*(1-origin) # natural				
				
                
               # tmp<- r(len_ini_low_hatchery_nospace)(n)*origin + # hatchery
                #        r(len_ini_low_natural_nospace)(n)*(1-origin) # natural
                }
        if(tolower(basin)=="upper" & spatial==TRUE )
                {####fixme####
				tmp<- r(len_ini_upp_hatchery_nospace)(n)*origin + # hatchery
					r(len_ini_upp_natural_nospace)(n)*(1-origin) # natural
                
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
