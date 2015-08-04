## GLOBAL
# MATURITY FUNCTION JUVENILES TO ADULTS
mat_fun<- function(age, mid=5, high=9)
	{
	k= log(99)/(high-mid)# solve for K given mid point and age at 99% 
	p<- 1/(1+exp(-k*(age-mid)))
	return(p)
	}
# MATURITY FUNCTION RECRUDESCENT ADULTS TO SPAWNING ADULTS
sp_fun<-function(yr, mid=2, high=4)
	{
	k= log(99)/(high-mid)# solve for K given mid point and age at 99% 
	p<- 1/(1+exp(-k*(yr-mid)))
	return(p)
	}





## SERVER
inits_ind<- function(input=input)
	{# FUNCTION TO INITIALIZE AGE STRUCTURED MODEL INPUTS
	# FUNCTION RETURNS THE NUMBER OF POST SPAWN PALLID STURGEON (JUVENILES, ADULTS, EMBRYOS)
	# TO INITIALIZE THE MODEL. 
	
	# SURVIVAL AGE-1 TO MAX AGE
    S<- c(logit(rnorm(1,log(-input$S1/(input$S1-1)),abs(input$S1_cv*mn))),
		logit(rnorm(1,log(-input$S2/(input$S2-1)),abs(input$S2_cv*mn))),
		rep(logit(rnorm(1,log(-input$S3plus/(input$S3plus-1)),abs(input$S3plus_cv*mn))), input$maxAge-2))
	
	
	# JUVENILES	
	## INITIALIZE NUMBER OF JUVENILES BY ORIGIN	
	out<- data.frame(origin=c(rep("h",input$juv_ini_h), rep("n",input$juv_ini_n)),sex=NA,age=NA,yr_since_spawn=-1)
	## ASSIGN SEX
	out$sex<- sample(c('m','f'),nrow(out),replace=TRUE,prob=c(0.5,0.5))# assumes 50:50 for juveniles
	## ASSIGN AGE
	out$age<- sample(c(1:(input$mat_high-1)),nrow(out),replace=TRUE,prob=cumprod(S[1:(input$mat_high-1)]))
	## ASSIGN FORK LENGTH
	out$fl<- input$Linf*(1-exp(-input$K*(out$age-input$t0)))#*rnorm(nrow(out),1, 0.1)
	## ASSIGN YEARS SINCE SPAWNING
	out$yr_since_spawn<- -1	
	
	# ADULTS
	## INITIALIZE NUMBER OF ADULTS BY ORIGIN	
	yyy<- data.frame(origin=c(rep("h",input$adults_ini_h), rep("n",input$adults_ini_n)),sex=NA,age=NA,yr_since_spawn=-1)
	## ASSIGN SEX
	yyy[yyy$origin=="h",]$sex<- sample(c('m','f'),input$adults_ini_h,replace=TRUE,prob=c(1-input$sr_h,input$sr_h))
	yyy[yyy$origin=="n",]$sex<- sample(c('m','f'),input$adults_ini_n,replace=TRUE,prob=c(1-input$sr_n,input$sr_n))
	## ASSIGN AGE
	yyy$age<- sample(c(input$mat_high:input$maxAge),nrow(yyy),replace=TRUE,prob=cumprod(S[input$mat_high:input$maxAge]))
	## ASSIGN FORK LENGTH
	yyy$fl<- input$Linf*(1-exp(-input$K*(yyy$age-input$t0)))#*rnorm(nrow(out),1, 0.1)
	## ASSIGN YEARS SINCE SPAWNING
	yyy$yr_since_spawn<- sample(c(0:4),nrow(yyy), replace=TRUE,prob=c(1,1,1,1,1))
	pop<- rbind(out, yyy)

	
	# AGE-0 
	## NUMBER OF AGE-0 FISH
	eggs<- round(sum(input$a_fec+pop[pop$yr_since_spawn==0 & pop$sex=='f',]$fl+input$b_fec),0)
	age0<- rbinom(1,eggs,logit(rnorm(1,log(-input$viableGam/(input$viableGam-1)),abs(input$viableGam_cv*mn)))) # NUMBER OF EMBRYOS
    return(list(pop=pop,age0=age0))
    }#})
  

xx_ind<- function(input=input)
	{
	
	xx<- data.frame() #SET UP DATAFRAME TO HOLD RESULTS
	
	for(j in 1:input$nreps)
		{
		S0<-logit(rnorm(1,log(-input$S0/(input$S0-1)),abs(input$S0_cv*mn)))
		
		# SURVIVAL FUNCTION
		surv_fun<- approxfun(c(0:input$maxAge),
			c(S0,
			logit(rnorm(1,log(-input$S1/(input$S1-1)),abs(input$S1_cv*mn))),
			logit(rnorm(1,log(-input$S2/(input$S2-1)),abs(input$S2_cv*mn))),
			rep(logit(rnorm(1,log(-input$S3plus/(input$S3plus-1)),abs(input$S3plus_cv*mn))), input$maxAge-2)))	
			
		# INITIAL NUMBERS AT T=0
		inits<- inits_ind(input)
		
		# JUVENILE AND ADULT STAGES
		pop<-inits$pop		# INITIAL POST SPAWN POPULATION 
		pop$p<- surv_fun(pop$age)# PROBABILITY OF SURVIVING TO NEXT YEAR
		pop$surv<- rbinom(nrow(pop),1,pop$p) # SURVIVE OR NOT (0,1) TO NEXT YEAR		
		

		# AGE-0 PRODUCTION
		## DID SPAWNING OCCUR
		spawn<- rbinom(1,1,(1/input$spawn_frequency))# DID SPAWNING OCCUR?
		## NUMBER OF EGGS PRODUCED
		eggs<- spawn*round(sum(input$a_fec+pop[pop$yr_since_spawn==0 & pop$sex=='f',]$fl+input$b_fec),0)
		## NUMBER OF VIABLE EMBRYOS PRODUCED
		embryos<- rbinom(1,eggs,logit(rnorm(1,log(-input$viableGam/(input$viableGam-1)),abs(input$viableGam_cv*mn))))
		## NUMBER OF VIABLE EMBRYOS SURVIVING TO RECRUIT TO AGE-1
		age0<- rbinom(1,embryos,S0) 
		## A PLACEHOLDER TO HELP HANDLE YEARS OF NO AGE-0 FISH
		recruits<- data.frame(origin='n',sex=NA,age=0,yr_since_spawn=-100,fl=NA,p=0,surv=0)
		if(age0>0){
			age0_m<-rbinom(1,age0,0.5)		
			recruits<- rbind(recruits,
				data.frame(origin='n',sex=c(rep('m',age0_m), rep('f', age0-age0_m)),age=0,
				yr_since_spawn=-1,fl=NA,p=1,surv=1))
			recruits$fl<- input$Linf*(1-exp(-input$K*(recruits$age-input$t0)))#*rnorm(nrow(out),1, 0.1)
			}
			
		# RECRUIT AGE-0 FISH TO THE POPULATION
		pop<- rbind(pop, recruits)
		pop<- subset(pop, yr_since_spawn>=-1)# to remove sham row prior to summary		
		
		# POPULATION LEVEL SUMMARY
		app<-dcast(pop, origin+sex~yr_since_spawn, value.var='fl',length,subset=.(age<38))
		app<-merge(expand.grid(origin=c("n",'h'),sex=c('f','m')),app,
			by=c('origin','sex'),all.x=TRUE)	
		app[is.na(app)]<-0
		app$year<- 0
		app$r<- j
		xx<-rbind.fill(xx,app)
		
		for(i in 1:input$nyears)
			{
			S0<-logit(rnorm(1,log(-input$S0/(input$S0-1)),abs(input$S0_cv*mn)))
		
			# SURVIVAL FUNCTION
			surv_fun<- approxfun(c(0:input$maxAge),
				c(S0,
				logit(rnorm(1,log(-input$S1/(input$S1-1)),abs(input$S1_cv*mn))),
				logit(rnorm(1,log(-input$S2/(input$S2-1)),abs(input$S2_cv*mn))),
				rep(logit(rnorm(1,log(-input$S3plus/(input$S3plus-1)),abs(input$S3plus_cv*mn))), input$maxAge-2)))	
			
			
			# t+1
			pop<- subset(pop,surv==1 & age<input$maxAge) # GET SURVIVORS
			# UPDATE AGE
			pop$age<- pop$age+1
			# UPDATE SIZE (GROWTH)
			pop$fl<- input$Linf*(1-exp(-input$K*(pop$age-input$t0)))#*rnorm(nrow(out),1, 0.1)

			
			# AGE-0 PRODUCTION
			## DID SPAWNING OCCUR
			spawn<- rbinom(1,1,(1/input$spawn_frequency))# DID SPAWNING OCCUR?
			## NUMBER OF EGGS PRODUCED
			eggs<- spawn*round(sum(input$a_fec+pop[pop$yr_since_spawn==0 & pop$sex=='f',]$fl+input$b_fec),0)
			## NUMBER OF VIABLE EMBRYOS PRODUCED
			embryos<- rbinom(1,eggs,logit(rnorm(1,log(-input$viableGam/(input$viableGam-1)),abs(input$viableGam_cv*mn))))
			# NUMBER OF VIABLE EMBRYOS SURVIVING TO RECRUIT TO AGE-1
			age0<- rbinom(1,embryos,S0) 
			
			
			# UPDATE YEARS SINCE SPAWNING
			pop[pop$yr_since_spawn>=0,]$yr_since_spawn<- pop[pop$yr_since_spawn>=0,]$yr_since_spawn+1
			
						
			## JUVENILES BECOMING ADULTS THAT WILL SPAWN NEXT YEAR
			pop$tmp<- rbinom(nrow(pop),1,mat_fun(age=pop$age,mid=input$mat_mid, high=input$mat_high))
			if(nrow(pop[pop$yr_since_spawn== -1 & pop$tmp==1,]))
				{pop[pop$yr_since_spawn== -1 & pop$tmp==1,]$yr_since_spawn<- 0}
			## ADULTS BECOMING SEXUALLY MATURE AND WILL SPAWN NEXT YEAR
			
			pop$tmp<- rbinom(nrow(pop),1,sp_fun(yr=pop$yr_since_spawn-1, mid=input$sp_mid, high=input$sp_high))
			if(nrow(pop[pop$yr_since_spawn> 4,]))
				{pop[pop$yr_since_spawn> 4,]$tmp<- 1}
			if(nrow(pop[pop$yr_since_spawn>0 & pop$tmp==1,])){pop[pop$yr_since_spawn>0 & pop$tmp==1,]$yr_since_spawn<- 0	}

			# UPDATE SURVIVAL FOR FISH GREATER OR EQUAL TO AGE 1
			pop$p<- surv_fun(pop$age)
			pop$surv<- rbinom(nrow(pop),1,pop$p)
			
			
			# ADD AGE-0 FISH RECRUITED TO THE POPULATION IN THE NEXT YEAR
			recruits<- data.frame(origin='n',sex=NA,age=0,yr_since_spawn=-100,fl=NA,p=0,surv=0,tmp=0)
			if(age0>0){
				age0_m<-rbinom(1,age0,0.5)		
				recruits<- rbind(recruits,
					data.frame(origin='n',sex=c(rep('m',age0_m), rep('f', age0-age0_m)),age=0,
					yr_since_spawn=-1,fl=NA,p=1,surv=1,tmp=0))
				recruits$fl<- input$Linf*(1-exp(-input$K*(recruits$age-input$t0)))#*rnorm(nrow(out),1, 0.1)
				}
			
			
			# RECRUIT AGE-0 FISH TO THE POPULATION
			pop<- rbind(pop, recruits)	
			
			
			# HATCHERY STOCKING
			# ASSUMES FISH ARE STOCKED AT THE END OF THE YEAR
			# AND ARE NOT SUBJECT TO SURVIVAL
			# BUT SURVIVAL COULD BE MODIFIED HERE...surv=1 line 189
			stocked<- data.frame(origin='h',sex=NA,age=0,yr_since_spawn=-100,fl=NA,p=0,surv=0,tmp=0)
			if(sum(input$age0_stock,input$age1_stock,input$age2_stock+
				input$age3_stock,input$age4_stock,
				input$age5_stock,input$age6_stock,
				input$age7_stock)>0){
				stocked<- rbind(stocked,
						data.frame(origin='h',sex=NA,age=c(rep(0,input$age0_stock),rep(1,input$age1_stock),
						rep(2,input$age2_stock),rep(3,input$age3_stock),rep(4,input$age4_stock),
						rep(5,input$age5_stock),rep(6,input$age6_stock),rep(6,input$age7_stock)),
					yr_since_spawn=-1,fl=NA,p=1,surv=1,tmp=0))
				stocked$sex<- sample(c('m','f'),nrow(stocked),replace=TRUE,prob=c(0.5,0.5))
				stocked$fl<- input$Linf*(1-exp(-input$K*(stocked$age-input$t0)))#*rnorm(nrow(out),1, 0.1)
				}
			pop<- rbind(pop, stocked)		

			
			# SUMMARY
			pop<- subset(pop, yr_since_spawn>=-1)# to remove sham row prior to summary
			if(nrow(pop)>0){
				app<-dcast(pop, origin+sex~yr_since_spawn, value.var='fl',length)
				app<-merge(expand.grid(origin=c("n",'h'),sex=c('f','m')),app,
					by=c('origin','sex'),all.x=TRUE)	
				app[is.na(app)]<-0
				app$year<- i
				app$r<- j
				}else {# error handling when population crashes
				app<- expand.grid(origin=c("h","n"),
					sex=c("f","m"),    
					"-1" =0,"0" =0,"1" =0,"2"=0,"3" =0,"4" =0,"5"=0, 
					year=i,r=j)
				}

			xx<- rbind.fill(xx,app)		
		
			} # END I
		}# END J
	xx[is.na(xx)]<-0
	return(xx)
	} # END FUNCTION
	
	
	

	
	
	
	
	
	
	