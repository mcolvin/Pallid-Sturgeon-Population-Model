


## SERVER
inits_ind<- function(input=input)
	{# FUNCTION TO INITIALIZE AGE STRUCTURED MODEL INPUTS
	# FUNCTION RETURNS THE NUMBER OF POST SPAWN PALLID STURGEON (JUVENILES, ADULTS, EMBRYOS)
	# TO INITIALIZE THE MODEL. 
	
	# SURVIVAL AGE-1 TO MAX AGE
    S<- c(input$S1,input$S2,rep(input$S3plus, input$maxAge-2))	
	
	
	# JUVENILES	
	## INITIALIZE NUMBER OF JUVENILES BY ORIGIN	
	out<- data.frame(origin=c(rep("h",input$juv_ini_h), rep("n",input$juv_ini_n)),sex=NA,age=NA,yr_since_spawn=-1)
	## ASSIGN SEX
	out$sex<- sample(c('m','f'),nrow(out),replace=TRUE,prob=c(0.5,0.5))# assumes 50:50 for juveniles
	## ASSIGN AGE
	out$age<- sample(c(1:(input$ee-1)),nrow(out),replace=TRUE,prob=cumprod(S[1:(input$ee-1)]))
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
	yyy$age<- sample(c(input$ee:input$maxAge),nrow(yyy),replace=TRUE,prob=cumprod(S[input$ee:input$maxAge]))
	## ASSIGN FORK LENGTH
	yyy$fl<- input$Linf*(1-exp(-input$K*(yyy$age-input$t0)))#*rnorm(nrow(out),1, 0.1)
	## ASSIGN YEARS SINCE SPAWNING
	yyy$yr_since_spawn<- sample(c(0:4),nrow(yyy), replace=TRUE,prob=c(1,1,1,1,1))
	pop<- rbind(out, yyy)

	
	# AGE-0 
	## NUMBER OF AGE-0 FISH
	eggs<- round(sum(input$a_fec+pop[pop$yr_since_spawn==0 & pop$sex=='f',]$fl+input$b_fec),0)
	age0<- rbinom(1,eggs,input$viableGam) # NUMBER OF EMBRYOS
    return(list(pop=pop,age0=age0))
    }#})
  

xx_ind<- function(input=input)
	{
	# SURVIVAL FUNCTION
	surv_fun<- approxfun(c(0:input$maxAge),c(input$S0,input$S1,input$S2,rep(input$S3plus, input$maxAge-2)))
	# MATURITY FUNCTION
	x<-c(0,input$aa, input$bb,input$cc,input$dd,input$ee,input$maxAge)
	y<-c(0,0,        0.25,    0.5,   0.75,   1,    1)
	mat_fun<- approxfun(x,y)
	sp_fun<- approxfun(c(0,1,2,3,4),c(input$aaa,input$bbb,input$ccc,input$ddd,input$eee),rule=2)
	
	xx<- data.frame()
	
	for(j in 1:input$nreps)
		{
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
		embryos<- rbinom(1,eggs,input$viableGam)
		## NUMBER OF VIABLE EMBRYOS SURVIVING TO RECRUIT TO AGE-1
		age0<- rbinom(1,embryos,input$S0) 
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
		app<-dcast(pop, age+origin+sex~yr_since_spawn, value.var='fl',length,subset=.(age<38))
		app<-merge(expand.grid(age=c(0:input$maxAge),origin=c("n",'h'),sex=c('f','m')),app,
			by=c('age','origin','sex'),all.x=TRUE)	
		app[is.na(app)]<-0
		app$year<- 0
		app$r<- j
		xx<-rbind.fill(xx,app)
		
		for(i in 1:input$nyears)
			{
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
			embryos<- rbinom(1,eggs,input$viableGam)
			# NUMBER OF VIABLE EMBRYOS SURVIVING TO RECRUIT TO AGE-1
			age0<- rbinom(1,embryos,input$S0) 
			
			
			# UPDATE YEARS SINCE SPAWNING
			pop[pop$yr_since_spawn>=0,]$yr_since_spawn<- pop[pop$yr_since_spawn>=0,]$yr_since_spawn+1
			
			## JUVENILES BECOMING ADULTS THAT WILL SPAWN NEXT YEAR
			pop$tmp<- rbinom(nrow(pop),1,mat_fun(pop$age))
			if(nrow(pop[pop$yr_since_spawn== -1 & pop$tmp==1,])){pop[pop$yr_since_spawn== -1 & pop$tmp==1,]$yr_since_spawn<- 0}
			## ADULTS BECOMING SEXUALLY MATURE AND WILL SPAWN NEXT YEAR
			pop$tmp<- rbinom(nrow(pop),1,sp_fun(pop$yr_since_spawn-1))
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
			pop<- subset(pop, yr_since_spawn>=-1)# to remove sham row prior to summary
		
		
			# SUMMARY
			app<-dcast(pop, age+origin+sex~yr_since_spawn, value.var='fl',length,subset=.(age<38))
			app<-merge(expand.grid(age=c(0:input$maxAge),origin=c("n",'h'),sex=c('f','m')),app,
				by=c('age','origin','sex'),all.x=TRUE)	
			app[is.na(app)]<-0
			app$year<- i
			app$r<- j
			xx<- rbind.fill(xx,app)
			} # END I
		}# END J
	xx[is.na(xx)]<-0
	return(xx)
	} # END FUNCTION
	
	
	
	
	
	
	
	
	
	
	
	
	
	