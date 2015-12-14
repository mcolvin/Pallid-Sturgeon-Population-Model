## GLOBAL

delete_all_output<-function(x="N")
	{
	if(x=="N"){return(print("No files deleted"))}
	if(x=="Y")
		{fnames<-dir("./output")
		file.remove(paste("./output",fnames,sep="/"))
		return(print("Output directory has been cleared"))
		}
	}




logit<- function(x){exp(x)/(1+exp(x))}

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
	
	if(input$type=="triag")
		{
		viableGam<- rtriangle(1, input$viableGam_rng[1],input$viableGam_rng[2] ,input$viableGam)
		S0<-rtriangle(1,input$S0_rng[1],input$S0_rng[2],input$S0)
		S1<-rtriangle(1,input$S1_rng[1],input$S1_rng[2],input$S1)
		S2<-rtriangle(1,input$S2_rng[1],input$S2_rng[2],input$S2)
		}
	if(input$type=="unif")
		{
		viableGam<- runif(1, input$viableGam_rng[1],
			input$viableGam_rng[2])
		S0<-runif(1,input$S0_rng[1],input$S0_rng[2])
		S1<-runif(1,input$S1_rng[1],input$S1_rng[2])
		S2<-runif(1,input$S2_rng[1],input$S2_rng[2])
		}

	# SURVIVAL AGE-1 TO MAX AGE
    S<- c(S1,S2,rep(S2,input$maxAge-1))
	
	
	# JUVENILES	
	## INITIALIZE NUMBER OF JUVENILES BY ORIGIN	
	
	if(input$type=="triag")
		{
		juv_ini_n<- rtriangle(1,input$juv_ini_n_rng[1],input$juv_ini_n_rng[2], input$juv_ini_n)
		juv_ini_h<-rtriangle(1,input$juv_ini_h_rng[1],input$juv_ini_h_rng[2], input$juv_ini_h)
		}
	if(input$type=="unif")
		{
		juv_ini_n<- runif(1,input$juv_ini_n_rng[1],
			input$juv_ini_n_rng[2])
		juv_ini_h<-runif(1,input$juv_ini_h_rng[1],
			input$juv_ini_h_rng[2])
		}
	
	out<- data.frame(origin=c(rep("h",juv_ini_h), rep("n",juv_ini_n)),sex=NA,age=NA,yr_since_spawn=-1)
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
	
	if(input$type=="triag")
		{
		adults_ini_n<- rtriangle(1,input$adults_ini_n_rng[1],input$adults_ini_n_rng[2], input$adults_ini_n)
		adults_ini_h<-rtriangle(1,input$adults_ini_h_rng[1],input$adults_ini_h_rng[2], input$adults_ini_h)	
		}
	if(input$type=="unif")
		{
		adults_ini_n<- runif(1,input$adults_ini_n_rng[1],
			input$adults_ini_n_rng[2])
		adults_ini_h<-runif(1,input$adults_ini_h_rng[1],
			input$adults_ini_h_rng[2])	
		}
	yyy<- data.frame(origin=c(rep("h",adults_ini_h), rep("n",adults_ini_n)),sex=NA,age=NA,yr_since_spawn=-1)

	# ASSIGN SEX TO INDIVUAL FISH GIVEN INITIAL SEX RATIO
	yyy[yyy$origin=="h",]$sex<- sample(c('m','f'),adults_ini_h,replace=TRUE,prob=c(1-input$sr_h,input$sr_h))
	yyy[yyy$origin=="n",]$sex<- sample(c('m','f'),adults_ini_n,replace=TRUE,prob=c(1-input$sr_n,input$sr_n))
	
	# ASSIGN AGE TO INDVIDUAL FISH ASSUMING 
	# APPROXIMATE EQULIBRIUM 
	yyy$age<- sample(c(input$mat_high:input$maxAge),nrow(yyy),replace=TRUE,prob=cumprod(S[input$mat_high:input$maxAge]))
	
	# ASSIGN FORK LENGTH TO INVIDUAL FISH, GIVEN AGE
	yyy$fl<- input$Linf*(1-exp(-input$K*(yyy$age-input$t0)))#*rnorm(nrow(out),1, 0.1)
	# ASSIGN YEARS SINCE SPAWNING TO INDVIDUAL FISH
	yyy$yr_since_spawn<- sample(c(0:4),nrow(yyy), replace=TRUE,prob=c(1,1,1,1,1))
	pop<- rbind(out, yyy)

	# NUMBER OF EGGS PRODUCED BY SEXUALLY MATURE FEMALES
	eggs<- round(sum(input$a_fec + input$b_fec*pop[pop$yr_since_spawn==0 & pop$sex=='f',]$fl),0)
	
	# NUMBER OF EMBRYOS PRODUCE FROM EGGS
	embryos<- qbinom(runif(1),eggs,viableGam) #rbinom(1,eggs,viableGam)
	# NUMBER OF VIABLE EMBRYOS SURVIVING TO RECRUIT TO AGE-1
	age0<- qbinom(runif(1),embryos,S0) 
	
	# BUNDLE UP THE DATA
	out_vals<- list(viableGam=viableGam,
		S0=S0,S1=S1,S2=S2, 
		juv_ini_n=juv_ini_n,juv_ini_h=juv_ini_h,
		adults_ini_h=adults_ini_h,adults_ini_n=adults_ini_n)
    return(list(pop=pop,age0=age0,out_vals=out_vals))
    }#})
  

xx_ind<- function(input=input)
	{
	xx<- data.table() #SET UP DATAFRAME TO HOLD RESULTS

	# INITIALIZE AT T=0
	if(input$type=="triag")
		{
		viableGam<- rtriangle(1, input$viableGam_rng[1],input$viableGam_rng[2] ,input$viableGam)
		S0<-rtriangle(1,input$S0_rng[1],input$S0_rng[2],input$S0)
		S1<-rtriangle(1,input$S1_rng[1],input$S1_rng[2],input$S1)
		S2<-rtriangle(1,input$S2_rng[1],input$S2_rng[2],input$S2)
		}
	if(input$type=="unif")
		{
		viableGam<- runif(1, input$viableGam_rng[1],
			input$viableGam_rng[2])
		S0<-runif(1,input$S0_rng[1],input$S0_rng[2])
		S1<-runif(1,input$S1_rng[1],input$S1_rng[2])
		S2<-runif(1,input$S2_rng[1],input$S2_rng[2])
		}

	# SURVIVAL FUNCTION
	surv_fun<- approxfun(c(0,1,2),	c(S0,S1,S2),rule=2)	
		
	# INITIAL NUMBERS AT T=0
	inits<- inits_ind(input)
	out_vals<-inits$out_vals
	out_vals$age0_stock<-input$age0_stock
	out_vals$age1_stock<-input$age1_stock
	out_vals$spawn_frequency<- input$spawn_frequency

	# JUVENILE AND ADULT STAGES
	pop<-data.table(inits$pop)	# INITIAL POST SPAWN POPULATION 
	pop$p<- surv_fun(pop$age)# PROBABILITY OF SURVIVING TO NEXT YEAR
	pop$surv<- rbinom(nrow(pop),1,pop$p) # SURVIVE OR NOT (0,1) TO NEXT YEAR		

	# AGE-0 PRODUCTION
	## DID SPAWNING OCCUR
	spawn<- rbinom(1,1,(1/input$spawn_frequency))# DID SPAWNING OCCUR?
	
	## NUMBER OF EGGS PRODUCED
	eggs<- spawn*round(sum(input$a_fec + input$b_fec*pop[pop$yr_since_spawn==0 & pop$sex=='f',]$fl),0)
	
	if(length(pop[pop$origin=='n',]$fl)> 300000){eggs<-0}
	## NUMBER OF VIABLE EMBRYOS PRODUCED
	embryos<- qbinom(runif(1),eggs,viableGam) #rbinom(1,eggs,viableGam)
	# NUMBER OF VIABLE EMBRYOS SURVIVING TO RECRUIT TO AGE-1
	age0<- qbinom(runif(1),embryos,S0) 
	## A PLACEHOLDER TO HELP HANDLE YEARS OF NO AGE-0 FISH
	recruits<- data.table(origin='n',sex=NA,age=0,yr_since_spawn=-100,fl=NA,p=0,surv=0)
	if(age0>0){
		age0_m<-rbinom(1,age0,0.5)		
		recruits<- list(recruits,
			new=data.table(origin='n',sex=c(rep('m',age0_m), rep('f', age0-age0_m)),age=0,
			yr_since_spawn=-1,fl=NA,p=1,surv=1))
		recruits<-rbindlist(recruits,use.names=TRUE)
		recruits$fl<- input$Linf*(1-exp(-input$K*(recruits$age-input$t0)))#*rnorm(nrow(out),1, 0.1)
		}
			
	# RECRUIT AGE-0 FISH TO THE POPULATION
	pop<- rbindlist(list(pop, recruits),use.names=TRUE)
	
	# SUMMARY
	pop<- subset(pop, yr_since_spawn>=-1)# to remove sham row prior to summary
	if(nrow(pop)>0){
		app<-data.table(pop[,j=list(abundance=length(fl)),by=list(origin,sex,yr_since_spawn)])
		app$year<-1
		}
	xx<- rbindlist(list(xx,app),use.names=TRUE)			
	
		
	## LOOP OVER YEARS
	for(i in 1:input$nyears)
		{
		if(input$type=="triag")
			{
			S0<-rtriangle(1,input$S0_rng[1],input$S0_rng[2],input$S0)
			S1<-rtriangle(1,input$S1_rng[1],input$S1_rng[2],input$S1)
			S2<-rtriangle(1,input$S2_rng[1],input$S2_rng[2],input$S2)
			}
		if(input$type=="unif")
			{
			S0<-runif(1,input$S0_rng[1],input$S0_rng[2])
			S1<-runif(1,input$S1_rng[1],input$S1_rng[2])
			S2<-runif(1,input$S2_rng[1],input$S2_rng[2])
			}
		
		# SURVIVAL FUNCTION
		surv_fun<- approxfun(c(0,1,2),	c(S0,S1,S2),rule=2)		

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
		eggs<- spawn*round(sum(input$a_fec+input$b_fec*pop[pop$yr_since_spawn==0 & pop$sex=='f',]$fl),0)
		if(length(pop[pop$origin=='n',]$fl)> 500000){eggs<-0}
		## NUMBER OF VIABLE EMBRYOS PRODUCED
		
		embryos<- qbinom(runif(1),eggs,viableGam) #rbinom(1,eggs,viableGam)
		# NUMBER OF VIABLE EMBRYOS SURVIVING TO RECRUIT TO AGE-1
		age0<- qbinom(runif(1),embryos,S0) 
		
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
		recruits<- data.table(origin='n',sex=NA,age=0,yr_since_spawn=-100,fl=NA,p=0,surv=0,tmp=NA)
		if(age0>0){
			age0_m<-rbinom(1,age0,0.5)		
			recruits<- list(recruits,
				new=data.table(origin='n',sex=c(rep('m',age0_m), rep('f', age0-age0_m)),age=0,
				yr_since_spawn=-1,fl=NA,p=1,surv=1,tmp=NA))
			recruits<-rbindlist(recruits,use.names=TRUE)
			recruits$fl<- input$Linf*(1-exp(-input$K*(recruits$age-input$t0)))#*rnorm(nrow(out),1, 0.1)
			}
				
		# RECRUIT AGE-0 FISH TO THE POPULATION
		pop<- rbindlist(list(pop, recruits),use.names=TRUE)
	
		# SUMMARY
		pop<- subset(pop, yr_since_spawn>=-1)# to remove sham row prior to summary
				
		# HATCHERY STOCKING
		# ASSUMES FISH ARE STOCKED AT THE END OF THE YEAR
		# AND ARE NOT SUBJECT TO SURVIVAL
		# BUT SURVIVAL COULD BE MODIFIED HERE...surv=1 line 189
		stocked<- data.table(origin='h',sex=NA,age=0,yr_since_spawn=-100,fl=NA,p=0,surv=0,tmp=0)
		if(sum(input$age0_stock,input$age1_stock,input$age2_stock+
			input$age3_stock,input$age4_stock,
			input$age5_stock,input$age6_stock,
			input$age7_stock)>0){
			newStocked<- data.table(origin='h',sex=NA,
				age=c(rep(0,rbinom(1,input$age0_stock,0.051)),rep(1,input$age1_stock),
					rep(2,input$age2_stock),rep(3,input$age3_stock),rep(4,input$age4_stock),
					rep(5,input$age5_stock),rep(6,input$age6_stock),rep(6,input$age7_stock)),
				yr_since_spawn=-1,fl=NA,p=1,surv=1,tmp=0)
			stocked<- rbindlist(list(stocked,newStocked))
			stocked$sex<- sample(c('m','f'),nrow(stocked),replace=TRUE,prob=c(0.5,0.5))
			stocked$fl<- input$Linf*(1-exp(-input$K*(stocked$age-input$t0)))#*rnorm(nrow(out),1, 0.1)
			}
		pop<- rbindlist(list(pop, stocked),use.names=TRUE)		

		# SUMMARY
		pop<- subset(pop,yr_since_spawn>=-1)# to remove sham row prior to summary

		if(nrow(pop)>0)
			{
			app<-data.table(pop[,j=list(abundance=length(fl)),by=list(origin,sex,yr_since_spawn)])
			app$year<-i
			}
		xx<- rbindlist(list(xx,app),use.names=TRUE)			
		}	 # END I
	return(list(xx=xx,out_vals=out_vals))
	} # END FUNCTION
	
	
	

	
	
	
	
	
	
	