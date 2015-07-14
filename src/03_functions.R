# FUNCTIONS

## HELPERS
sim_nxt_adults<- function(matured,sexRatio,prev,S,tmax)
	{
	# FUNCTION TO SIMULATE THE NEXT YEARS ABUNDANCE
	# GIVEN PREVIOUS ABUNDANCE, MATURATION, AND SURVIVAL
	nxt<-rbinom(tmax,prev,S)
	nxt<- nxt+round(matured*sexRatio,0)
	return(c(0,nxt[-tmax]))  
	}

bundleReps<- function(appendTo,appendDat,rep)
	{ 
	# FUNCTION TO BUNDLE UP REPLICATE SIMUALTIONS 
	app<-as.data.frame(appendDat)
	names(app)<- paste("yr",c(1:ncol(app)))
	app$rep<-rep
	out<- rbind(appendTo,app)
	return(out)
	}

## SERVER
inits_ind<- function(input=input)
	{#reactive({
	# FUNCTION TO INITIALIZE AGE STRUCTURED MODEL INPUTS
    S<- c(input$S1,input$S2,rep(input$S3plus, input$maxAge-2))
	# INITIALIZE JUVENILES
	out<- data.frame(origin=c(rep("h",input$juv_ini_h*1000), rep("n",input$juv_ini_n*1000)),stage="jv")
	# ASSIGN SEX
	out$sex<- sample(c('m','f'),nrow(out),replace=TRUE,prob=c(0.5,0.5))# assumes 50:50 for juveniles
	# ASSIGN AGE
	out$age<- sample(c(1:(input$ee-1)),nrow(out),replace=TRUE,prob=cumprod(S[1:(input$ee-1)]))
	out$yr_since_spawn<- -1	

	
	# INITIALIZE ADULTS	
	yyy<- data.frame(origin=c(rep("h",input$adults_ini_h*1000), rep("n",input$adults_ini_n*1000)),stage=NA,sex=NA)
	# ASSIGN STAGE	
	yyy$stage<- "ad" #sample(c("sp","r"),nrow(yyy),replace=TRUE,prob=c(0.20,0.8))
	# ASSIGN SEX
	yyy[yyy$origin=="h",]$sex<- sample(c('m','f'),input$adults_ini_h,replace=TRUE,prob=c(1-input$sr_h,input$sr_h))
	yyy[yyy$origin=="n",]$sex<- sample(c('m','f'),input$adults_ini_n,replace=TRUE,prob=c(1-input$sr_n,input$sr_n))
	# ASSIGN AGE
	yyy$age<- sample(c(input$ee:input$maxAge),nrow(yyy),replace=TRUE,prob=cumprod(S[input$ee:input$maxAge]))
	# ASSIGN YEARS SINCE SPAWNING
	yyy$yr_since_spawn<- sample(c(0:4),nrow(yyy), replace=TRUE,prob=c(1,1,1,1,1))
	
	out<- rbind(out, yyy)
	out$fl<-input$Linf*(1-exp(-input$K*(out$age-input$t0)))#*rnorm(nrow(out),1, 0.1)
    return(out)
    }#})
  

xx_ind<- function(input=input)
	{
	# SURVIVAL FUNCTION
	surv_fun<- approxfun(c(1:input$maxAge),c(input$S1,input$S2,rep(input$S3plus, input$maxAge-2)))
	# MATURITY FUNCTION
	x<-c(0,input$aa, input$bb,input$cc,input$dd,input$ee,input$maxAge)
	y<-c(0,0,        0.25,    0.5,   0.75,   1,    1)
	mat_fun<- approxfun(x,y)
	
	
	
	# t=0
	pop<- inits_ind(input)	
	pop$p<- surv_fun(pop$age)
	pop$surv<- rbinom(nrow(pop),1,pop$p)
	# POPULATION SUMMARY
	xx<-as.data.frame(table(pop$origin,pop$yr_since_spawn))
	xx$year<- 0

	
	for(i in 1:input$nyears)
		{
		# t+1
		pop<- subset(pop,surv==1 & age<input$maxAge)
		# RECRUITMENT
		eggs<- round(sum(input$a_fec+pop[pop$yr_since_spawn==0 & pop$sex=='f',]$fl+input$b_fec),0)
		embryos<- rbinom(1,eggs,input$viableGam)
		age1<- rbinom(1,embryos,input$S0) 
		
		# UPDATE AGE
		pop$age<- pop$age+1
		# UPDATE YEARS SINCE SPAWNING
		pop[pop$yr_since_spawn>0,]$yr_since_spawn<- pop[pop$yr_since_spawn>0,]$yr_since_spawn+1

		# UPDATE STAGE
		## JUVENILES BECOMING ADULTS THAT WILL SPAWN NEXT YEAR
		pop$tmp<- rbinom(nrow(pop),1,mat_fun(pop$age))
		pop[pop$yr_since_spawn== -1 & pop$tmp==1,]$yr_since_spawn<- 0
			
		# WHICH FISH WILL SURVIVE TO NEXT YEAR?
		pop$p<- surv_fun(pop$age)
		pop$surv<- rbinom(nrow(pop),1,pop$p)	
	
		return(out)    
		}
  






  
inits<- function(input=input)
	{#reactive({
	# FUNCTION TO INITIALIZE AGE STRUCTURED MODEL INPUTS
    S<- c(input$S1,input$S2,rep(input$S3plus, input$maxAge-2))
	stages<- rbind(expand.grid(stage="jv",
				origin = c("n","h"),
				sex="i"),
			expand.grid(stage=c("sp","r1","r2","r3","r4"),
				origin = c("n","h"),
				sex=c("m","f")))
	stages$name<- paste(stages$stage, stages$origin, stages$sex,sep="_")
		
	stages$ini<- 0
	# SET JUVENILE NUMBERS
	stages[stages$stage=="jv" & stages$origin=="n",]$ini<- input$juv_ini_n
	stages[stages$stage=="jv" & stages$origin=="h",]$ini<- input$juv_ini_h
	# SET HATCHERY ORIGIN ADULT NUMBERS
	adult_ini_h_f<- rbinom(1,input$adults_ini_h, input$sr_h)
	adult_ini_h_m<- input$adults_ini_h-adult_ini_h_f
	stages[stages$stage%in%c("sp","r1","r2","r3","r4") & 
		stages$origin=="h"&stages$sex=='f',]$ini<- rmultinom(1,adult_ini_h_f,c(1,1,1,1,1))
	stages[stages$stage%in%c("sp","r1","r2","r3","r4") & 
		stages$origin=="h"&stages$sex=='m',]$ini<- rmultinom(1,adult_ini_h_m,c(1,1,1,1,1))
	# SET NATURAL ORIGIN ADULT NUMBERS
	adult_ini_n_f<- rbinom(1,input$adults_ini_n, input$sr_n)
	adult_ini_n_m<- input$adults_ini_n-adult_ini_n_f
	stages[stages$stage%in%c("sp","r1","r2","r3","r4") & 
		stages$origin=="n"&stages$sex=='f',]$ini<- rmultinom(1,adult_ini_n_f,c(1,1,1,1,1))
	stages[stages$stage%in%c("sp","r1","r2","r3","r4") & 
		stages$origin=="n"&stages$sex=='m',]$ini<- rmultinom(1,adult_ini_n_m,c(1,1,1,1,1))
	
	## INITIALIZE JUVENILES WITH AGE STRUCTURE
    max_juv<- input$ee-1 # maximum juvenile age
    S_juv<- S
    S_juv[(max_juv+1):input$maxAge]<-0
    S_juv<- cumprod(S_juv)
    S_juv<- S_juv/sum(S_juv)

    S_ad<-S
    S_ad[1:max_juv]<-0
    S_ad[(max_juv+1):input$maxAge]<-cumprod(S[(max_juv+1):input$maxAge])
	
	ini<- as.data.frame(matrix(0,nrow=input$maxAge,ncol=nrow(stages)))
	names(ini)<- stages$name
	ini$age<- c(1:input$maxAge)

    ## ASSIGN NUMBERS OF FISH FOR EACH STAGE TO AGE
	out<- list(survivals=data.frame(age=c(0:input$maxAge),
		survival=c(input$S0,S),
		S_ad=c(0,S_ad)),
		stages=stages)
	for(i in 1:nrow(stages))
		{
		if(stages$stage[i]=="jv"){SS<-S_juv} else {SS<-S_ad}
		indx<-match(stages$name[i],names(ini))
		ini[,indx]<-rmultinom(1,(stages$ini[i]*1000),SS)
		}
	out$inits<- data.frame(ini)
    return(out)
    }#})

maturity<- function(input=input){#reactive({
  # 1. MATURITY FUNCTION
  x<-c(0,input$aa, input$bb,input$cc,input$dd,input$ee,input$maxAge)
  y<-c(0,0,        0.25,    0.5,   0.75,   1,    1)
  mat<- approxfun(x,y)
  out<- data.frame(age=c(1:input$maxAge),maturity=mat(c(1:input$maxAge)))
  return(out)
  }#})


xx<- function(input=input)
	{#reactive({
	#out<- data.frame()
	#j_n_out<-j_h_out<-a_h_m_out<-a_h_f_out<-a_n_f_out<-a_n_m_out<-data.frame()
	maxAge<- input$maxAge
		Linf=input$Linf # 1683
		K=input$K #0.036
		t0=input$t0# -5.9
		#sigma=93.77
		FL<-input$Linf*(1-exp(-input$K*(c(1:input$maxAge)-input$t0)))
	mat_dat<- maturity(input)
	yy<-inits(input)
	stages<- yy$stages
	survival<- yy$survivals[-1]# drop age-0 survival
	# PREALLOCATE STAGES IN A LIST
	x <- lapply(1:nrow(stages), function(i) matrix(0,nrow=input$maxAge,
		ncol=input$nyears))
	names(x)<- stages$name
	
	for(j in 1:input$nreps)
		{
		dat<- inits(input)$inits
		# ASSIGN INITIAL VALUES TO EACH STAGE
		for(i in 1:nrow(stages))
			{
			colindx<- match(stages$name[i],names(dat))
			x[[stages$name[i]]][,1]<- dat[,colindx]
			}
		# SIMULATE POPULATION DYNAMICS
		for(i in 2:input$nyears)
			{
			eggs<- sum((dat$sp_n_f+dat$sp_h_f)*(input$a_fec*(input$Linf*(1-exp(-input$K*(c(1:input$maxAge)-input$t0))))^input$b_fec))
			# AGE-0
			## EMBRYOS
			embryos<-sum()
			## FREE EMBRYOS
			free_embryos<- rbinom()
			## EXOGENOUSLY FEEDING LARVAE AND AGE-0
			
			# AGE-1 TO TMAX
			
			
			
			# NUMBER OF PALLID STURGEON STOCKED
			stocked<-c(rbinom(1,input$fe_stock,input$S0),
				rbinom(1,input$efl_stock,input$S0^0.666),
				rbinom(1,input$juv_stock,input$S0^0.333))
			stocked<- sum(stocked)
			nxt<- rbinom(maxAge,j_h[,i-1],survival)
			nxt_mat_h<- rbinom(maxAge,nxt,mat_dat$maturity)
			nxt<- nxt-nxt_mat_h
			j_h[,i]<-c(stocked,nxt[-maxAge])
		  
			# NUMBER OF PALLID STURGEON EMBRYOS PRODUCED
			age0<- sum(a_h_f[,i]*a_n_f[,i])*0.25*2
			nxt<- rbinom(maxAge,j_n[,i-1],survival)
			nxt_mat_n<- rbinom(maxAge,nxt,mat_dat$maturity)
			nxt<- nxt-nxt_mat_n  
			j_n[,i]<-c(age0,nxt[-maxAge])  
			
			# NUMBER OF ADULT NATURAL ORIGIN FEMALES  
			a_n_f[,i]<-sim_nxt_adults(matured=nxt_mat_n,sexRatio=0.5,
				prev=a_n_f[,i-1],
				S=survival,
				tmax=maxAge)

		  # NUMBER OF ADULT NATURAL ORIGIN MALES    
		  a_n_m[,i]<-sim_nxt_adults(matured=nxt_mat_n,sexRatio=0.5,
			   prev=a_n_m[,i-1],S=survival,tmax=maxAge)

		  a_h_f[,i]<-sim_nxt_adults(matured=nxt_mat_h,sexRatio=0.5,
									prev=a_h_f[,i-1],S=survival,tmax=maxAge)
		  
		  a_h_m[,i]<-sim_nxt_adults(matured=nxt_mat_h,sexRatio=0.5,
									prev=a_h_m[,i-1],S=survival,tmax=maxAge)
		  }
 
		a_n_f_out<- bundleReps(appendTo=a_n_f_out,append=a_n_f,rep=j)
		a_n_m_out<- bundleReps(appendTo=a_n_m_out,append=a_n_m,rep=j)
		a_h_f_out<- bundleReps(appendTo=a_h_f_out,append=a_h_f,rep=j)  
		a_h_m_out<- bundleReps(appendTo=a_h_m_out,append=a_h_m,rep=j)
		j_h_out<- bundleReps(appendTo=j_h_out,append=j_h,rep=j)  
		j_n_out<- bundleReps(appendTo=j_n_out,append=j_n,rep=j)
	  
		}# end J (reps)
	out<- list(age=c(1:maxAge),survival=survival,
			 a_n_f=a_n_f_out,
			 a_n_m=a_n_m_out,
			 a_h_f=a_h_f_out,
			 a_h_m=a_h_m_out,
			 j_h=j_h_out,
			 j_n=j_n_out)
	return(out)    
	}#})
  







## OLD FUNCTIONS
year0<- function(S0=0.051,stoch=FALSE)
	{# THIS FUNCTION RETURNS A VECTOR SURVIVALS FOR THE 3 STAGES THAT
	# THAT OCCUR IN YEAR 0 
	# DEFAULT 0.051 IS FROM STEPHENSON ET AL...
	if(stoch==TRUE)
		{
		repeat{
			x<- runif(2, S0,0.999)
			if (prod(x)>S0){break}
			x<- c(x,exp(log(S0)-(log(x[1])+log(x[2]))))
			}
		}
	if(stoch==FALSE){x<- rep(S0^(1/3),3)}
	return(x)
	}


initialize<- function(S,
	Jmat, 
	maxAge=41,
	Njuv_Natural=40000,
	Nadult_Natural=10000, 
	Njuv_Hatchery=40000,
	Nadult_Hatchery=1, 
	EGGS=40000,
	nyears=100)
	{
	# [1] SET UP ARRAYS TO HOLD STATE VALUES
	J<-SP<- R1<- R2<- R3<- R4<- array(0,dim=c(maxAge,nyears,2))
  
	                                         
    # [2] INITIALIZE MATRICES AT T=1
    ## [2.1] NATURAL ORIGIN JUVENILE FISH
	x<-S # SURVIVAL VECTOR
	x[-c(1:9)]<- 0	
	x[c(1:9)]<-cumprod(x[c(1:9)])
	x<- x/sum(x)
	ini<- rmultinom(1,Njuv_Natural,x)	
	sp<- rbinom(maxAge,ini,Jmat)	
	J[,1,1]<- ini-sp
	
	## [2.2] HATCHERY ORIGIN JUVENILE FISH
	x<-S # SURVIVAL VECTOR
	x[-c(1:9)]<- 0	
	x[c(1:9)]<-cumprod(x[c(1:9)])
	x<- x/sum(x)
	ini<- rmultinom(1,Njuv_Hatchery,x)	
	sp<- rbinom(maxAge,ini,Jmat)	
	J[,1,2]<- ini-sp	
    
    
	# [3] INITIAL VALUES FOR ADULT STAGES: SP, R1, R2, R3 R4
   
    ## [3.1] NATURALLY PRODUCED FISH    
	x<-S # SURVIVAL VECTOR	
	x[c(1:9)]<- 0
	x[-c(1:9)]<-cumprod(x[-c(1:9)])
	ini<- rmultinom(1,Nadult_Natural,x)	
	# ALLOCATE ADULTS AMONG SPAWNING AND RECRDUCESCENT STAGES
	states<- t(sapply(1:length(ini), function(x) rmultinom(1,ini[x],c(1,1,1,1,1))))

	SP[,1,1]<- states[,1]
	R1[,1,1]<- states[,2]
	R2[,1,1]<- states[,3]
	R3[,1,1]<- states[,4]
	R4[,1,1]<- states[,5]
		
	## [3.2] HATCHERY PRODUCED FISH    
	x<-S # SURVIVAL VECTOR	
	x[c(1:9)]<- 0
	x[-c(1:9)]<-cumprod(x[-c(1:9)])
	ini<- rmultinom(1,Nadult_Hatchery,x)	
	states<- t(sapply(1:length(ini), function(x) rmultinom(1,ini[x],c(1,1,1,1,1))))
		
	SP[,1,2]<- states[,1]
	R1[,1,2]<- states[,2]
	R2[,1,2]<- states[,3]
	R3[,1,2]<- states[,4]
	R4[,1,2]<- states[,5]		
      
	# INTIALIZE MATRICES FOR MATRIX OPERATIONS
	A<- matrix(0,nrow=maxAge,ncol=maxAge)
	A[cbind(c(2:maxAge),c(1:(maxAge-1)))]<-1
	fec<- matrix(0,nrow=maxAge,ncol=maxAge)
	fec[1,]<- EGGS	

	return(list(J=J, A=A, fec=fec, SP=SP, R1=R1, R2=R2, R3=R3,R4=R4))
	}




simulate<- function(nyears=100,nreps=50,
	stocked = c(0,0),
	# ABUDNANCE
	N_juv_Natural=c(0,500,1000),
	N_adult_Natural=c(3750,4000,4250),
	N_juv_Hatchery=c(18000,21500,25000),
	N_adult_Hatchery=c(12500,14500,16750),
	# RATES
	sex_ratio=c(0.33*0.8,0.33,0.33*1.2),
	S_egg_embryo=c(0.0000019*0.8,0.0000019,0.0000019*1.2),
	S0=c(0.02,0.051,0.1),

	S1=c(0.60,0.686,0.75),
	S2=c(0.9,0.922,0.95),
	stoch_in=FALSE)
	{## A FUNCTION TO SIMULATE POPULATION DYNAMICS OVER A PERIOD OF TIME	
	outp<-data.frame()
	parms<- data.frame()	

	#von Bertalanffy Curve
	maxAge=41		
	Linf=1683
	K=0.036
	t0=-5.9
	sigma=93.77
	FL<-Linf*(1-exp(-K*(c(1:maxAge)-t0)))
	
	# ANNUAL STOCKING VALUES	
	## [3.4] NUMBER OF AGE-0 FISH STOCKED
	AGE_0_STOCKED<- stocked[1] 
	## [3.5] NUMBER OF AGE-1 FISH PRODUCED
	JUV_STOCKED<- stocked[2] 	
	
	for(i in 1:nreps)
		{
		## [1.2] DEMOGRAPHIC RATES AND VALUES
		S0<- ifelse(unique(S0)>1, 
			rtriangle(1,a=S0[1] , b=S0[3], c=S0[2]),
			S0[1])
		S0<- year0(S0=S0,stoch=stoch_in) 
		S_embryo_free_embryo		<- S0[1]
		S_free_embryo_exo_larvae	<- S0[2]
		S_exo_larvae_age1			<- S0[3] 
		S1_ini<- rtriangle(1,a=S1[1] , b=S1[3], c=S1[2])
		S2_ini<- rtriangle(1,a=S2[1], b=S2[3], c=S2[2])

		Jmat<- c(rep(0,6),0.25,0.5,rep(1,33))# PROBABILITY OF BECOMING SEXUALLY MATURE 
					
		# PROBABILITY EGG BECOMES EMBRYO [CALIBTRATED TO KEEP ~STABLE AGE STRUC]0.000019 
		S_egg_embryo_ini<- ifelse(length(unique(S_egg_embryo))>1, 
			rtriangle(1,a=S_egg_embryo[1],b=S_egg_embryo[3],c=S_egg_embryo[2]),
			S_egg_embryo[1])

		
		#pop_influx<- 12000
		#allee_rate<- 0.0004
		#S_egg_embryo<-  max_s_egg_embryo/(1+exp(-allee_rate*(pop - pop_influx)))
		EGGS_age<- 3.48*10^-8 *FL^4.05 #  +110056*log(FL)# EGGS AT LENGTH 
		S<- c(S1_ini,rep(S2_ini,maxAge-1))#	

		## [1.3] STATE INITIAL VALUES
		### [1.3.1] NATURAL ORIGIN FISH
		N_juv_Natural_ini<- round(rtriangle(1,a=N_juv_Natural[1],b=N_juv_Natural[3],c=N_juv_Natural[2]),0) 
		N_adult_Natural_ini<- round(rtriangle(1,a=N_adult_Natural[1],b=N_adult_Natural[3],c=N_adult_Natural[2]) ,0)


		
		### [1.3.2] HATCHERY ORIGIN FISH
		N_juv_Hatchery_ini<- round(rtriangle(1,a=N_juv_Hatchery[1],b=N_juv_Hatchery[3],c=N_juv_Hatchery[2]),0)
		N_adult_Hatchery_ini<- round(rtriangle(1,a=N_adult_Hatchery[1],b=N_adult_Hatchery[3],c=N_adult_Hatchery[2]),0)
		sex_ratio_ini<-  rtriangle(1,a=sex_ratio[1],b=sex_ratio[3],c=sex_ratio[2])


		
		# [3] SIMULATE POPULATION DYNAMICS
		## [3.1] INITIALIZE MODEL at t=1 GIVEN INPUTS AT STABLE AGE DISTRIBUTION	
		inits<- initialize(S=S, 
			Jmat=Jmat,
			maxAge=maxAge,
			Njuv_Natural=N_juv_Natural_ini,
			Nadult_Natural=N_adult_Natural_ini,
			Njuv_Hatchery=N_juv_Hatchery_ini,
			Nadult_Hatchery=N_adult_Hatchery_ini,
			EGGS=EGGS_age) 
		J<- as.array(inits$J)
		A<- as.matrix(inits$A)
		SP<- as.array(inits$SP)
		fec<- as.matrix(inits$fec)
		R1<- as.array(inits$R1)
		R2<- as.array(inits$R2)
		R3<- as.array(inits$R3)
		R4<- as.array(inits$R4)		
		
		# SIMULATE DYNAMICS		
		for(type in 1:2)
			{
			for(yr in 2:nyears)
				{  
				## [1.1] NUMBER OF PST RECRUITED TO AGE 1
				EGGS_NAT<- round(sum(fec %*% (SP[,yr-1,1]+SP[,yr-1,2])*sex_ratio_ini),0) # egg produced
								 
				## [1.2] NUMBER OF EGGS THAT WERE SUCCESSFULLY FERTILIZED
				EMBRYOS_NAT<- rbinom(1,EGGS_NAT,S_egg_embryo_ini)
								 
				## [1.3] FREE EMBRYO
				FREE_EMBRYOS_NAT<- rbinom(1,EMBRYOS_NAT,S_embryo_free_embryo)
								  
				## [1.4] EXOGENOUSLY FEEDING LARVAE
				EXO_LARVAE_NAT<- rbinom(1,FREE_EMBRYOS_NAT,S_free_embryo_exo_larvae)
				AGE1_NAT<- rbinom(1,EXO_LARVAE_NAT,S_exo_larvae_age1)
				
				## [1.2] JUVENILE STAGE
				tmp<- A %*% rbinom(maxAge,J[,yr-1,type],S) # fish surviving
				tmp[1]<- ifelse(type==1,AGE1_NAT,JUV_STOCKED)
			
				mat<- rbinom(maxAge,tmp,Jmat)

				## [1.3] JUVENILES REMAINING SEXUALLY IMMATURE
				J[,yr,type]<- tmp-mat
										
				## [1.4] SPAWNERS AND POST-SPAWNING
				srv_spn<- A %*% rbinom(maxAge,SP[,yr-1,type],S)
				R1[,yr,type]<- srv_spn
							
				## [1.5]  R1
				srv_R1<- A %*% rbinom(maxAge,R1[,yr-1,type],S)
				spn_R1<- rbinom(maxAge,srv_R1,0.1)
				R2[,yr,type]<- srv_R1-spn_R1
										
				## [1.6]  R2
				srv_R2 <-A %*% rbinom(maxAge,R2[,yr-1,type],S) 
				spn_R2<- rbinom(maxAge,srv_R2,0.5)
				R3[,yr,type]<- srv_R2-spn_R2  

				## [1.7] R3
				srv_R3 <-A %*% rbinom(maxAge,R3[,yr-1,type],S)
				spn_R3<- rbinom(maxAge,srv_R3,0.75)
				R4[,yr,type]<- srv_R3-spn_R3
								
				## [1.6]  R4 
				srv_R4 <- A %*% rbinom(maxAge,R4[,yr-1,type],S) 
				spn_R4<- rbinom(maxAge,srv_R4,1)

				SP[,yr,type]<- (mat+spn_R1+spn_R2+spn_R3+spn_R4)	
				}  # END i
			} # END type

		juveniles<- apply(J,c(2:3),sum)
		adults<- SP+R1+R2+R3+R1
		adults<- apply(adults,c(2:3),sum)
		out<- as.data.frame(cbind(juveniles, adults))
		names(out)<-c("jn","jh","an","ah")
		out$rep<- i
		out$year<- c(1:nyears)
		outp<- rbind(outp,out)
					
		vals<- c(prod(S0), S_egg_embryo_ini, S_embryo_free_embryo, S_free_embryo_exo_larvae,
			S_exo_larvae_age1, S1_ini, S2_ini, AGE_0_STOCKED,sum(JUV_STOCKED),
			N_juv_Natural_ini, N_adult_Natural_ini, N_juv_Hatchery_ini, N_adult_Hatchery_ini,sex_ratio_ini)	
		nams<- c("S: age-0 to age-1", 'S: egg to embryo', 'S: embryo to free embryo', 'S: free: embryo to exo larvae',
			'S: exo. larvae to age-1', 'S: age-1 to age-2', 'S: greater than age-1', 'Stocked: age-0','Stocked: age-1',
			"Juvenile abundance (natural)", "Adult abundance (natural)"," Juvenile abundance (hatchery)", "Adult abundance (hatchery)",
			"Sex ratio")	
		parms_app<- data.frame(rep=rep(i,length(vals)),parm=nams,vals=vals)
		parms<- rbind(parms, parms_app)
		print(i)
		}	# END REPS
	return(list(outp=outp, parms=parms))
	}	

	
	
	
	
	