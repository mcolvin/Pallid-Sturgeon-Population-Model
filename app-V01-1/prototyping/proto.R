input<-list(		
	maxAge=41,
	S0=0.051,
	S1=0.6,
	S2=0.8   ,               
	S3plus=0.92,                     
	aa=5,
	a=6  ,         
	b=7,
	c=8,
	d=9,
	a_fec=3.48,
	b_fec=4.05,
	sr_n=0.33,
	sr_h=0.5,
	juv_ini_n=50, 
	juv_ini_h=50,
	adults_ini_n=50,
	adults_ini_h=50,
	fe_stock=0,
	efl_stock=0,
	juv_stock=0 ,                         
	nreps=5 ,                        
	nyears=50)
	
maturity<- function(){#reactive({
  # 1. MATURITY FUNCTION
  x<-c(0,input$aa, input$a,input$b,input$c,input$d,input$maxAge)
  y<-c(0,0,        0.25,    0.5,   0.75,   1,    1)
  mat<- approxfun(x,y)
  out<- data.frame(age=c(1:input$maxAge),maturity=mat(c(1:input$maxAge)))
  return(out)
  }#})	
	
inits<- function(){# reactive({
    # INITIALIZE POPULATION

    ## CREATE SURVIVOR FUNCTION (NO OUTER LOOP)
    S<- c(input$S1,input$S2,rep(input$S3plus, input$maxAge-2))
    S<- S[c(1,2,rep(3,input$maxAge-2))]
    ## INITIALIZE JUVENILES
    max_juv<- 9
    S_juv<- S
    S_juv[max_juv:input$maxAge]<-0
    S_juv<- cumprod(S_juv)
    S_juv<- S_juv/sum(S_juv)
    juv_n<- rmultinom(1,(input$juv_ini_n*1000),S_juv)
    juv_h<- rmultinom(1,(input$juv_ini_h*1000),S_juv)

    ## ASSIGN MALES AND FEMALES
    adults_n_f<- rbinom(1, (input$adults_ini_n*1000), input$sr_n)
    adults_n_m<- (input$adults_ini_n*1000)-adults_n_f
    adults_h_f<- rbinom(1,(input$adults_ini_h*1000),input$sr_h)
    adults_h_m<- (input$adults_ini_h*1000)-adults_h_f
    
    ## ASSIGN ADULTS TO AGE
    S_ad<-S
    S_ad[1:max_juv]<-0
    S_ad[(max_juv+1):input$maxAge]<-cumprod(S[(max_juv+1):input$maxAge])
    adult_n_f_age<-rmultinom(1,adults_n_f,prob=S_ad)
    adult_n_m_age<-rmultinom(1,adults_n_m ,prob=S_ad)
    adult_h_f_age<-rmultinom(1,adults_h_f,prob=S_ad)
    adult_h_m_age<-rmultinom(1,adults_h_m,prob=S_ad)
    
    out<- list(survivals=data.frame(age=c(0:input$maxAge),survival=c(input$S0,S),S_ad=c(0,S_ad)),
               inits=data.frame(age=c(1:input$maxAge),
                                j_h=juv_h,
                                j_n=juv_n,                                
                                a_h_f=adult_h_f_age,
                                a_h_m=adult_h_m_age,
                                a_n_f=adult_n_f_age,
                                a_n_m=adult_n_m_age))
    return(out)
    }#})	
	
xx<- function(){#reactive({
#	out<- data.frame()
	j_n_out<-j_h_out<-a_h_m_out<-a_h_f_out<-a_n_f_out<-a_n_m_out<-data.frame()
	for(j in 1:input$nreps)
		{
		maxAge<- input$maxAge
		Linf=1683
		K=0.036
		t0=-5.9
		FL<-Linf*(1-exp(-K*(c(1:maxAge)-t0))) # CALCULATE FORK LENGTH FOR EACH AGE
		mat_dat<- maturity()
		dat<- inits()$inits
		survival<-inits()$survivals$survival[-1]# drop age-0 survival
		a_n_f<-a_n_m<-a_h_f<-a_h_m<-j_h<-j_n<- matrix(0,nrow=maxAge,ncol=input$nyears)
		
		# SET INITIAL VALUES FOR EACH STAGE AND ORIGIN
		## COLUMNS INDEX YEAR
		## ROWS INDEX AGE
		a_n_f[,1]	<- dat$a_n_f
		a_n_m[,1]	<- dat$a_n_m
		a_h_f[,1]	<- dat$a_h_f
		a_h_m[,1]	<- dat$a_h_m
		j_h[,1]		<- dat$j_h
		j_n[,1]		<- dat$j_n  
		 
		for(i in 2:input$nyears)
			{
			# STOCKED INTO THE SYSTEM.....
			stocked<-rpois(1,round(input$fe_stock*0.051+
				input$efl_stock*0.051^0.666+
				input$juv_stock*0.051^0.333,0))
			nxt<- rbinom(maxAge,j_h[,i-1],survival)
			nxt_mat_h<- rbinom(maxAge,nxt,mat_dat$maturity)
			nxt<- nxt-nxt_mat_h
			j_h[,i]<-c(stocked,nxt[-maxAge])
			  
			  
			# FECUDNITY
			eggs<-sum((input$a_fec*10^-8 * FL^input$b_fec)*(a_h_f[,i-1]+a_n_f[,i-1]))
               	
			  
			age0<- sum(a_h_f[,i]*a_n_f[,i])*0.25*2
			nxt<- rbinom(maxAge,j_n[,i-1],survival)
			nxt_mat_n<- rbinom(maxAge,nxt,mat_dat$maturity)
			nxt<- nxt-nxt_mat_n  
			j_n[,i]<-c(age0,nxt[-maxAge])  
				
			# NUMBER OF ADULT NATURAL ORIGIN FEMALES  
			a_n_f[,i]<-sim_nxt_adults(matured=nxt_mat_n,sexRatio=0.5,
			prev=a_n_f[,i-1],S=survival,tmax=maxAge)

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
#	return(out)    
#	})