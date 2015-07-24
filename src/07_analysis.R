input<- list(
	# POPULATION DEMOGRAPHIC RATES
	maxAge=41, 			# [0,90] Maximum age
	S0=0.0001, 			# [0,0.3] Survival age-0 0.051; 0.0001 IS THE SAME AS STEFFENSEN'S ANALYSIS
	S1=0.6, 			# [0.2,1] Survival age-1
	S2=0.8, 			# [0.8,1] Survival age-2               
	S3plus=0.92, 		# [0.8, 1] Survival age-3+  
	viableGam = 0.01,	# PROBABILITY OF PRODUCING VIABLE GAMETES GAMETES TO DEVELOPING EMBRYOS

	# MATURITY FUNCTION
	#aa=5, 				# [0,30] Minimum age of sexual maturity
	#bb=6, 				# [0,30] Age at 25% maturity           
	#cc=7, 				# [0,30] Age at 50% maturity
	#dd=8,				# [0,30] Age at 75% maturity"
	#ee=9,				# [0,30] maximum juvenile age"
	
	mat_mid=5,				# AGE AT 50% SEXUALLY MATURE
	mat_high=9, 			# AGE AT 99% SEXUALLY MATURE

	sp_mid=2,				# AGE AT 50% SEXUALLY MATURE
	sp_high=4, 			# AGE AT 99% SEXUALLY MATURE	
  
	# GROWTH
	Linf = 1683,
	K =0.036,
	t0=-5.9,
  
	# INITIAL STATE VALUES
	sr_n=0.33,			# [0,1] Sex ratio natural origin fish 
	sr_h=0.5, 			# [0,1] Sex ratio hatchery origin fish 
	juv_ini_n=2000,		# [0,100] Natural origin Juvenile (age 1-age -2) fish 
	juv_ini_h=50000,		# [0,100] Hatchery origin Juvenile (age 1-age -2) fish 
	adults_ini_n=500,	# [0,100] Natural origin adult  (>age-2) fish
	adults_ini_h=20000,	# [0,100] Hatchery origin Juvenile (>age-2) fish

	# FECUNDITY
	a_fec= -43.678, 	# from S1001  
	b_fec=72.70, 		# from S1001 

	# SPAWING FREQUENCY
	aaa = 0,			# 0 years since spawning
	bbb = 0.25,			# 1 years since spawning
	ccc = 0.50,			# 2 years since spawning
	ddd = 0.75,			# 3 years since spawning
	eee = 1, 			# 4 years since spawning
	
	# VIABILITY ANALYSIS INPUTS
	#fe_stock=0, 		# [0,1000000] Free embryos stocked
	#efl_stock=0, 		# [0,1000000] Exo. feeding larvae stocked
	#juv_stock=0, 		# [0,100000]
	age0_stock = 10000,	# NUMBER OF AGE-0 FISH STOCKED
	age1_stock = 0,		# NUMBER OF AGE-1 FISH STOCKED
	age2_stock = 0,		# NUMBER OF AGE-2 FISH STOCKED
	age3_stock = 0,		# NUMBER OF AGE-3 FISH STOCKED
	age4_stock = 0,		# NUMBER OF AGE-4 FISH STOCKED
	age5_stock = 0,		# NUMBER OF AGE-5 FISH STOCKED
	age6_stock = 0,		# NUMBER OF AGE-6 FISH STOCKED
	age7_stock = 0,		# NUMBER OF AGE-7 FISH STOCKED

	
	nreps=5,			# [1,100] Number of replicates"
	nyears=100,			# [20,100] Years to simulate
	
	spawn_frequency=1)
	
	# CHANGE VARIABLES
	input$nreps<-2
	input$nyears<- 100
	age0_stock<- c(1000,2000,3000)
	yyy<- data.frame()
	for(i in 1:3)
		{
		input$age0_stock<- age0_stock[i]
		output<-xx_ind(input=input)
		output$scenario<-i
		yyy<- rbind(yyy,output)
		}
	# RESHAPE OUTPUT FROM WIDE TO LONG
	tmp<- reshape(yyy,
		varying = names(yyy) [4:9],
		v.names = "count",
		timevar= "stage",
		times =  names(yyy) [4:9],
		direction = "long")
	
	# SUMMARY OUTPUT TOTAL POPULATION
	total_pop<-dcast(tmp,year+r+scenario~origin, value.var="count",sum,subset=.(age>0))
	total_pop$total<- total_pop$n+total_pop$h
	plot(total~year,total_pop,type='n')
	for(j in 1:max(total_pop$scenario))
		{
		for(i in 1:max(total_pop$r))
			{
			points(total~year, total_pop,
				subset=r==i & scenario==j,
				type='l',col=i)
			}
		}
	
	plot(n~year,total_pop,type='n')
	for(j in 1:max(total_pop$scenario))
		{
		for(i in 1:max(total_pop$r))
			{
			points(n~year, total_pop,
				subset=r==i & scenario==j,
				type='l',col=i)
			}
		}
	plot(h~year,total_pop,type='n')
	for(j in 1:max(total_pop$scenario))
		{
		for(i in 1:max(total_pop$r))
			{
			points(h~year, total_pop,
				subset=r==i & scenario==j,
				type='l',col=i)
			}
		}

		
	plot(h~year,total_pop,type='n')
	for(i in 1:max(total_pop$r))
		{
		points(h~year, total_pop,subset=r==i,type='l',col=i)
		}
	
	plot(n~year,total_pop,type='n')
	for(i in 1:max(total_pop$r))
		{
		points(n~year, total_pop,subset=r==i,type='l',col=i)
		}	
	
	
	
# dont delete this	
lambda<-dcast(out,year~rep,value.var="total",mean)
vals<-(lambda[-1,]+1)/(lambda[-nrow(lambda),]+1)
g_mn<- data.frame(scenario="Median stocking", 
	mn_lambda=apply(vals,2,function(x) exp(mean(log(x)))))	
	
	
	
	
	
	
	
	
	
	
	
	