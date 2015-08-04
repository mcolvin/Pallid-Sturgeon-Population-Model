

load("./output/total_pop.Rdata")	

input<- list(
	# POPULATION DEMOGRAPHIC RATES
	maxAge=41, 			# [0,90] Maximum age
	S0=0.0001, 			# [0,0.3] Survival age-0 0.051; 0.0001 IS THE SAME AS STEFFENSEN'S ANALYSIS
	S0_cv=0.2,
	S1=0.6, 			# [0.2,1] Survival age-1
	S1_cv=0.3,
	S2=0.8, 			# [0.8,1] Survival age-2               
	S2_cv=0.3,
	S3plus=0.92, 		# [0.8, 1] Survival age-3+  
	S3plus_cv=0.3,
	viableGam = 0.01,	# PROBABILITY OF PRODUCING VIABLE GAMETES GAMETES TO DEVELOPING EMBRYOS
	viableGam_cv=0.1,
	# MATURITY FUNCTION
	mat_mid=5,				# AGE AT 50% SEXUALLY MATURE
	mat_high=9, 			# AGE AT 99% SEXUALLY MATURE

		
  
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
	sp_mid=2,				# AGE AT 50% SEXUALLY MATURE
	sp_high=4, 			# AGE AT 99% SEXUALLY MATURE
	
	# VIABILITY ANALYSIS INPUTS
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




