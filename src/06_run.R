


S0<- rep(0.37,3) # Survival for 0-365 dph
S_g<- 0.001 # "Survival" of oocytes to gametes
S_1<- 0.686 # Survival for Age-1 to Age-2
S_adult<- 0.922 # Survival for >= Age-2


for(i in 1:(nyears-1))
	{	
	juv_spawners_n 	<- rbinom(maxAge,(juv[i,,1]),juv_maturity(c(1:41)))	
	spawners_n		<- sum(juv_spawners_n+recadult3[i,,1])	
		
	juv_spawners_h 	<- rbinom(maxAge,(juv[i,,2]),juv_maturity(c(1:41)))	
	spawners_h		<- sum(juv_spawners_h+recadult3[i,,2])		
				
	gade[i,1]		<- round(sum(spawners_n,spawners_h)*fecundity*sex_ratio *0.0002,0)
	fe[i,1]			<- round(gade[i,1]*S0[1],0)
	efl[i,1]		<- round(fe[i,1]*S0[2],0)
	
	# HATCHERY INPUTS
	gade[i,2]		<- 0 # to be converted to slider input
	fe[i,2]			<- 0 # to be converted to slider input
	efl[i,2]		<- 0 # to be converted to slider input
		
	juv_survival	<- round(c(S_1,rep(S_adult,maxAge-1))*(juv[i,,1] - juv_spawners_n),0)
	juv[i+1,,1]		<- round(c(efl[i,1]*S0[3], juv_survival[-maxAge]),0)
		
	juv_survival	<- round(c(S_1,rep(S_adult,maxAge-1))*(juv[i,,2] - juv_spawners_h),0)
	juv[i+1,,2]		<- round(c(efl[i,2]*S0[3], juv_survival[-maxAge]),0)		
		
	recadult1[i+1,,1]	<- round(c(juv_spawners_n+recadult3[i,,1]),0)
	recadult2[i+1,,1] 	<- round(c(0,(recadult1[i,,1]*S_adult)[-maxAge]),0)
	recadult3[i+1,,1] 	<- round(c(0,(recadult2[i,,1]*S_adult)[-maxAge]),0)
	
	# HATCHERY FISH
	recadult1[i+1,,2]	<- round(c(juv_spawners_h+recadult3[i,,2]),0)
	recadult2[i+1,,2] 	<- round(c(0,(recadult1[i,,2]*S_adult)[-maxAge]),0)
	recadult3[i+1,,2] 	<- round(c(0,(recadult2[i,,2]*S_adult)[-maxAge]),0)	
	}
	

	
	
	adults_n<- apply(recadult1[,,1],1,sum) + apply(recadult2[,,1],1,sum)+apply(recadult3[,,1],1,sum)
	adults_h<- apply(recadult1[,,2],1,sum) + apply(recadult2[,,2],1,sum)+apply(recadult3[,,2],1,sum)
	
	juv_n<- apply(juv[,,1],1,sum)
	juv_h<- apply(juv[,,2],1,sum)
	
	plot(c(1:nyears),adults_h/1000,type='b',ylim=c(0, 45),las=1,ylab="Abundance (x1000)",xlab="Years")
	points(c(1:nyears),adults_n/1000,type='b')
	
	plot(c(1:nyears),juv_h/1000,type='b',ylim=c(0, 45),las=1,ylab="Abundance (x1000)",xlab="Years")
	points(c(1:nyears),juv_n/1000,type='b')
	
	
	
