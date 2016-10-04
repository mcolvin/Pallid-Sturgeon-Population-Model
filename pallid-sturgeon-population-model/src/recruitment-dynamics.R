



		
		
		## RECRUITMENT FREQUENCY > 0
		if(inputs$recruitmentFreq>0 & m[i]%in% c(6,7,8))
			{		
			### RECRUIT AGE-0 FISH TO THE POPULATION
			### NATURAL AND HATCHERY
			### STATUS: NOT DONE
			#if(m[i]%in% c(6,7,8)) 
				#{
				#dyn<- recruitment_to_population(spatial=inputs$spatial)
				#}
			
			### UPDATE SPAWNING [NO|YEST]
			### GIVEN SEXUAL MATURITY AND 
			### TIME SINCE LAST SPAWNING EVENT
			### STATUS: DONE
			SPN_H[indx_H]<-spawn(mps=MPS_H[indx_H],a=inputs$spn_a,
				b=inputs$spn_b,
				mature=MAT_H[indx_H])
			SPN_N[indx_N]<-spawn(mps=MPS_N[indx_N],a=inputs$spn_a,
				b=inputs$spn_b,
				mature=MAT_N[indx_N])
			
			
			### UPDATE THE NUMBER OF EGGS IN A FEMALE 
			### GIVEN SEX, SPAWNING STATUS, AND MATURATION
			### STATUS: DONE BUT SLOW 
			EGGS_H[indx_H]<-fecundity(fl=LEN_H[indx_H],
				a=inputs$fec_a,
				b=inputs$fec_b,
				er=inputs$fec_er,
				sex=SEX_H[indx_H],
				spawn=SPN_H[indx_H],
				mature=MAT_H[indx_H])	
			EGGS_N[indx_N]<-fecundity(fl=LEN_N[indx_N],
				a=inputs$fec_a,
				b=inputs$fec_b,
				er=inputs$fec_er,
				sex=SEX_N[indx_N],
				spawn=SPN_N[indx_N],
				mature=MAT_N[indx_N])			

			### UPDATE THE NUMBER OF MONTHS SINCE SPAWNING
			### FOR FISH JUST SPAWNED
			### STATUS: DONE
			MPS_H[EGGS_H>0]<-0
			MPS_N[EGGS_N>0]<-0
			
			### UPDATE MONTHS SINCE SPAWNING
			MPS_H[indx_H]<-MPS_H[indx_H]+1 	
			MPS_N[indx_N]<-MPS_N[indx_N]+1 	
			} ## RECRUITMENT FREQUENCY > 0	 



		## NUMBER OF EGGS IN EACH BEND
		AGE_0_N_BND<- matrix(colSums(EGGS_N)+
			colSums(EGGS_H),nrow=1) 

			
		### eggs --> embryos	
		### STATUS: DONE NEEDS TO BE MODIFIED WITH DENSITY
		AGE_0_N_BND[]<- rbinom(inputs$nreps,
			AGE_0_N_BND,
			inputs$pr_embryo) 
			
			
		## SURVIVAL OF EMBRYO ---> ???
		### STATUS: DONE BUT NEEDS WORK TO FIX STAGES
		AGE_0_N_BND[]<- rbinom(inputs$nreps,
			AGE_0_N_BND,
			inputs$phi_embryo) # embryos --> free embryos
		AGE_0_N_BND[]<- rbinom(inputs$nreps,
			AGE_0_N_BND,
			inputs$phi_free_embryo) # free embryos --> age0