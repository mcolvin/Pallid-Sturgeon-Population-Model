## SPATIAL  = TRUE
		if(inputs$spatial==TRUE)
			{		
			### SIMULATE ADULT MOVEMENT AMONG BENDS 
			### DEPENDING ON WHAT BEND FISH IS IN 
			### STATUS:  DONE BUT SLOW
			BEND_H[indx_H]<- adultMovement(
				previousLocation=BEND_H[indx_H],
				fromToMatrix=inputs$adult_prob)
			BEND_N[indx_N]<- adultMovement(
				previousLocation=BEND_N[indx_N],
				fromToMatrix=inputs$adult_prob)
				
				
			### MOVEMENT OF AGE-0
			### ASSUMES NOT MOVEMENT ONCE INTERCEPTED IN A BEND
			### STATUS: DONE
			### NOTES:  EACH ROW IS A BEND, PROBABLY SHOULD EXPAND TO ALLOW MUTIPLE COHORTS MORE READILY
			# AGE_0_N_BND[]<- AGE_0_N_BND[]
			# AGE_0_H_BND[]<- AGE_0_H_BND[]
			}
			