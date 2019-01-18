## SPATIAL  = TRUE
		if(inputs$spatial==TRUE)
			{		
			### SIMULATE ADULT MOVEMENT AMONG BENDS 
			### DEPENDING ON WHAT BEND FISH IS IN
		  ### AND IF FISH IS SPAWNING
			### STATUS:  DONE BUT SLOW
			BEND_H[indx_H]<- adultMovement(
				previousLocation=BEND_H[indx_H],
				month=m[i],
				spn=SPN_H[indx_H],
				fromToMatrix=inputs$adult_mov_prob,
				spnMatrix=inputs$spn_mov_prob)
			BEND_N[indx_N]<- adultMovement(
				previousLocation=BEND_N[indx_N],
				month=m[i],
				spn=SPN_N[indx_N],
				fromToMatrix=inputs$adult_mov_prob,
				spnMatrix=inputs$spn_mov_prob)
			
			### MOVEMENT OF AGE-0
			### ASSUMES NOT MOVEMENT ONCE INTERCEPTED IN A BEND
			### STATUS: DONE
			### NOTES:  EACH ROW IS A BEND, PROBABLY SHOULD EXPAND TO ALLOW MUTIPLE COHORTS MORE READILY
			# AGE_0_N_BND[]<- AGE_0_N_BND[]
			# AGE_0_H_BND[]<- AGE_0_H_BND[]
			}
			