## PALLID STURGEON STOCKING 
		### STATUS: DONE 
		### NOTE: NEED TO MODIFY TO ALLOW COHORT, PARENT, AND LOCATION INFO
		### PROBABLY MORE EFFICIENT TO RBIND TO REDUCED DATASET RATHER THAN BIG MATRIX OF BENDS
		### COULD DO ONE FOR NATURAL AND HATHCERY AND KEEP TRACK IN LONG FORMAT...
		if(inputs$fingerling > 0 & inputs$fingerling_month==m[i])
			{
			# ADD NUMBER OF FISH STOCKED IN A BEND
			dyn$AGE_0_H_BND[inputs$fingerling$bend,]<- AGE_0_H_BND[inputs$fingerling$bend,]+inputs$fingerling$number
			
			}

		### YEARLING STOCKING (AGE-1+)
		### STOCK INDIVIDUAL FISH INTO BENDS
		### STATUS: DONE 
		if(inputs$yearling>0 & inputs$yearling_month==m[i])
			{
			### GET INDEXES OF OPEN SLOTS TO STICK STOCKED INDIVIDUALS
			indx_R<- lapply(1:inputs$nreps,
				function(x){out<- which(Z_H[,x]==0)[1:inputs$yearling]}) 
			indx_R<- cbind(unlist(indx_R),
				sort(rep(1:inputs$nreps,inputs$yearling)))
				
			### ADD NEWLY STOCKED INDIVIDUALS TO Z_H	
			Z_H[indx_R]<- 1
			### UPDATE LENGTH
			LEN_H[indx_R]<- rnorm(inputs$yearling*inputs$nreps,
				inputs$yearling_mn,
				inputs$yearling_sd)
			### UPDATE 
			WGT_H[indx_R]<- rlnorm(length(indx_R[,1]),
				log(inputs$a*LEN_H[indx_R]^inputs$b),
				inputs$lw_er)	
				
			### ASSIGN AGE	
			AGE_H[indx_R]<- inputs$yearling_age 
			### ASSIGN MATURITY
			MAT_H[indx_R]<- 0	
			### ASSIGN MATURATION STATUS OF NEW RECRUITS
			MAT_H[indx_R]<- 0  
			
			### ASSIGN SEX
			SEX_H[indx_R]<-rbinom(length(indx_R[,1]),
				n=1,
				p=0.5)
			}
		