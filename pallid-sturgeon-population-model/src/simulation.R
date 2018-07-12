

### CORE FUNCTION TO DO SIMULATIONS	
sim<- function(inputs,dyn,recruitmentFreq=0,sizeStructure=FALSE)
	{
	inputs$recruitmentFreq<-recruitmentFreq
	inputs$sizeStructure<-sizeStructure
	
	## ASSIGN OBJECTS FROM DYN INPUT
	k_H<-dyn$k_H
	k_N<-dyn$k_N
	
	Linf_H<-dyn$Linf_H
	Linf_N<-dyn$Linf_N
	
	LEN_H<-dyn$LEN_H
	LEN_N<-dyn$LEN_N
	
	WGT_H<-dyn$WGT_H
	WGT_N<-dyn$WGT_N
	
	Z_H<-dyn$Z_H
	Z_N<-dyn$Z_N
	
	AGE_H<-dyn$AGE_H
	AGE_N<-dyn$AGE_N

	MAT_H<-dyn$MAT_H
	MAT_N<-dyn$MAT_N
	
	MPS_H<-dyn$MPS_H
	MPS_N<-dyn$MPS_N
	
	SEX_H<-dyn$SEX_H
	SEX_N<-dyn$SEX_N
	
	SPN_H<-dyn$SPN_H
	SPN_N<-dyn$SPN_N
	
	EGGS_H<-dyn$EGGS_H
	EGGS_N<-dyn$EGGS_N
	
	AGE_0_N_BND<-dyn$AGE_0_N_BND
	AGE_0_H_BND<-dyn$AGE_0_H_BND
	
	m<-dyn$m
	

	if(inputs$spatial==TRUE)
		{
		BEND_N <- dyn$BEND_N
		BEND_H <- dyn$BEND_H
		}
	
	#rm(list='dyn')	
	
	# SET UP SUMMARIES
	RECRUITS<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	
	MEANWEIGHT_N<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	MEANWEIGHT_H<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	MEANWEIGHT<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	
	MEANLENGTH_N<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	MEANLENGTH_H<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	MEANLENGTH<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	
	BIOMASS_N<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	BIOMASS_H<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	BIOMASS<-matrix(0,nrow=length(m),ncol=inputs$nreps)
	
	N_NAT<- matrix(0,nrow=length(m),ncol=inputs$nreps)
	N_HAT<- matrix(0,nrow=length(m),ncol=inputs$nreps)

	
	if(inputs$sizeStructure==TRUE){ss<- list()}
	# END SUMMARIES

	
	# SET UP INDICES PRIOR TO RUNNING MODEL
	indx_H<- lapply(1:inputs$nreps,
		function(x){which(Z_H[,x]==1)}) # ROW INDICES
	tmp<- unlist(lapply(1:inputs$nreps,
		function(x) rep(x,length(indx_H[[x]])))) # COLUMN INDICES
	indx_H<- cbind(unlist(indx_H),tmp)#row,column

	indx_N<- lapply(1:inputs$nreps,
		function(x){which(Z_N[,x]==1)}) # ROW INDICES
	tmp<- unlist(lapply(1:inputs$nreps,
		function(x) rep(x,length(indx_N[[x]])))) # COLUMN INDICES
	indx_N<- cbind(unlist(indx_N),tmp)#row,column
		

	# PROGRESS BAR
	pb<-txtProgressBar(min=1,max=length(m),initial=0,char="*",style=3)

    
    
    
    
	# SIMULATE POPULATION DYNAMICS GIVEN INITIAL STATES
	for(i in 1:length(m)) 
		{
		setTxtProgressBar(pb, i)
        
		Z_H[indx_H]<- dSurvival(phi_age=inputs$phi,
			age=AGE_H[indx_H],
			maxAge=inputs$maxage)
		AGE_H<- (AGE_H+1/12)*Z_H ### UPDATE AGE FOR SURVIVORS		
		
        
        Z_N[indx_N]<- dSurvival(phi_age=inputs$phi,
			age=AGE_N[indx_N],
			maxAge=inputs$maxage)
		AGE_N<- (AGE_N+1/12)*Z_N ### UPDATE AGE FOR SURVIVORS
		
		### ZERO OUT FISH THAT DIED
		LEN_H<- LEN_H*Z_H
		LEN_N<- LEN_N*Z_N
        
		WGT_H<- WGT_H*Z_H
		WGT_N<- WGT_N*Z_N	

		### ADD IN AGE-1 RECRUITS
        if(sum(AGE_0_N_BND)>0 & m[i]==6)
            {
            indx_0<- lapply(1:inputs$nreps,function(x)
                {
                which(Z_N[,x]==0)[1:AGE_0_N_BND[,x]]
                }) # ROW INDICES
            tmp<- unlist(lapply(1:inputs$nreps,
                function(x) rep(x,length(indx_0[[x]])))) # COLUMN INDICES
            indx_0<- cbind(unlist(indx_0),tmp)#row,column
            ## ADD NEW AGE-1 RECRUITS TO THE POPULATION    
            Z_N[indx_0]<-1    #####FIXME##### NEED TO ADD LENGTH AND SO ON
            AGE_N[indx_0]<-12  
            print(AGE_0_N_BND)
            }


		### INDICES FOR FISH THAT ARE ALIVE
		### AND SUBJECT TO DEMOGRAPHIC PROCESSES
		indx_H<- lapply(1:inputs$nreps,
			function(x){which(Z_H[,x]==1)}) # ROW INDICES
		tmp<- unlist(lapply(1:inputs$nreps,
			function(x) rep(x,length(indx_H[[x]])))) # COLUMN INDICES
		indx_H<- cbind(unlist(indx_H),tmp)#row,column
	
		indx_N<- lapply(1:inputs$nreps,
			function(x){which(Z_N[,x]==1)}) # ROW INDICES
		tmp<- unlist(lapply(1:inputs$nreps,
			function(x) rep(x,length(indx_N[[x]])))) # COLUMN INDICES
		indx_N<- cbind(unlist(indx_N),tmp)#row,column

			

			
		### UPDATE TOTAL LENGTH 
		LEN_H[indx_H]<-dLength(k=k_H[indx_H],
			linf=Linf_H[indx_H],
			dT=1/12,
			length1=LEN_H[indx_H])
		LEN_N[indx_N]<-dLength(k=k_N[indx_N],
			linf=Linf_N[indx_N],
			dT=1/12,
			length1=LEN_N[indx_N])			
		
		
		### UPDATE WEIGHT
		###	GIVEN NEW LENGTH
		### STATUS: DONE BUT SLOW
		WGT_H[indx_H]<-dWeight(len=LEN_H[indx_H],
			a=inputs$a,
			b=inputs$b,
			er=inputs$lw_er)
		WGT_N[indx_N]<-dWeight(len=LEN_N[indx_N],
			a=inputs$a,
			b=inputs$b,
			er=inputs$lw_er)
            
            
        ## RECRUITMENT    
		source("./src/recruitment-dynamics.R")

            
		if(inputs$spatial==TRUE)
			{	
			source("./src/spatial-dynamics.R")
			}
		if(!(is.null(inputs$stocking)))  #####fixme#####
			{
			source("./src/stocking-dynamics.R")
			}
            
        
		# END POPULATION DYNAMICS
		
	
		## SUMMARIES 
		
		### ABUNDANCE AGE-1+
		N_NAT[i,]<-colSums(Z_N) 
		N_HAT[i,]<-colSums(Z_H) 
	
	
		### BIOMASS IN KILOGRAMS
		BIOMASS_N[i,]<- colSums(WGT_N)/1000
		BIOMASS_H[i,]<- colSums(WGT_H)/1000
		BIOMASS[i,]<- colSums(WGT_H)+colSums(WGT_N)/1000 
	
	
		### MEAN WEIGHT IN KILOGRAMS
		MEANWEIGHT_N[i,]<- (colSums(WGT_N)/colSums(Z_N))/1000 
		MEANWEIGHT_H[i,]<- (colSums(WGT_H)/colSums(Z_H))/1000 
		MEANWEIGHT[i,]<-((colSums(WGT_H)+colSums(WGT_N))/(colSums(Z_H)+colSums(Z_N)))/1000 
	
	
		### MEAN LENGTH IN MM
		MEANLENGTH_N[i,]<- colSums(LEN_N)/colSums(Z_N)	
		MEANLENGTH_H[i,]<- colSums(LEN_H)/colSums(Z_H)
		MEANLENGTH[i,]<-(colSums(LEN_H)+ colSums(LEN_N))/(colSums(Z_H)+colSums(Z_N))

		
		### AGE-1 RECRUITS; NATURAL ORIGIN
		indx_R<- lapply(1:inputs$nreps,function(x)
			{
			out<- which(AGE_N[,x]>0 & AGE_N[,x]<24 & Z_N[,x]==1)
			}) 		
		RECRUITS[i,]<- sapply(1:inputs$nreps,
			function(x)
				{ 
				length(indx_R[[x]])
				})
		
		### SIZE STRUCTURE
		if(inputs$sizeStructure==TRUE)
			{
			ss[[i]]<-process_size_structure(year=i/(length(m))*inputs$nyears,
				L_N=LEN_N,L_H=LEN_H)
			}

		}# end i 


	out<-list(
		total_N=N_NAT,
		total_H=N_HAT,
		months=m,
		recruits=RECRUITS,
		biomass_n=BIOMASS_N,
		biomass_h=BIOMASS_H,
		biomass=BIOMASS,
		mean_weight_n=MEANWEIGHT_N,
		mean_weight_h=MEANWEIGHT_H,
		mean_weight=MEANWEIGHT,
		mean_length_n=MEANLENGTH_N,
		mean_length_h=MEANLENGTH_H,
		mean_length=MEANLENGTH,
		post_length=c(LEN_H[LEN_H[,1]>0,1],LEN_N[LEN_N[,1]>0,1]), # LENGTH DISTRIBUTION POST SIMULATION
		post_weight=c(WGT_H[WGT_H[,1]>0,1],WGT_N[WGT_N[,1]>0,1]), # WEIGHT DISTRIBUTION POST SIMULATION
		years=inputs$startYear+cumsum(rep(1,length(m))/12),
		#init_summary=init_summary, ####fixme#####
		inputs=inputs)
		
	# COMPILE UP ANNUAL SIZE STRUCTURE TALLIES FOR OUTPUT
	##out$size_structure<- do.call("rbind",ss)
	
	return(out)	
	}
