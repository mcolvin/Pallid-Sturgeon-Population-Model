

### CORE FUNCTION TO DO SIMULATIONS	
sim<- function(inputs,dyn)
	{
	## assign objects from dyn
	k<-dyn$k
	Linf<-dyn$Linf
	LEN<-dyn$LEN
	WGT<-dyn$WGT
	Z<-dyn$Z
	AGE<-dyn$AGE
	MAT<-dyn$MAT
	MPS<-dyn$MPS
	SEX<-dyn$SEX
	SPN<-dyn$SPN
	ORIGIN<-dyn$ORIGIN
	EGGS<-dyn$EGGS
	AGE_0_N_BND<-dyn$AGE_0_N_BND
	AGE_0_H_BND<-dyn$AGE_0_H_BND
	m<-dyn$m
	rm(list='dyn')
	
	
	# SUMMARIES
	## ABUNDANCE (AGE-1+)
	N_REC<-MEANWEIGHT<-BIOMASS<-N_NAT<-N_HAT<- matrix(0,nrow=length(m),ncol=inputs$nreps)


	#sq<-qp<-pm<-mt<-tr<-matrix(0,ncol=input$nreps,nrow=length(m))# MATRIX
	#init_summary<- data.frame(len=LEN[,1],
	#	linf=Linf[,1],
	#	k=k[,1],
	#	age=AGE[,1])
	
ptm <- proc.time()
	# PROGRESS BAR
	pb<-txtProgressBar(min=1,max=length(m),initial=0,char="*",style=3)
	
#Rprof("out.out") # for profiling in for loop

	# SIMULATE POPULATION DYNAMICS GIVEN INITIAL STATES
	for(i in 1:length(m)) # M IS A VECTOR OF MONTHS 1:12, REPEATED FOR NYEARS
		{
		setTxtProgressBar(pb, i)

		indx<- lapply(1:inputs$nreps,function(x){which(Z[,x]==1)}) # ROW INDICES
		tmp<- unlist(lapply(1:inputs$nreps,function(x) rep(x,length(indx[[x]])))) # COLUMN INDICES
		indx<- cbind(unlist(indx),tmp)#row,column
	
		### UPDATE TOTAL LENGTH 
		LEN[indx]<-dLength(k=k[indx],
			linf=Linf[indx],
			dT=1/12,
			length1=LEN[indx])
		### UPDATE WEIGHT # slow
		WGT[indx]<-dWeight(len=LEN[indx],
			a=inputs$a,
			b=inputs$b,
			er=inputs$lw_er)	
		### UPDATE WHETHER A FISH WILL SPAWN
		SPN[indx]<-spawn(mps=MPS[indx],a=inputs$spn_a,
			b=inputs$spn_b,
			mature=MAT[indx])*inputs$recruitment			
		### RECRUIT AGE-0 FISH TO THE POPULATION; NATURAL AND HATCHERY
		#if(m[i]==6) 
			#{
			#dyn<- recruitment_to_population(spatial=inputs$spatial)
			#}

			
		## UPDATE THE NUMBER OF EGGS IN A FEMALE
		EGGS[indx]<-fecundity(fl=LEN[indx],
			a=inputs$fec_a,
			b=inputs$fec_b,
			er=inputs$fec_er,
			sex=SEX[indx],
			spawn=SPN[indx],
			mature=MAT[indx])	
		
		## UPDATE THE NUMBER OF MONTHS SINCE SPAWNING FOR FISH JUST SPAWNED
		MPS[EGGS>0]<-0
		
		AGE_0_N_BND<- matrix(colSums(EGGS),nrow=1) # NUMBER OF EGGS
		AGE_0_N_BND[]<- rbinom(inputs$nreps,AGE_0_N_BND,inputs$pr_embryo) # eggs --> embryos
		AGE_0_N_BND[]<- rbinom(inputs$nreps,AGE_0_N_BND,inputs$phi_embryo) # embryos --> free embryos
		AGE_0_N_BND[]<- rbinom(inputs$nreps,AGE_0_N_BND,inputs$phi_free_embryo) # free embryos --> age0
			
		# UPDATE MONTHS SINCE SPAWNING
		MPS[indx]<-MPS[indx]+1 	
		
		
		# PALLID STURGEON STOCKING 
	if(inputs$fingerling > 0 & inputs$fingerling_month==m[i])
		{
		# ADD NUMBER OF FISH STOCKED IN A BEND
		dyn$AGE_0_H_BND[inputs$stocking_bend,]<- AGE_0_H_BND[inputs$stocking_bend,]+inputs$fingerling
		}

	## YEARLING STOCKING (AGE-1+); INDIVIDUALS
	if(inputs$yearling>0 & inputs$yearling_month==m[i])
		{
		### GET INDEXES OF OPEN SLOTS TO STICK STOCKED INDIVIDUALS
		indx<- lapply(1:inputs$nreps,function(x){out<- which(Z[,x]==0)[1:inputs$yearling]}) 
		indx<- cbind(unlist(indx),sort(rep(1:inputs$nreps,inputs$yearling)))
		Z[indx]<- 1### ADD NEWLY STOCKED INDIVIDUALS TO Z_H
		LEN[indx]<- rnorm(inputs$yearling*inputs$nreps,inputs$yearling_mn,inputs$yearling_sd)# UPDATE LENGTH
		WGT[indx]<- rlnorm(length(indx[,1]),log(inputs$a*LEN[indx]^inputs$b),inputs$lw_er)	# UPDATE 
		AGE[indx]<- inputs$yearling_age # ASSIGN AGE
		MAT[indx]<- 0	# ASSIGN MATURITY
		#RKM[indx]<- inputs$yearling_stocking_rkm# ASSIGN LOCATION OF STOCKED INDIVIDUALS
		MAT[indx]<- 0  # ASSIGN MATURATION STATUS OF NEW RECRUITS
		SEX[indx]<-rbinom(length(indx[,1]),1,0.5)# ASSIGN SEX TO RECRUITS	
		ORIGIN[indx]<-1
		}
		
		
		# need to draw new linf and k for new fish...				



		# SUMMARIES #####################################################################
		## ABUNDANCE AGE-1+
		N_NAT[i,]<-colSums(Z) - colSums(Z*ORIGIN)
		N_HAT[i,]<-colSums(Z*ORIGIN)
		## BIOMASS
		BIOMASS[i,]<- colSums(WGT)
		## MEAN WEIGHT
		MEANWEIGHT[i,]<- colSums(WGT)/colSums(Z)		

		## AGE-1 RECRUITS; NATURAL ORIGIN
		indx<- lapply(1:inputs$nreps,function(x)
			{
			out<- which(AGE[,x]>0 & AGE[,x]<24 & ORIGIN[,x]==0 & Z[,x]==1)
			}) 		
		N_REC[i,]<- sapply(1:inputs$nreps,function(x) length(indx[[x]]))

		# END SUMMARIES #################################################################
		}# end i 
#Rprof(NULL)
#summaryRprof(out.out”)
proc.time() - ptm

	x<- sort(rep(inputs$startYear:(inputs$startYear+inputs$nyears-1),12))+rep(1:12/12,inputs$nyears)
	out<-list(total=N_SUM, 
		years=x,
		sq=sq,qp=qp,pm=pm,mt=mt,tr=tr,
		biomass=biomass,
		mn_wght=mn_wght,
		init_summary=init_summary)
		
	#fn<-paste0("./output/",inputs$output_name,"/",inputs$output_name,"-output.txt")
	#dput(out,fn)
	#fn<-paste0("./output/",inputs$output_name,"/",inputs$output_name,"-input.txt")
	#dput(inputs,fn)
	#fn<-paste0("./output/",inputs$output_name,"/",inputs$output_name,".Rdata")
	#save(out,inputs, file=fn)
	return(out)	
	}

