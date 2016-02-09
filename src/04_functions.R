### VERSION 2

# to do
## 1. set up values in the initialization of indData
## 2. convert movement to rkm rather than bend to bend
## 3. assign bend to rkm

initialize<- function(inputs)
	{
	# INITIALIZE POPULATION
	## SET UP SPATIAL MATRIX [FROM, TO, MONTH]
	sp<- array(0,c(input$n_bends,input$n_bends,12))
	loc_mat<- matrix(0, input$n_bends,input$n_bends)
	for(bend in 1:input$n_bends)
		{
		if(input$spread==0)
			{ # UNIFORM DISTRIBUTION
			values<- rep(1,input$n_bends)
			}
		if(input$spread>0)
			{# GAUSSIAN	
			values<-dnorm(input$rkm,
				input$rkm[bend],input$spread)
			}
		loc_mat[bend,]<- values/sum(values)# NORMALIZE TO 1			
		}
	for(month in 1:12)
		{
		sp[,,month]<-loc_mat# sp:  matrix of monthly movement probabilities
		}
	### END SPATIAL

	# NUMBER OF ADULTS IN EACH BEND GIVEN DENSITY AND BEND LENGTH
	N_adults<- rpois(input$n_bends, 
		exp(log(input$den)+rnorm(input$n_bends,0,input$den_sd) + 
			log(input$rkm))
			)
	# VECTOR OF MONTHS
	m<- rep(c(6:12,1:5),input$nyears) 	
	
	# DISTRIBUTE INVIDUALS ASSUMING EQUILIBRIUM AGE DISTRIBUTION
	ageDist<- cumprod(rep(input$phi,input$maxage))
	ageDist<- ageDist/sum(ageDist)
	
	# SET UP A SUPER POPULATION OF INDIVIDUALS
	indData<-data.table(
		origin=c(rep(1L,input$natural), # 1 natural 
			rep(2L,input$hatchery), # 2 hatchery
			rep(0L, (input$daug-input$hatchery-input$natural))), # slots to fill
		age = c(rep(c(1:input$maxage),rmultinom(1,input$natural,ageDist)),# NATURAL ORIGIN FISH
			rep(c(1:input$maxage),rmultinom(1,input$hatchery,ageDist)),# HATCHERY ORIGIN FISH
			rep(0L,(input$daug-input$hatchery-input$natural))), # OPEN SLOTS
		sex = c(rbinom(input$natural,1,input$sexratio),# NATURAL ORIGIN FISH
			rbinom(input$hatchery,1,input$sexratio), # HATCHERY ORIGIN FISH
			rep(0L, (input$daug-input$hatchery-input$natural))),

		# VBGF
		#k=rep(input$k,input$daug),# can slide in individual k
		k=runif(input$daug,input$k*0.8,input$k*1.2),# can slide in individual k
		Linf=rep(input$linf,input$daug),# can slide in individual Linf
		t0= rep(input$t0,input$daug),# t0 fixed for all fish
		live=c(rep(1, input$hatchery+input$natural),rep(0L,input$daug-input$hatchery-input$natural)),# ALIVE?
		
		# SPAWNING
		mature=rep(0L,input$daug),
		yearsPostSpawn=rep(0L,input$daug),
		spawnNextYear=rep(0L,input$daug),
		
		# SPACE AND TIM
		bend=rep(0L, input$daug),
		eggs=rep(0, input$daug),
		tl=rep(0, input$daug),
		year=1,
		month=5)
	
	# INITIAL LENGTH AT AGE
	indData$tl<-(rlnorm(sum(input$daug),
		log(indData$Linf*(1-exp(-indData$k*(indData$age-input$t0)))),
		input$vb_er))*indData$live	
		
	phi_adults<- input$phi^(1/12) # MONTHLY SURVIVAL
	mov<- array(1, c(input$n_bends,input$n_bends,2))
	# END INITIALIZATION ##########
	return(list(indData=indData, mov=mov,phi_adults=phi_adults))
	}


## PLUGINS
dLength<- function(k, linf,length1,dT)
	{
	# A VECTORIZED FUNCTION 
	L2<- (linf-length1)*(1-exp(-k*dT)) + length1 # FABENS MODEL WITH MODFICATION
	return(L2)
	}

dWeight<- function(a=0.0001,b=3,len,er=0.1)
	{
	weight<- exp(log(a)+b*log(len)+rnorm(length(len),0,er))
	}

dMaturity<- function(maturity,mat_k,age,age_mat,live)
	{
	y<- -mat_k*(age-age_mat)
	y<- -input$mat_k*(indData$age-input$age_mat)
	M2<- rbinom(length(y),1,1/(1+exp(y))*live)
	return(M2)
	}
spawnNextYear<- function(yps,a=-12,b=4.5,mature,live)
	{
	# yps: years post spawn
	y<- a+b*yps
	y<- rbinom(length(yps),1,plogis(y)*mature*live) 
	return(y)	
	}
fecundity<- function(len,wgt,a,b,er,sex,live,spawnNextYear)
	{
	y<- exp(log(a)+b*log(len)+rnorm(input$daug,0,er))
	eggs<-rpois(length(len),y )*live*sex*spawnNextYear
	return(eggs)
	}


sim_pop<- function(inputs)
	{
	# BEGIN INITIALIZATION
	out<- initialize(inputs=input)
	indData<- out$indData
	phi_adults<- out$phi_adults
	mov<- out$mov
	rm(list=c("out"))# clear list from memory
	# END INITIALIZATION
	
	# MONTHLY DYNAMICS ##########
	for(i in 1:length(m)) # M IS A VECTOR OF MONTHS 1:12, REPEATED FOR NYEARS
		{
		# UPDATE TOTAL LENGTH 
		indData$tl<-dLength(k=indData$k, linf=indData$Linf,length1=indData$tl,dT=1/12)*indData$live
		
		# UPDATE WEIGHT
		indData$wgt<-dWeight(a=0.0001,b=3,len=indData$tl,er=0.1)
		
		# UPDATE SEXUAL MATURITY: HAS A FISH REACHED SEXUAL MATURITY?
		## SEXUALLY MATURE: [0=NO,1=YES]
		indData$mature<-dMaturity(maturity=indData$mature,mat_k=input$mat_k,
			age=indData$age,age_mat=input$age_mat,live=indData$live) 
			
		# UPDATE YEARS POST SPAWN
		indData$yearsPostSpawn<-sample(c(0:4),input$daug,replace=TRUE,c(1,1,1,1,1))*indData$mature
			
		# WILL THE FISH SPAWN NEXT YEAR: [0=NO, 1=YES]
		indData$spawnNextYear<-spawnNextYear(yps=indData$yearsPostSpawn,a=-12,b=4.5,
			mature=indData$mature,live=indData$live)
		
		# NUMBER OF EGGS PRODUCED BY LIVE, FEMALE SPAWNING FISH
		indData$eggs<- fecundity(len=indData$tl,wgt=indData$weight,a=input$fec_a,
			b=input$fec_b,er=input$fec_er,sex=indData$sex,
			live=indData$live,spawnNextYear=indData$spawnNextYear)
		
		
		
		# SPAWNING MIGRATION AND AGGREGATION
		## MOVE UPSTREAM AS A PROBABILITY OF FLOW AND TEMPERATURE
		## GO TO A REACH GIVEN SOME PROBABILITY BASED ON HABITAT
		## SPAWN GIVEN A PROBABILITY BASED ON NUMBERS OF FISH
		
		
		# NUMBER OF SEXUALLY MATURE FISH PER BEND
		## THIS NEEDS SOME WORK TO MODEL MOVEMENT AND AGGREGATION
		xxx<- indData[spawnNextYear==1 & sex==1 & live==1, 
			list(female=sum(sex),eggs=sum(eggs)),by=bend]
		xxx<- merge(xxx, indData[live==1 & sex==0, 
			list(male=length(sex)),by=bend],by="bend",all=TRUE)
		xxx[is.na(xxx)]<- 0
		# RECRUITMENT PER BEND
		## EGGS
		xxx$embryo<-ifelse(xxx$male>0,xxx$eggs*input$pr_fert,0)
		xxx$free_embryo<-xxx$embryo*input$phi_1 # CAN DO A SPATIAL REASSIGMENT HERE... A DRIFT PROBABLITY VECTOR/MATRIX
		xxx$efl<- xxx$free_embryo*input$phi_2 
		xxx$age0<- round(xxx$efl*input$phi_3,0)

		if(m[i]==6 & i>1){}# spawning
		
		# SUMMARIZE POPULATION 
		pop_out<- indData[live==1, list(n=sum(live)),by=c("year","month","origin","sex")]
		}
	return(list(pop_out=pop_out))
	}
	

# FUNCTION TO SIMULATE CAPTURE HISTORIES 
sim_ch<- function(input,Z, Z_loc)
	{
	ch<-data.table(matrix(0L,input$daug,input$nprim*input$nsec))
	indx<- sort(rep(c(1:input$nprim),input$nsec)) # index of primary and secondary occasions
	sample_indx<- which(m==input$sample_month)
	p<- matrix(input$p,input$n_bends,input$nprim) # matrix of capture probabilities for each segment and occasion 
	k=0 # counter to index ch matrix columns
	for(i in 1:input$nprim)
		{
		for(j in 1:input$nsec)
			{
			k<-k+1 # COUNTER
			indx<- which(Z[,names(Z)[sample_indx[i]],with=FALSE]==1)
			captured<- rbinom(length(indx), 1, p[unlist(Z_loc[indx,names(Z_loc)[sample_indx],with=FALSE]), i])
			ch[indx,names(ch)[k]:=captured]
			} 
		}
	# Full capture-recapture matrix
	# Remove individuals never captured
	cap.sum <- rowSums(ch)
	ch <- ch[-which(cap.sum == 0),]
	xx<- expand.grid(nprim=paste("X",sample_indx,sep=""),nsec=c(1:input$nsec))
	xx<- xx[order(xx$nprim,xx$nsec),]
	setnames(ch, names(ch),apply(xx,1, paste, collapse="_"))

	Nt <- colSums(Z)    # Actual population size
	Nt_bend<- lapply(sample_indx,function(x) table(Z_loc[,names(Z_loc)[x],with=FALSE]))
	ch_loc<- Z_loc[,sample_indx, with=FALSE]
	setnames(ch_loc, names(ch_loc),paste("X",sample_indx,sep=""))
	return(list(ch=ch,ch_loc=ch_loc, N=Nt))
	}

	### START CAPTURE HISTORY FUNCTION
sim_ch<- function(input=input)
	{
	# SET UP MATRICES
	b<- c(input$b*3, rep(input$b, input$nprim-1))
	PHI<-  matrix(rep(input$phi,(input$nprim-1)*N),ncol=input$nprim-1, nrow=input$N, byrow=TRUE)
	P<- matrix(rep(input$p,input$nprim*input$N),ncol=input$nprim, nrow=input$N, byrow=TRUE)
	B <- rmultinom(1, input$N, b[1:input$nprim]) # Generate no. of entering ind. per occasion
	Z<-matrix(0,input$N,input$nprim)
	# Define a vector with the occasion of entering the population
	ent.occ <- numeric()
	for (t in 1:input$nprim)
		{
		ent.occ <- c(ent.occ, rep(t, B[t]))
		}
	# Write 1 when ind. enters the pop.
	for (i in 1:input$N)
		{
		Z[i, ent.occ[i]] <- 1   
		}
	# SURVIVAL POST RECRUITMENT
	for(i in 1:input$N)
		{
		if(ent.occ[i]<input$nprim)
			{
			for(k in ent.occ[i]:(input$nprim-1))
				{
				Z[i,k+1]<- rbinom(1, 1, PHI[i,k]*Z[i,k])
				}		
			}
		}
	ch<-matrix(0,input$N,input$nprim*input$nsec)
	indx<- sort(rep(c(1:input$nprim),input$nsec))
	# Simulating capture
	for (i in 1:input$N)
		{
		for(j in 1:(input$nsec*input$nprim))
			{
			ch[i,j]<-rbinom(1, 1, P[i,indx[j]]*Z[i,indx[j]])
			} 
		}
		
		
	# INITIAL SPATIAL DYNAMICS
	loc<-matrix(0,input$N,input$nprim)
	if(input$distribution_type=="uniform")
		{
		loc[,1]<-sample( c(1:input$nsegs),input$N,replace=TRUE,prob=rep(1,input$nsegs))	
		}
	if(input$distribution_type=="heterogeneous")
		{
		loc[,1]<-sample( c(1:input$nsegs),input$N,replace=TRUE,prob=runif(input$nsegs))	
		}

	# MIXING TYPE AMONG YEARS
	if(input$mixing=="none")
		{
		for(a in 2:input$nprim){loc[,a]<-loc[,1]}
		}
	if(input$mixing=="complete")
		{
		for(a in 2:input$nprim){loc[,a]<-sample(c(1:input$nsegs),input$N,replace=TRUE,prob=runif(input$nsegs))}
		}
	
	# SAMPLING REACHES	
	id<- c(1:input$N)
	sampleReaches<- sample(c(1:input$nsegs), input$n_reaches,	replace=FALSE)
	indx<- sort(rep(1:input$nprim,input$nsec))
	ch_out<- list()
	for(i in sampleReaches)
		{
		id<- which(Z[,1]==1 & loc[,1]==i)
		if(length(id)==0){out<- data.frame(seg=i,id=0,  ch=matrix(0,nrow=1, ncol=input$nsec))}
		if(length(id)==1){out<- data.frame(seg=i,id=id, ch=matrix(ch[id,which(indx==1)],nrow=1,ncol=input$nsec))}
		if(length(id)>1){out<- data.frame(seg=i,id=id, ch=ch[id,which(indx==1)])}
		names(out)[-c(1,2)]<- paste("ch", 1,c(1:input$nsec),sep="_")
			for(j in 2:input$nprim)
				{
				id<- which(Z[,j]==1 & loc[,j]==i)
				if(length(id)==0){tmpp<- data.frame(seg=i,id=0,  ch=matrix(0,nrow=1, ncol=input$nsec))}
				if(length(id)==1){tmpp<- data.frame(seg=i,id=id, ch=matrix(ch[id,which(indx==j)],nrow=1,ncol=input$nsec))}
				if(length(id)>1){tmpp<- data.frame(seg=i,id=id, ch=ch[id,which(indx==j)])}
				names(tmpp)[-c(1,2)]<- paste("ch", j,c(1:input$nsec),sep="_")
				out<- merge(out, tmpp,by=c("seg","id"), all=TRUE)
				}
				out[is.na(out)]<-0
				ch_out[[length(ch_out)+1]]<- out
		}
	
	return(list(Z=Z,ch=ch, loc=loc, ch_out=ch_out))
	}
### END CAPTURE HISTORY FUNCTION	
	
	

### END VERSION 2


	
	

	
	
	
	
	
	
	