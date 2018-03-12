plot_dynamics<- function(
	dat=dat,
	org='n' ,
	spawn_freq=1 , 
	age0_st=0 ,
	age1_st=0,
	lab="a)",
	xaxis="n")
	{
	plotdat<- subset(dat, 
		origin==org &
		spawn_frequency==spawn_freq & 
		age0_stock==age0_st &
		age1_stock==age1_st)
	if(xaxis=='n'){
		plot(abundance/1000~year, plotdat, 
			type='n',ylab="",xlab="",las=1,xaxt='n')
		axis(1,at=axTicks(1),labels=FALSE)} else {
		plot(abundance/1000~year, plotdat, 
			type='n',ylab="",xlab="",las=1)	
		}
	sapply(unique(plotdat$scenario),function(i)
		{
		points(abundance/1000~year, plotdat, type='l',
			subset=scenario==i,col='grey')
		})
	panLab(lab)			
	}

	
	par(mfrow=c(2,2))
	org<-'n'
	# AGE-0 STOCKING
	plotdat<- subset(sims_lower, 
		origin==org &
		age1_stock==0&
		year %in% c(50,100))
	plotdat$abundance<- ifelse(plotdat$abundance>1,1,0)
	pex<- plotdat[,j=list(
		pex=mean(abundance)),
		by=list(year,age0_stock,
		spawn_frequency)]
	pex<- pex[order(year,age0_stock,spawn_frequency),]
	plot(pex~age0_stock,pex,type='n',
		las=1)
	vals<- unique(pex$spawn_frequency)
	for(i in 1:length(vals))
		{
		points(pex~age0_stock,pex,
			subset=spawn_frequency==vals[i] &
			year==50, type='b',pch=letters[i])
		}
	# AGE-0 STOCKING 
	# YEAR 100
	pex<- plotdat[,j=list(
		pex=mean(abundance)),
		by=list(year,age0_stock,
		spawn_frequency)]
	pex<- pex[order(year,age0_stock,spawn_frequency),]
	plot(pex~age0_stock,pex,type='n',
		las=1)
	vals<- unique(pex$spawn_frequency)
	for(i in 1:length(vals))
		{
		points(pex~age0_stock,pex,
			subset=spawn_frequency==vals[i] &
			year==100, type='b',pch=letters[i])
		}
	
	# AGE-1 STOCKING AND PEX
	# 50 YEAR
	plotdat<- subset(sims, 
		origin==org &
		age0_stock==0 &
		year %in% c(50,100))
	plotdat$abundance<- ifelse(plotdat$abundance>1,1,0)
	pex<- plotdat[,j=list(
			pex=mean(abundance)),
			by=list(year,age1_stock,
			spawn_frequency)]
	pex<- pex[order(year,age1_stock,spawn_frequency),]
	plot(pex~age1_stock,pex,type='n',
		las=1)
	vals<- unique(pex$spawn_frequency)
	for(i in 1:length(vals))
		{
		points(pex~age1_stock,pex,
			subset=spawn_frequency==vals[i] &
			year==50, type='b',pch=letters[i])
		}

	# AGE-1 STOCKING AND PEX
	# 100 YEAR
	pex<- plotdat[,j=list(
			pex=mean(abundance)),
			by=list(year,age1_stock,
			spawn_frequency)]
	pex<- pex[order(year,age1_stock,spawn_frequency),]
	plot(pex~age1_stock,pex,type='n',
		las=1)
	vals<- unique(pex$spawn_frequency)
	for(i in 1:length(vals))
		{
		points(pex~age1_stock,pex,
			subset=spawn_frequency==vals[i] &
			year==100, type='b',pch=letters[i])
		}

	
	
	
	
figures<-function(n)
	{	
	if(n==1)
		{# PLOT OF NATURAL ORIGIN DYNAMICS
		trans_grey<- rgb(120,120,120,alpha=10,maxColorValue=255)
		par(mfrow=c(3,2),mar=c(1,3,0,0),
			oma=c(3,3,1,1))

		# AGE-0 STOCKING
		sp_freq<-1
			## STOCKING LEVELS SPAWNING EVERY YEAR
		plot_dynamics(
			dat=sims_lower,
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=0 ,
			age1_st=0,
			lab="a) Age-0 stocking = 0",
			xaxis="n")
		plot_dynamics(
			dat=sims_lower,
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=10000 ,
			age1_st=0,
			lab="a) Age-0 stocking = 10000",
			xaxis="n")
		plot_dynamics(
			dat=sims_lower,
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=20000 ,
			age1_st=0,
			lab="a) Age-0 stocking = 20000",
			xaxis="n")		
		plot_dynamics(
			dat=sims_lower,
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=30000 ,
			age1_st=0,
			lab="a) Age-0 stocking = 30000",
			xaxis="n")			
		plot_dynamics(
			dat=sims_lower,
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=40000 ,
			age1_st=0,
			lab="a) Age-0 stocking = 40000",
			xaxis="n")			
			mtext(side=1, "Year",line=1.5,outer=TRUE)
			mtext(side=2, "Abundance (x1000)",line=1,outer=TRUE)	
		}

	if(n==2)
		{
		trans_grey<- rgb(120,120,120,alpha=10,maxColorValue=255)
		par(mfrow=c(3,2),mar=c(1,3,0,0),
			oma=c(3,3,1,1))
		# AGE-0 STOCKING
		sp_freq<-1
			## STOCKING LEVELS SPAWNING EVERY YEAR
		plot_dynamics(
			dat=sims_lower,
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=0,
			age1_st=0,
			lab="a) Age-1 stocking = 0",
			xaxis="n")
		plot_dynamics(
			dat=sims_lower,
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=0,
			age1_st=2000,
			lab="b) Age-1 stocking = 2000",
			xaxis="n")
		plot_dynamics(
			dat=sims_lower,
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=0,
			age1_st=4000,
			lab="c) Age-1 stocking = 4000",
			xaxis="n")			
		plot_dynamics(
			dat=sims_lower,
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=0,
			age1_st=6000,
			lab="d) Age-1 stocking = 6000",
			xaxis="n")				
		plot_dynamics(
			dat=sims_lower,
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=0,
			age1_st=8000,
			lab="e) Age-1 stocking = 8000",
			xaxis="y")				
		plot_dynamics(
			dat=sims_lower,
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=0,
			age1_st=10000,
			lab="f) Age-1 stocking = 10000",
			xaxis="y")				
		mtext(side=1, "Year",line=1.5,outer=TRUE)
		mtext(side=2, "Abundance (x1000)",line=1,outer=TRUE)	
		}
		
	if(n==3)
		{# PLOT OF NATURAL ORIGIN DYNAMICS
		trans_grey<- rgb(120,120,120,alpha=10,maxColorValue=255)
		par(mfrow=c(3,2),mar=c(1,3,0,0),
			oma=c(3,3,1,1))

		# AGE-0 STOCKING
		sp_freq<-2
			## STOCKING LEVELS SPAWNING EVERY YEAR
		plot_dynamics(
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=0 ,
			age1_st=0,
			lab="a) Age-0 stocking = 0",
			xaxis="n")
		plot_dynamics(
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=10000 ,
			age1_st=0,
			lab="a) Age-0 stocking = 10000",
			xaxis="n")
		plot_dynamics(
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=20000 ,
			age1_st=0,
			lab="a) Age-0 stocking = 20000",
			xaxis="n")		
		plot_dynamics(
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=30000 ,
			age1_st=0,
			lab="a) Age-0 stocking = 30000",
			xaxis="n")			
		plot_dynamics(
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=40000 ,
			age1_st=0,
			lab="a) Age-0 stocking = 40000",
			xaxis="n")			
			mtext(side=1, "Year",line=1.5,outer=TRUE)
			mtext(side=2, "Abundance (x1000)",line=1,outer=TRUE)	
		}

	if(n==4)
		{
		trans_grey<- rgb(120,120,120,alpha=10,maxColorValue=255)
		par(mfrow=c(3,2),mar=c(1,3,0,0),
			oma=c(3,3,1,1))
		# AGE-0 STOCKING
		sp_freq<-2
			## STOCKING LEVELS SPAWNING EVERY YEAR
		plot_dynamics(
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=0,
			age1_st=0,
			lab="a) Age-1 stocking = 0",
			xaxis="n")
		plot_dynamics(
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=0,
			age1_st=2000,
			lab="b) Age-1 stocking = 2000",
			xaxis="n")
		plot_dynamics(
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=0,
			age1_st=4000,
			lab="c) Age-1 stocking = 4000",
			xaxis="n")			
		plot_dynamics(
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=0,
			age1_st=6000,
			lab="d) Age-1 stocking = 6000",
			xaxis="n")				
		plot_dynamics(
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=0,
			age1_st=8000,
			lab="e) Age-1 stocking = 8000",
			xaxis="y")				
		plot_dynamics(
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=0,
			age1_st=10000,
			lab="f) Age-1 stocking = 10000",
			xaxis="y")				
		mtext(side=1, "Year",line=1.5,outer=TRUE)
		mtext(side=2, "Abundance (x1000)",line=1,outer=TRUE)	
		}
		
	if(n==5)
		{# PLOT OF NATURAL ORIGIN DYNAMICS
		trans_grey<- rgb(120,120,120,alpha=10,maxColorValue=255)
		par(mfrow=c(3,2),mar=c(1,3,0,0),
			oma=c(3,3,1,1))

		# AGE-0 STOCKING
		sp_freq<-5
			## STOCKING LEVELS SPAWNING EVERY YEAR
		plot_dynamics(
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=0 ,
			age1_st=0,
			lab="a) Age-0 stocking = 0",
			xaxis="n")
		plot_dynamics(
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=10000 ,
			age1_st=0,
			lab="a) Age-0 stocking = 10000",
			xaxis="n")
		plot_dynamics(
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=20000 ,
			age1_st=0,
			lab="a) Age-0 stocking = 20000",
			xaxis="n")		
		plot_dynamics(
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=30000 ,
			age1_st=0,
			lab="a) Age-0 stocking = 30000",
			xaxis="n")			
		plot_dynamics(
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=40000 ,
			age1_st=0,
			lab="a) Age-0 stocking = 40000",
			xaxis="n")			
			mtext(side=1, "Year",line=1.5,outer=TRUE)
			mtext(side=2, "Abundance (x1000)",line=1,outer=TRUE)	
		}

	if(n==6)
		{
		trans_grey<- rgb(120,120,120,alpha=10,maxColorValue=255)
		par(mfrow=c(3,2),mar=c(1,3,0,0),
			oma=c(3,3,1,1))
		# AGE-0 STOCKING
		sp_freq<-5
			## STOCKING LEVELS SPAWNING EVERY YEAR
		plot_dynamics(
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=0,
			age1_st=0,
			lab="a) Age-1 stocking = 0",
			xaxis="n")
		plot_dynamics(
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=0,
			age1_st=2000,
			lab="b) Age-1 stocking = 2000",
			xaxis="n")
		plot_dynamics(
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=0,
			age1_st=4000,
			lab="c) Age-1 stocking = 4000",
			xaxis="n")			
		plot_dynamics(
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=0,
			age1_st=6000,
			lab="d) Age-1 stocking = 6000",
			xaxis="n")				
		plot_dynamics(
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=0,
			age1_st=8000,
			lab="e) Age-1 stocking = 8000",
			xaxis="y")				
		plot_dynamics(
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=0,
			age1_st=10000,
			lab="f) Age-1 stocking = 10000",
			xaxis="y")				
		mtext(side=1, "Year",line=1.5,outer=TRUE)
		mtext(side=2, "Abundance (x1000)",line=1,outer=TRUE)	
		}
		
	if(n==7)
		{# PLOT OF NATURAL ORIGIN DYNAMICS
		trans_grey<- rgb(120,120,120,alpha=10,maxColorValue=255)
		par(mfrow=c(3,2),mar=c(1,3,0,0),
			oma=c(3,3,1,1))

		# AGE-0 STOCKING
		sp_freq<-10
			## STOCKING LEVELS SPAWNING EVERY YEAR
		plot_dynamics(
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=0 ,
			age1_st=0,
			lab="a) Age-0 stocking = 0",
			xaxis="n")
		plot_dynamics(
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=10000 ,
			age1_st=0,
			lab="a) Age-0 stocking = 10000",
			xaxis="n")
		plot_dynamics(
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=20000 ,
			age1_st=0,
			lab="a) Age-0 stocking = 20000",
			xaxis="n")		
		plot_dynamics(
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=30000 ,
			age1_st=0,
			lab="a) Age-0 stocking = 30000",
			xaxis="n")			
		plot_dynamics(
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=40000 ,
			age1_st=0,
			lab="a) Age-0 stocking = 40000",
			xaxis="n")			
			mtext(side=1, "Year",line=1.5,outer=TRUE)
			mtext(side=2, "Abundance (x1000)",line=1,outer=TRUE)	
		}

	if(n==8)
		{
		trans_grey<- rgb(120,120,120,alpha=10,maxColorValue=255)
		par(mfrow=c(3,2),mar=c(1,3,0,0),
			oma=c(3,3,1,1))
		# AGE-0 STOCKING
		sp_freq<-10
			## STOCKING LEVELS SPAWNING EVERY YEAR
		plot_dynamics(
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=0,
			age1_st=0,
			lab="a) Age-1 stocking = 0",
			xaxis="n")
		plot_dynamics(
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=0,
			age1_st=2000,
			lab="b) Age-1 stocking = 2000",
			xaxis="n")
		plot_dynamics(
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=0,
			age1_st=4000,
			lab="c) Age-1 stocking = 4000",
			xaxis="n")			
		plot_dynamics(
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=0,
			age1_st=6000,
			lab="d) Age-1 stocking = 6000",
			xaxis="n")				
		plot_dynamics(
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=0,
			age1_st=8000,
			lab="e) Age-1 stocking = 8000",
			xaxis="y")				
		plot_dynamics(
			org='n' ,
			spawn_freq=sp_freq , 
			age0_st=0,
			age1_st=10000,
			lab="f) Age-1 stocking = 10000",
			xaxis="y")				
		mtext(side=1, "Year",line=1.5,outer=TRUE)
		mtext(side=2, "Abundance (x1000)",line=1,outer=TRUE)	
		}
	
	if(n==9)
		{
		plotDat<- subset(sims,year %in% c(50,100))
		}
	}# end function