


	# VISUALIZING UNCERTAINTY
	mn<-log(-input$S0/(input$S0-1))
	x <- seq(0.000000001,0.2,length=10000)
	x_l<- log(-x/(x-1))
	hx <- dnorm(x_l,mn,abs(input$S0_cv*mn))# uncertainty as CV
	plot(x, hx, type="l")


	
	# VISUALIZING UNCERTAINTY
	mn<-log(-input$S1/(input$S1-1))
	x <- seq(0.000000001,0.999999,length=10000)
	x_l<- log(-x/(x-1))
	hx <- dnorm(x_l,mn,abs(input$S1_cv*mn))# uncertainty as CV
	plot(x, hx, type="l")	
	
	# VISUALIZING UNCERTAINTY
	mn<-log(-input$S2/(input$S2-1))
	x <- seq(0.000000001,0.999999,length=10000)
	x_l<- log(-x/(x-1))
	hx <- dnorm(x_l,mn,abs(input$S2_cv*mn))# uncertainty as CV
	plot(x, hx, type="l")		

	# VISUALIZING UNCERTAINTY
	mn<-log(-input$S3plus/(input$S3plus-1))
	x <- seq(0.000000001,0.999999,length=10000)
	x_l<- log(-x/(x-1))
	hx <- dnorm(x_l,mn,abs(input$S3plus_cv*mn))# uncertainty as CV
	plot(x, hx, type="l")


	# VISUALIZING UNCERTAINTY
	mn<-log(-input$viableGam/(input$viableGam-1))
	x <- seq(0.000000001,0.999999,length=10000)
	x_l<- log(-x/(x-1))
	hx <- dnorm(x_l,mn,abs(input$viableGam_cv*mn))# uncertainty as CV
	plot(x, hx, type="l")
	
	
	# CHANGE VARIABLES FOR ANALYSIS
	scenarios<- expand.grid(spawn_frequency=c(1,2,5,10),
		S0=c(0.0001,0.001,0.001,0.01),
		age0_stock=c(1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,15000,20000))
	yyy<- data.frame()
	
	scenarios<- expand.grid(spawn_frequency=c(1,2,5,10),
		age0_stock=c(1000,2000))
	yyy<- data.frame()	
	
	for(i in 1:nrow(scenarios))
		{
		input$age0_stock<- scenarios$age0_stock[i]
		input$spawn_frequency<-scenarios$spawn_frequency[i]
		input$S0<-scenarios$S0[i]
		output<-xx_ind(input=input)
		output$scenario<-i
		yyy<- rbind.fill(yyy,output)
		}
		
	# RESHAPE OUTPUT FROM WIDE TO LONG
	vv<- names(yyy)[-match(c("origin","sex","year","r","scenario"),names(yyy))]
	tmp<- reshape(yyy,
		varying = vv,
		v.names = "count",
		timevar= "stage",
		times =  vv,
		direction = "long")
	
	# SUMMARY OUTPUT TOTAL POPULATION
	total_pop<-dcast(tmp,year+r+scenario~origin, value.var="count",sum)
	total_pop$total<- total_pop$n+total_pop$h
	save(scenarios, total_pop,file="./output/total_pop.Rdata")	
	
	
# dont delete this	
lambda<-dcast(out,year~rep,value.var="total",mean)
vals<-(lambda[-1,]+1)/(lambda[-nrow(lambda),]+1)
g_mn<- data.frame(scenario="Median stocking", 
	mn_lambda=apply(vals,2,function(x) exp(mean(log(x)))))	
	
	
	
	
	
	
	
	
	
	
	
	