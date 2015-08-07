
# CHANGE VARIABLES FOR ANALYSIS
	
	fn<-dir("./output")
	for(j in (length(fn)+1):(length(fn)+10))
		{
		input$age0_stock<- round(runif(1,0,40000))# range of age-0 is 0 to 40k from Steffansons model
		input$age1_stock<- 0 #round(runif(1,0,14000))# range of age-0 is 0 to 40k from Steffansons model
		input$spawn_frequency<-sample(c(1,2,5,10,20),1,replace=TRUE)
		output<-xx_ind(input=input)
		save(input,output,file=
			paste("./output/output_",j,".csv",sep=""))
		}
	
	
	
	
	
	input$nyears<-20
	Rprof("out.out")
	for (i in 1:3) pos = xx_ind(input)
	Rprof(NULL)
	proftable("out.out")
	summaryRprof("out.out")

	ptm <- proc.time()
	output<-xx_ind(input=input)
	proc.time() - ptm

	
	
	# SUMMARY OUTPUT TOTAL POPULATION
	total_pop<-dcast(tmp,year+r+scenario~origin, value.var="count",sum)
	total_pop$total<- total_pop$n+total_pop$h
	save(scenarios, total_pop,input, file="./output/sim_output.Rdata")	
	
	
# dont delete this	
lambda<-dcast(out,year~rep,value.var="total",mean)
vals<-(lambda[-1,]+1)/(lambda[-nrow(lambda),]+1)
g_mn<- data.frame(scenario="Median stocking", 
	mn_lambda=apply(vals,2,function(x) exp(mean(log(x)))))	
	
	





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
	
	
	
	
	
	
	
	
	
	
	
	
	