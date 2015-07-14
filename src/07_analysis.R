input<- list(
	# POPULATION DEMOGRAPHIC RATES
	maxAge=41, 			# [0,90] Maximum age
	S0=0.051, 			# [0,0.3] Survival age-0 
	S1=0.6, 			# [0.2,1] Survival age-1
	S2=0.8, 			# [0.8,1] Survival age-2               
	S3plus=0.92, 		# [0.8, 1] Survival age-3+  
	viableGam = 0.00000001,	# PROBABILITY OF PRODUCING VIABLE GAMETES

	# MATURITY FUNCTION
	aa=5, 				# [0,30] Minimum age of sexual maturity
	bb=6, 				# [0,30] Age at 25% maturity           
	cc=7, 				# [0,30] Age at 50% maturity
	dd=8,				# [0,30] Age at 75% maturity"
	ee=9,				# [0,30] maximum juvenile age"
  
	# GROWTH
	Linf = 1683,
	K =0.036,
	t0=-5.9,
  
	# INITIAL STATE VALUES
	sr_n=0.33,			# [0,1] Sex ratio natural origin fish 
	sr_h=0.5, 			# [0,1] Sex ratio hatchery origin fish 
	juv_ini_n=50,		# [0,100] Natural origin Juvenile (age 1-age -2) fish 
	juv_ini_h=50,		# [0,100] Hatchery origin Juvenile (age 1-age -2) fish 
	adults_ini_n=50,	# [0,100] Natural origin adult  (>age-2) fish
	adults_ini_h=50,	# [0,100] Hatchery origin Juvenile (>age-2) fish

	# FECUNDITY
	a_fec= –43 678, 	# from S1001  
	b_fec=72.70, 		# from S1001 

	# VIABILITY ANALYSIS INPUTS
	fe_stock=0, 		# [0,1000000] Free embryos stocked
	efl_stock=0, 		# [0,1000000] Exo. feeding larvae stocked
	juv_stock=0, 		# [0,100000]
	nreps=5,			# [1,100] Number of replicates"
	nyears=50) 			# [20,100] Years to simulate
	
	
	out<- xx(input=input) 	
maturity		
		
		

	
	



	
# [1] LOAD VALUES

## [1.1] GENERAL MODEL VALUES
nyears=100
mdn<-c(2895,0)# age-0 and age-1
sims<- simulate(nyears=100,nreps=500,stocked=mdn, sensi="uncertainty") # evaluate uncertainty
out<- sims$outp
out$total<- out$jn+out$jh+out$an+out$ah
figures(1)
savePlot("./median_total_population_dynamics.wmf",type="wmf")
savePlot("./median_total_population_dynamics.jpg",type="jpg")




# WHAT LEVEL OF S0 IS NEEDED TO GET A LAMBDA > 1

combos<- expand.grid(S_egg_embryo=seq(0.00001,0.0001, 0.00001), S0=seq(0,0.3,0.01))
combos$prop_lam1<- 0
k=nrow(combos)
for (k in 1:nrow(combos))
	{
	sims<- simulate(nyears=100,
		nreps=100,
		stocked = c(0,0),
		# ABUDNANCE
		N_juv_Natural=c(0,500,1000),
		N_adult_Natural=c(3750,4000,4250),
		N_juv_Hatchery=c(18000,21500,25000),
		N_adult_Hatchery=c(12500,14500,16750),
		# RATES
		sex_ratio=c(0.33*0.8,0.33,0.33*1.2),
		S0=rep(combos[k,2],3),
		S_egg_embryo=rep(combos[k,1],3),
		S1=c(0.60,0.686,0.75),
		S2=c(0.9,0.922,0.95),
		stoch_in=FALSE)
	out<- sims$outp
	sims$parms
	out$total<- out$jn+out$jh+out$an+out$ah
	figures(1)
	lambda<-dcast(out,year~rep,value.var="total",mean)[,-1]
	vals<-(lambda[-1,]+1)/(lambda[-nrow(lambda),]+1)
	# CALCUALTE STOCHASTIC LAMBDA
	mn_lambda=apply(vals,2,function(x) exp(mean(log(x))))

	S0_lambda<- sims$parms[sims$parms$parm =="S: age-0 to age-1",]
	S0_lambda<- S0_lambda[order(S0_lambda$rep),]
	S0_lambda$lambda<- mn_lambda
	S0_lambda$bin<- ifelse(S0_lambda$lambda>=1,1,0)

	combos[k,3]<- mean(S0_lambda$bin) # proportion of reps with lambda >=1
	print(k)
	}
	
write.csv(combos,"./output/lower_lambda1.csv")









levelplot(prop_lam1~ S_egg_embryo*S0,combos)


plot(lambda~vals,S0_lambda,subset=lambda<1,ylim=c(0.9,1.2))
points(lambda~vals,S0_lambda,subset=lambda>=1,pch=19,col="red")
# 

figures(2)
savePlot("./median_lambda_dynamics.wmf",type="wmf")
savePlot("./median_lambda_dynamics.jpg",type="jpg")

g_mn<- data.frame(scenario="Median stocking", 
	mn_lambda=apply(vals,2,function(x) exp(mean(log(x)))))

	
	
	
	
	
	
	
	
	
	
	
	
	
	
# NO STOCKING OF AGE-0 OR AGE-1	
none<-c(0,0)# age-0 and age-1
sims<- simulate(nyears=100,nreps=500,stocked=none, sensi="uncertainty") # evaluate uncertainty
out<- sims$outp
out$total<- out$jn+out$jh+out$an+out$ah
figures(3)
savePlot("./zero_total_population_dynamics.wmf",type="wmf")
savePlot("./zero_total_population_dynamics.jpg",type="jpg")
lambda<-dcast(out,year~rep,value.var="total",mean)
vals<-(lambda[-1,]+1)/(lambda[-nrow(lambda),]+1)
figures(4)
savePlot("./zero_lambda_dynamics.wmf",type="wmf")
savePlot("./zero_lambda_dynamics.jpg",type="jpg")
g_mn<- rbind(g_mn,data.frame(scenario="No stocking", 
	mn_lambda=apply(vals,2,function(x) exp(mean(log(x))))))
figures(5)
savePlot("./lambda_comparisons.wmf",type="wmf")
savePlot("./lambda_comparisons.jpg",type="jpg")







parms<- sims$parms

hist(out$an)

parms
bins_vals<- dcast(parms,rep~parm, value.var="vals",median)
bins<- bins_vals
for(i in 2:ncol(bins))
	{
	x<-seq(min(bins_vals[,i]),max(bins_vals[,i]),length.out=4)
	y<- c(1,2,3,3)
	bin_fun<- approxfun(x,y,method="constant",rule=2,ties=max)
	bins[,i]<- bin_fun(bins_vals[,i])
	}
	
# RESHAPE TO LONG
bins<- reshape(bins,
	varying = names(bins) [2:ncol(bins)],
	v.names = "bin",
	timevar= "parm",
	times = names(bins) [2:ncol(bins)],
	direction = "long")
	
# SIMULATION SUMMARY
out_plot<- out
out<- subset(out, year==100)
sensi<- merge(bins,out,by="rep")
sensi<- sensi[,-c(4,9)]
sensi<- reshape(sensi,
	varying = names(sensi) [4:ncol(sensi)],
	v.names = "pop",
	timevar= "stage",
	times = names(sensi) [4:ncol(sensi)],
	direction = "long")
xx<- dcast(sensi, bin+stage~parm, value.var="pop",mean,na.rm=TRUE)

dev.new(height= 6, width = 9.75)

# TORNADO PLOT
mns<-apply(xx[xx$stage=="ah",-c(1:2)],2,min,na.rm=TRUE)
mxs<-apply(xx[xx$stage=="ah",-c(1:2)],2,max,na.rm=TRUE)
tornado<- data.frame(parms=names(xx)[-c(1,2)],mns=mns,mxs=mxs)
tornado$rnge<- abs(tornado$mns-tornado$mxs)
tornado<- tornado[order(tornado$rng,decreasing=FALSE),]
tornado$y<- c(1:nrow(tornado))
figures(6)

# TORNADO PLOT: 
mns<-apply(xx[xx$stage=="an",-c(1:2)],2,min,na.rm=TRUE)
mxs<-apply(xx[xx$stage=="an",-c(1:2)],2,max,na.rm=TRUE)
tornado<- data.frame(parms=names(xx)[-c(1,2)],mns=mns,mxs=mxs)
tornado$rnge<- abs(tornado$mns-tornado$mxs)
tornado<- tornado[order(tornado$rng,decreasing=FALSE),]
tornado$y<- c(1:nrow(tornado))
figures(7)
# TORNADO PLOT
mns<-apply(xx[xx$stage=="jh",-c(1:2)],2,min,na.rm=TRUE)
mxs<-apply(xx[xx$stage=="jh",-c(1:2)],2,max,na.rm=TRUE)
tornado<- data.frame(parms=names(xx)[-c(1,2)],mns=mns,mxs=mxs)
tornado$rnge<- abs(tornado$mns-tornado$mxs)
tornado<- tornado[order(tornado$rng,decreasing=FALSE),]
tornado$y<- c(1:nrow(tornado))
figures(8)
# TORNADO PLOT: 
mns<-apply(xx[xx$stage=="jn",-c(1:2)],2,min,na.rm=TRUE)
mxs<-apply(xx[xx$stage=="jn",-c(1:2)],2,max,na.rm=TRUE)
tornado<- data.frame(parms=names(xx)[-c(1,2)],mns=mns,mxs=mxs)
tornado$rnge<- abs(tornado$mns-tornado$mxs)
tornado<- tornado[order(tornado$rng,decreasing=FALSE),]
tornado$y<- c(1:nrow(tornado))
figures(9)
# ALL PARAMETERS: TOTAL POPULATION ABUNDANCE
xx<- dcast(sensi, bin~parm, value.var="pop",mean,na.rm=TRUE)
# TORNADO PLOT: 
mns<-apply(xx[,-c(1)],2,min,na.rm=TRUE)
mxs<-apply(xx[,-c(1)],2,max,na.rm=TRUE)
tornado<- data.frame(parms=names(xx)[-c(1)],mns=mns,mxs=mxs)
tornado$rnge<- abs(tornado$mns-tornado$mxs)
tornado<- tornado[order(tornado$rng,decreasing=FALSE),]
tornado$y<- c(1:nrow(tornado))
par(mar=c(4,15,1,1))
plot(c(5000,40000),c(1,14),type="n",las=1,xlab="Expected population size",
	yaxt="n",ylab="")
axis(2, at=c(1:14),labels=tornado$parms,las=1)
segments(tornado$mns,tornado$y,tornado$mxs,tornado$y,lwd=4)

dev.new(height= 6, width = 9.75)
xyplot(an~year,out_plot, group=rep,type='l',ylab="Abundance",xlab="Year",cex.lab=1.5,cex.axis=1.5)
xyplot(ah~year,out_plot, group=rep,type='l',ylab="Abundance",xlab="Year",cex.lab=1.5,cex.axis=1.5)
	
xyplot(jn~year,out_plot, group=rep,type='l',ylab="Abundance",xlab="Year",cex.lab=1.5,cex.axis=1.5)
xyplot(jh~year,out_plot, group=rep,type='l',ylab="Abundance",xlab="Year",cex.lab=1.5,cex.axis=1.5)	


dev.new(height= 6, width = 9.75)
par(mar=c(3,3,0,0),oma=c(1,1.25,1,1),mfrow=c(2,2),cex.axis=1.3)
nreps=1
plot(jn/1000~year,out_plot, type='n',las=1,ylab="",xlab="",ylim=c(0,9))	
sapply(c(1:nreps), function(x){points(jn/1000~year,out_plot[out_plot$rep==x,],type='l',col=x,lwd=3)})
plot(an/1000~year,out_plot, type='n',las=1,ylab="",xlab="",ylim=c(0,9))	
sapply(c(1:nreps), function(x){points(an/1000~year,out_plot[out_plot$rep==x,],type='l',col=x,lwd=3)})
plot(ah/1000~year,out_plot, type='n',las=1,ylab="",xlab="",ylim=c(0,180))	
sapply(c(1:nreps), function(x){points(ah/1000~year,out_plot[out_plot$rep==x,],type='l',col=x,lwd=3)})
plot(jh/1000~year,out_plot, type='n',las=1,ylab="",xlab="",ylim=c(0,180))	
sapply(c(1:nreps), function(x){points(jh/1000~year,out_plot[out_plot$rep==x,],type='l',col=x,lwd=3)})

mtext(side=1, "Year", outer=TRUE, line=-0.75,cex=1.5)
mtext(side=2, "Abundance", outer=TRUE, line=0,cex=1.5)


# EVALUATE PARAMETER ESTIMATE SENSTIVITY
sims<- simulate(nyears=100,nreps=1000,sensi="parm") # evaluate uncertainty
out<- sims$outp
parms<- sims$parms
bins_vals<- dcast(parms,rep~parm, value.var="vals",median)
bins<- bins_vals
for(i in 2:ncol(bins))
	{
	x<-seq(min(bins_vals[,i]),max(bins_vals[,i]),length.out=4)
	y<- c(1,2,3,3)
	bin_fun<- approxfun(x,y,method="constant",rule=2,ties=max)
	bins[,i]<- bin_fun(bins_vals[,i])
	}
	x<- min(bins_vals[,i]):max(bins_vals[,i])
	plot(x,bin_fun(x))
# RESHAPE TO LONG
bins<- reshape(bins,
	varying = names(bins) [2:ncol(bins)],
	v.names = "bin",
	timevar= "parm",
	times = names(bins) [2:ncol(bins)],
	direction = "long")
	
# SIMULATION SUMMARY
out_plot<- out
out<- subset(out, year==100)
sensi<- merge(bins,out,by="rep")
sensi<- sensi[,-c(4,9)]
sensi<- reshape(sensi,
	varying = names(sensi) [4:ncol(sensi)],
	v.names = "pop",
	timevar= "stage",
	times = names(sensi) [4:ncol(sensi)],
	direction = "long")
xx<- dcast(sensi, bin+stage~parm, value.var="pop",mean,na.rm=TRUE)


dev.new(height= 6, width = 9.75)

# TORNADO PLOT: HATCHERY ORIGIN ADULTS
mns<-apply(xx[xx$stage=="ah",-c(1:2)],2,min,na.rm=TRUE)
mxs<-apply(xx[xx$stage=="ah",-c(1:2)],2,max,na.rm=TRUE)
tornado<- data.frame(parms=names(xx)[-c(1,2)],mns=mns,mxs=mxs)
tornado$rnge<- abs(tornado$mns-tornado$mxs)
tornado<- tornado[order(tornado$rng,decreasing=FALSE),]
tornado$y<- c(1:nrow(tornado))
par(mar=c(4,15,1,1))
plot(c(2000,16000),c(1,14),type="n",las=1,xlab="Expected population size",
	yaxt="n",ylab="")
axis(2, at=c(1:14),labels=tornado$parms,las=1)
segments(tornado$mns,tornado$y,tornado$mxs,tornado$y,lwd=4)

# TORNADO PLOT: NATURAL ORIGIN ADULTS
mns<-apply(xx[xx$stage=="an",-c(1:2)],2,min,na.rm=TRUE)
mxs<-apply(xx[xx$stage=="an",-c(1:2)],2,max,na.rm=TRUE)
tornado<- data.frame(parms=names(xx)[-c(1,2)],mns=mns,mxs=mxs)
tornado$rnge<- abs(tornado$mns-tornado$mxs)
tornado<- tornado[order(tornado$rng,decreasing=FALSE),]
tornado$y<- c(1:nrow(tornado))
par(mar=c(4,15,1,1))
plot(c(00,400),c(1,14),type="n",las=1,xlab="Expected population size",
	yaxt="n",ylab="")
axis(2, at=c(1:14),labels=tornado$parms,las=1)
segments(tornado$mns,tornado$y,tornado$mxs,tornado$y,lwd=4)

# TORNADO PLOT: HATCHERY ORIGIN JUVENILES
mns<-apply(xx[xx$stage=="jh",-c(1:2)],2,min,na.rm=TRUE)
mxs<-apply(xx[xx$stage=="jh",-c(1:2)],2,max,na.rm=TRUE)
tornado<- data.frame(parms=names(xx)[-c(1,2)],mns=mns,mxs=mxs)
tornado$rnge<- abs(tornado$mns-tornado$mxs)
tornado<- tornado[order(tornado$rng,decreasing=FALSE),]
tornado$y<- c(1:nrow(tornado))
par(mar=c(4,15,1,1))
plot(c(0,12000),c(1,14),type="n",las=1,xlab="Expected population size",
	yaxt="n",ylab="")
axis(2, at=c(1:14),labels=tornado$parms,las=1)
segments(tornado$mns,tornado$y,tornado$mxs,tornado$y,lwd=4)

# TORNADO PLOT: NATURAL ORIGIN JUVENILES
mns<-apply(xx[xx$stage=="jn",-c(1:2)],2,min,na.rm=TRUE)
mxs<-apply(xx[xx$stage=="jn",-c(1:2)],2,max,na.rm=TRUE)
tornado<- data.frame(parms=names(xx)[-c(1,2)],mns=mns,mxs=mxs)
tornado$rnge<- abs(tornado$mns-tornado$mxs)
tornado<- tornado[order(tornado$rng,decreasing=FALSE),]
tornado$y<- c(1:nrow(tornado))
par(mar=c(4,15,1,1))
plot(c(50,270),c(1,14),type="n",las=1,xlab="Expected population size",
	yaxt="n",ylab="")
axis(2, at=c(1:14),labels=tornado$parms,las=1)
segments(tornado$mns,tornado$y,tornado$mxs,tornado$y,lwd=4)


# ALL PARAMETERS: TOTAL POPULATION ABUNDANCE
out_all<- out
out_all$n<- out_all$jn+ out_all$jh+out_all$an+out_all$ah
sensi<- merge(bins,out_all,by=c("rep"))
sensi<- sensi[,c("rep","parm","bin","n")]
xx<- dcast(sensi, bin~parm, value.var="n",mean,na.rm=TRUE)


# TORNADO PLOT: 
mns<-apply(xx[,-c(1)],2,min,na.rm=TRUE)
mxs<-apply(xx[,-c(1)],2,max,na.rm=TRUE)
tornado<- data.frame(parms=names(xx)[-c(1)],mns=mns,mxs=mxs)
tornado$rnge<- abs(tornado$mns-tornado$mxs)
tornado<- tornado[order(tornado$rng,decreasing=FALSE),]
tornado$y<- c(1:nrow(tornado))
par(mar=c(4,15,1,1))
plot(c(0,28000),c(1,14),type="n",las=1,xlab="Expected population size",
	yaxt="n",ylab="")
axis(2, at=c(1:14),labels=tornado$parms,las=1)
segments(tornado$mns,tornado$y,tornado$mxs,tornado$y,lwd=4)



# PLOT ALLEE EFFECT [NOT IMPLEEMENTED BUT COULD BE
max_p_gamete<- 0.000019
pop_fifty<- 12000
pop<- c(1:35000)
p<-  max_p_gamete/(1+exp(-0.0004*(pop - 11000)))

dev.new(height= 6, width = 9.75)
par(mar=c(4,6,1,1))
plot(pop,p,las=1,xlab="Population size", ylab="",ylim=c(0, 0.000022),cex.lab=1.5, cex.lab=1.3)
abline(h=max_p_gamete,lty=2)
mtext(side=2, "Probability of fertilization",outer=TRUE,line= -1.5,cex=1.3)
text(1000,max_p_gamete,"Maximum",pos=3)



		
	plot_tri<- function(mn,mx,md)
		{
		vals<- seq(mn,mx,length.out=5000)
		x<-dtriangle(vals, a=mn,b=mx,c=md)
		plot(vals,x,type='n',las=1,ylab="",xlab="",cex.axis=1.3, cex.lab=1.5)
		#mtext(side=2, "Density", outer=TRUE, line=-.8,cex=1.5)
		polygon(c(vals,vals[1]),c(x,0),col='lightgrey',lwd=3)
		box()
		}

			par(mar=c(2,6,0,0),oma=c(2,1,1,1))
			plot_tri(0.000019*0.8,0.000019*1.2,0.000019)
			
			
			
			x<-rtriangle(1000000, a=0.000019*0.8,b=0.000019*1.2,c=0.000019)
			hist(x,prob=TRUE)
			