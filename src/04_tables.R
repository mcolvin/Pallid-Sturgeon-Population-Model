tables<- function(n)
	{
	if(n=="tbl1")
		{
		#output$tbl1<- renderTable({
		out<- as.data.frame(xx()$a_n_m)
		return(out)		
		}
		#})
	if(n==1)
		{
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
			
		# RESHAPE TO LONG
		bins<- reshape(bins,
			varying = names(bins) [2:ncol(bins)],
			v.names = "bin",
			timevar= "parm",
			times = names(bins) [2:ncol(bins)],
			direction = "long")
			
# SIMULATION SUMMARY
out_plot<- out
out<- subset(out, year==50)
sensi<- merge(bins,out,by="rep")
sensi<- sensi[,-c(4,9)]
sensi<- reshape(sensi,
	varying = names(sensi) [4:ncol(sensi)],
	v.names = "pop",
	timevar= "stage",
	times = names(sensi) [4:ncol(sensi)],
	direction = "long")
xx<- dcast(sensi, bin+stage~parm, value.var="pop",mean,na.rm=TRUE)
mns<-apply(xx[xx$stage==stage,-c(1:2)],2,min,na.rm=TRUE)
mxs<-apply(xx[xx$stage==stage,-c(1:2)],2,max,na.rm=TRUE)
tornado<- data.frame(parms=names(xx)[-c(1,2)],mns=mns,mxs=mxs)
tornado$rnge<- abs(tornado$mns-tornado$mxs)
tornado<- tornado[order(tornado$rng,decreasing=FALSE),]
tornado$y<- c(1:nrow(tornado))
		return(tornado)
		}
	}