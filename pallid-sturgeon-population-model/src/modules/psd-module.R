		# PSD MODULE ####################################################################
		#if(inputs$size_indices==TRUE & dyn$m[i]==6)
		#	{
		#	PSD<- sapply(1:input$nreps,function(x){
		#		out<-c(
		#		length(c(which(dyn$LEN[,x]>=330 & dyn$LEN[,x]<629))),# STOCK
		#		length(c(which(dyn$LEN[,x]>=630 & dyn$LEN[,x]<839))),# QUALITY
		#		length(c(which(dyn$LEN[,x]>=840 & dyn$LEN[,x]<1039))),# PREFERRED
		#		length(c(which(dyn$LEN[,x]>=1040 & dyn$LEN[,x]<1269))),# MEMORABLE
		#		length(c(which(dyn$LEN[,x]>=1270))))# TROPHY
		#		out<-trunc(out/sum(out)*100)
		#	return(out) 
		#	})
		#	sq[i,]<- PSD[1,]
		#	qp[i,]<- PSD[2,]
		#	pm[i,]<- PSD[3,]
		#	mt[i,]<- PSD[4,]
		#	tr[i,]<- PSD[5,]
		#	}
		# END PSD MODULE ################################################################
		
		