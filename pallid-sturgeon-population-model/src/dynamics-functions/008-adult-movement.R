adultMovement<- function(previousLocation,fromToMatrix)
	{
	out<-lapply(unique(previousLocation) ,function(x)
		{
		indx<- which(previousLocation==x)
		new<-sample(1:ncol(fromToMatrix),
				length(indx),
				prob=fromToMatrix[x,],
				replace=TRUE)
		return(cbind(indx,new))
		})
	out<-do.call("rbind",out)# convert list to dataframe
	out<- out[order(out[,1]),]
	return(out[,2])
	}