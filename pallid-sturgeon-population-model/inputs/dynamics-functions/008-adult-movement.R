adultMovement<- function(previousLocation,fromToMatrix)
	{
	tmp<- data.table(prevLocation=previousLocation,ord=c(1:length(previousLocation)))
	tmp<- tmp[order(prevLocation)]
	nLocToFind<-as.data.frame(tmp[,list(Freq=.N),by=list(prevLocation=prevLocation)])

	newLocations<-unlist(lapply(1:nrow(nLocToFind),function(x)
		{
		sample(c(1:ncol(fromToMatrix)),nLocToFind[x,2],prob=fromToMatrix[nLocToFind[x,1],],replace=TRUE)
		}))
		
		
	tmp[,newLocation:= unlist(lapply(1:nrow(nLocToFind),function(x)
		{
		sample(c(1:ncol(fromToMatrix)),nLocToFind[x,2],prob=fromToMatrix[nLocToFind[x,1],],replace=TRUE)
		}))]
	tmp<- tmp[order(ord)]
	return(tmp$newLocation)	
	}

	
	
	
	