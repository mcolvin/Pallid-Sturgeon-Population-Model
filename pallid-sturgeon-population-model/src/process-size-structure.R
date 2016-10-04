

process_size_structure<- function(year=NULL, L_N=NULL, L_H=NULL)
	{
	if(is.null(L)){return()}
	
	tmp<-rbind(L_N,L_H)
	tmp[]<-0
	
	tmp[L>=1270]<-1
	T<- colSums(tmp)
	tmp[L>=1040]<-1
	M<- colSums(tmp)
	tmp[L>=840]<-1
	P<- colSums(tmp)	
	tmp[L>=630]<-1
	Q<- colSums(tmp)	
	tmp[L>=330]<-1
	S<- colSums(tmp)		
	return(data.frame(year=rep(year,ncol(tmp)),S=S,P=P,Q=Q,M=M,T=T))
	}

