tables<- function(n)
	{
	if(n==1)
		{# PSD TABLE FOR VARYING YEARS
		indx<-which(rowSums(out$qp)>1)
		tmp<-data.frame(year=c(2015+0:49),
			sq=apply(out$sq[indx,],1,mean),
			qp=apply(out$qp[indx,],1,mean),
			pm=apply(out$pm[indx,],1,mean),
			mt=apply(out$mt[indx,],1,mean),
			tr=apply(out$tr[indx,],1,mean))
		tbl1<-data.frame(PSD=c("PSD-SQ","PSD-QP","PSD-PM","PSD-MT","PSD-T"),
			yr2015=unlist(tmp[tmp$year==2015,-1]),
			yr2025=unlist(tmp[tmp$year==2025,-1]),
			yr2050=unlist(tmp[tmp$year==2050,-1]),
			yrLast=unlist(tmp[tmp$year==max(tmp$year),-1]))
		}
	}