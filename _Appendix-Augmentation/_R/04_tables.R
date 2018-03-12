tables<- function(n)
	{
	if(n==1){
		# summarize by origin
		out<- sims[,j=list(abundance=sum(abundance)),
			by=list(origin, year,scenario)]
		return(out)
		}
	if(n==2)
		{# NUMBER OF YEARS TO EXTINCTION
		
		# SUM OVER ORIGIN 
		out<- sims[,j=list(abundance=sum(abundance)),
			by=list(year,scenario)]
		out$scenario<- factor(out$scenario)
		
		
		scenarios<- data.frame(scenario=unique(out$scenario))
		tmp<- subset(out, abundance==0)
		tmp<- dcast(out, scenario~year, value.var="year",min,
			drop=FALSE, fill=100,subset=.(abundance==0))
		
		
		tmp<- merge(scenarios, tmp, by="scenario", all.x=TRUE)
		tmp$year<- ifelse(is.na(tmp$year),100,tmp$year)
		
		xx<-sims[,j=list(indx=which(abundance==0)),
			by=list(scenario,origin)]
	
		which.min(sims$abundance
		}
	if(n==3)
		{# CPT formatting and table for 5k
		
		
		
		sims$
		
		}
	
	}