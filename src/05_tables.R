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
	if(n==2)
		{# 	QUICK SUMMARY OF INPUTS
		meta<- list(
			"Analysis Metadata",
				paste(c("Analysis ID","Commit"),c(inputs$output_name, inputs$commit),sep=": "),
			
			"Population characteristics",
				paste(c("Hatchery origin fish", "Natural origin fish", "Initial ratio", "Maximum age","Size at hatch (mm)"),
				c(inputs$hatchery, inputs$natural,inputs$sexratio,inputs$maxAge,7),sep=": ")	
			"Size and growth",
				list("Length-weight",paste(c("a","b"),c(inputs$a,inputs$b),sep=": "),
					"Growth", paste(c("$L_{\infty}","k","Correlation"),c(inputs$Linf, input$k),sep=": ")
					c(),sep=": ")
			
			
		pandoc.list(meta)
		
		growth<-
		surviv<-
		stocking<- 
		
		sim<- data.frame(
			Type="Simulation meta data",
			Description=c(),
			Value=c())
		
		
		}
	}
	
	 l <- list(
        "First list element",
        paste0(1:5, '. subelement'),
        "Second element",
        list('F', 'B', 'I', c('phone', 'pad', 'talics')))
 pandoc.list(l, 'roman')