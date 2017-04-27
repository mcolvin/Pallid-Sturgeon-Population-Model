tables<- function(n)
	{
	if(n==1)
		{# PSD TABLE FOR VARYING YEARS
		tmp<-data.frame(year=out$years,
			month=out$month,
			sq=apply(out$stock,1,mean)-
				apply(out$quality,1,mean),
			qp=apply(out$quality,1,mean)-
				apply(out$preferred,1,mean),
			pm=apply(out$preferred,1,mean)-
				apply(out$memorable,1,mean),
			mt=apply(out$memorable,1,mean)-
				apply(out$trophy,1,mean),
			tr=apply(out$trophy,1,mean))
		tmp$sq<- tmp$sq/apply(out$stock,1,mean)*100
		tmp$qp<- tmp$qp/apply(out$stock,1,mean)*100
		tmp$pm<- tmp$pm/apply(out$stock,1,mean)*100
		tmp$mt<- tmp$mt/apply(out$stock,1,mean)*100
		tmp$tr<- tmp$tr/apply(out$stock,1,mean)*100
		
		
		tbl1<-data.frame(PSD=c("PSD-SQ","PSD-QP","PSD-PM","PSD-MT","PSD-T"),
			yr2015=unlist(tmp[floor(tmp$year)==2015 & tmp$month==6,-c(1:2)]),
			yr2025=unlist(tmp[floor(tmp$year)==2025 & tmp$month==6,-c(1:2)]),
			yr2050=unlist(tmp[floor(tmp$year)==2050 & tmp$month==6,-c(1:2)]),
			yrLast=unlist(tmp[tmp$year==max(tmp$year),-c(1:2)]))
		tbl1<-format(tbl1,digits=0)
		return(tbl1)
		}
	if(n==2)
		{# 	QUICK SUMMARY OF INPUTS
		meta<- list(
			"Analysis Metadata",
				paste(c("Analysis ID","GIT Commit"),c(inputs$output_name, inputs$commit),sep=": "),
			
			"Population characteristics",
				paste(c("Total population", "Initial ratio", "Maximum age","Size at hatch (mm)"),
				c(inputs$hatchery+inputs$natural,inputs$sexratio,inputs$maxage,7),sep=": "),	

			"Size and growth",
				list("Length-weight",
						paste(c("a","a' (ln)","b", " $\\sigma$"),
						c(round(inputs$a,8),
						round(inputs$a_prime,2),
						round(inputs$b,2),
						round(inputs$lw_er,2)),sep=": "),

					"Growth",  # be sure to add a space before equations in a list
						paste(c(" $\\mu_{L_{\\infty}}$",
								" $\\mu_{k}$",
								" $ln(\\mu_{L_{\\infty}})$",
								" $ln(\\mu_{k})$",
								" $\\sigma_{ln(\\mu_{L_{\\infty}})}$", 
								" $\\sigma_{ln(\\mu_{k})}$",
								"Correlation  of $ln(\\mu_{L_{\\infty}})$ and $ln(\\mu_{k})$; ($\\rho$)"),
							c(round(exp(inputs$ln_Linf_mu),2), 
								round(exp(inputs$ln_k_mu),2),
								round(inputs$ln_Linf_mu,2), 
								round(inputs$ln_k_mu,2),
								round(sqrt(inputs$vcv[1]),2),
								round(sqrt(inputs$vcv[4]),2),
								round((inputs$vcv[2]/(sqrt(inputs$vcv[1])*sqrt(inputs$vcv[4]))),2)),sep=": ")
					))

		return(meta)
		}
	}
	
