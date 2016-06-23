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
	
