tables<- function(n){

if(n=="tbl1")
	{# TABLE OF A AND B VALUES FOR UPPER AND LOWER BASIN
	options(scipen=15)
	fit_upper<-	lm(log(weight)~log(length),dat,subset=(basin=="upper" & length>0),na.action=na.omit)
	fit_lower<-	lm(log(weight)~log(length),dat,subset=(basin=="lower" & length>0),na.action=na.omit)
	out<- data.frame(Basin=c("Upper","Lower"), 
		l= c(paste(round(mean(na.omit(dat[dat$basin=="upper",]$length)),1),"(",min(na.omit(dat[dat$basin=="upper",]$length)),"-",max(na.omit(dat[dat$basin=="upper",]$length)),")",sep=""),
			paste(round(mean(na.omit(dat[dat$basin=="lower",]$length)),1),"(",min(na.omit(dat[dat$basin=="lower",]$length)),"-",max(na.omit(dat[dat$basin=="lower",]$length)),")",sep="")),
		w= c(paste(round(mean(na.omit(dat[dat$basin=="upper",]$weight)),1),"(",min(na.omit(dat[dat$basin=="upper",]$weight)),"-",max(na.omit(dat[dat$basin=="upper",]$weight)),")",sep=""),
			paste(round(mean(na.omit(dat[dat$basin=="lower",]$weight)),1),"(",min(na.omit(dat[dat$basin=="lower",]$weight)),"-",max(na.omit(dat[dat$basin=="lower",]$weight)),")",sep="")),
		a= c(paste(round((coef(fit_upper)[1]),2),"(", paste(round(unlist((confint(fit_upper) [1,])),2),collapse=",") ,")",sep=""),
			paste(round((coef(fit_lower))[1],2),"(", paste(round(unlist((confint(fit_lower) [1,])),2),collapse=",") ,")",sep="")),
		b= c(paste(round(coef(fit_upper)[2],3),"(", paste(round(unlist(confint(fit_upper) [2,]),3),collapse=",") ,")",sep=""),
			paste(round(coef(fit_lower)[2],3),"(", paste(round(unlist(confint(fit_lower) [2,]),3),collapse=",") ,")",sep="")),
		er= c(round(summary(fit_upper)$sigma,4),round(summary(fit_lower)$sigma,4)))
	return(out)
	
	aggregate(year~basin, dat[!(is.na(dat$weight)),], max	)
	
	}
if(n=="tbl2")
	{# MODEL SELECTION FOR GROWTH MODEL
	tmp<-data.frame(basin=c(rep("Lower",4),rep("Upper",4)),
		Model=rep(c("VB","GM"),4),
		modelType=rep(c(1,1,2,2),2),
		 DIC=c(m1$BUGSoutput$DIC,
			m2$BUGSoutput$DIC,
			m3$BUGSoutput$DIC,
			m4$BUGSoutput$DIC,
			m5$BUGSoutput$DIC,
			m6$BUGSoutput$DIC,
			m7$BUGSoutput$DIC,
			m8$BUGSoutput$DIC))
	tmp$dDIC<-0
	tmp[tmp$basin=="Lower",]$dDIC<- tmp[tmp$basin=="Lower",]$DIC-min(tmp[tmp$basin=="Lower",]$DIC)
	tmp[tmp$basin=="Upper",]$dDIC<- tmp[tmp$basin=="Upper",]$DIC-min(tmp[tmp$basin=="Upper",]$DIC)
	tmp$lik<- exp(-0.5*tmp$dDIC)
	tmp$w<- 0
	tmp[tmp$basin=="Lower",]$w<- tmp[tmp$basin=="Lower",]$lik/sum(tmp[tmp$basin=="Lower",]$lik)
	tmp[tmp$basin=="Upper",]$w<- tmp[tmp$basin=="Upper",]$lik/sum(tmp[tmp$basin=="Upper",]$lik)
	return(tmp)
	}
if(n=="tbl3")
	{
	tmp<-data.frame(Linf=c(m1$BUGSoutput$mean$Linf,m2$BUGSoutput$mean$Linf,m3$BUGSoutput$mean$Linf,m4$BUGSoutput$mean$Linf,
		m5$BUGSoutput$mean$Linf,m6$BUGSoutput$mean$Linf,m7$BUGSoutput$mean$Linf,m8$BUGSoutput$mean$Linf),
	k=c(m1$BUGSoutput$mean$k,m2$BUGSoutput$mean$k,m3$BUGSoutput$mean$k,m4$BUGSoutput$mean$k,
		m5$BUGSoutput$mean$k,m6$BUGSoutput$mean$k,m7$BUGSoutput$mean$k,m8$BUGSoutput$mean$k),
	sigma_linf=c(m1$BUGSoutput$mean$sigma_Linf,m2$BUGSoutput$mean$sigma_Linf,m3$BUGSoutput$mean$sigma_Linf,m4$BUGSoutput$mean$sigma_Linf,
		m5$BUGSoutput$mean$sigma_Linf,m6$BUGSoutput$mean$sigma_Linf,m7$BUGSoutput$mean$sigma_Linf,m8$BUGSoutput$mean$sigma_Linf),	
	sigma_k=c(NA,NA,m3$BUGSoutput$mean$sigma_k,m4$BUGSoutput$mean$sigma_k,
		NA,NA,m7$BUGSoutput$mean$sigma_k,m8$BUGSoutput$mean$sigma_k),	
	sigma_obs=c(m1$BUGSoutput$mean$sigma_obs,m2$BUGSoutput$mean$sigma_obs,m3$BUGSoutput$mean$sigma_obs,m4$BUGSoutput$mean$sigma_obs,
		m5$BUGSoutput$mean$sigma_obs,m6$BUGSoutput$mean$sigma_obs,m7$BUGSoutput$mean$sigma_obs,m8$BUGSoutput$mean$sigma_obs))
	return(tmp)
	}
if(n==2)
	{# DATASET OF FISH THAT WERE RECAPTURED
	# LENGTH AND DAYS POST TAGGING
	
	# FISH WITH MORE THAN 1 RECOVERIES
	tmp<- aggregate(tmp~tagnumber,dat, 
		subset=!is.na(tagnumber),sum)
	tmp<- subset(tmp, tmp>1 & !(tagnumber %in% 
		toupper(c("N/A", "nofishscan","xxxxxxxxxx", "XXXXXXXXXX"))))
	xx<-as.character(tmp$tagnumber)
	out<- subset(dat,tagnumber %in% xx)
	tagDat<- aggregate(setdate~tagnumber,out,min)
	names(tagDat)[2]<-"tagdate"
	tagDat$tagLength<-NA
	tagDat$tagWeight<-NA
	for(i in 1:nrow(tagDat))
		{
		indx<- which(dat$tagnumber %in% tagDat$tagnumber[i] & dat$setdate==tagDat$tagdate[i])
		indx<- indx[which.min(dat[indx,]$mr_id)]   # get first one entered
		tagDat$tagLength[i]<- dat[indx ,]$length	
		tagDat$tagWeight[i]<- dat[indx,]$weight
		}
	out<- merge(out,tagDat,by="tagnumber")
	out$dpt<- out$setdate-out$tagdate
	# convert seconds to days
	out$dpt<-as.numeric(out$dpt/86400)
	
	# dL
	out$dl<- out$length-out$tagLength
	out$dw<- out$weight-out$tagWeight
	
	# SUBSET OUT INSTANCES OF MULTIPLE CATCHES IN THE SAME DAY
	return(out)
	}
if(n==3)
	{# DATASET FOR PLOTTING LENGTH AND WEIGHT
	# LENGTH AND DAYS POST TAGGING
	# FISH WITH MORE THAN 1 RECOVERIES
	tmp<- aggregate(tmp~tagnumber,dat, 
		subset=!is.na(tagnumber),sum)
	tmp<- subset(tmp, tmp>1 & !(tagnumber %in% 
		toupper(c("N/A", "nofishscan","xxxxxxxxxx", "XXXXXXXXXX"))))
	xx<-as.character(tmp$tagnumber)
	out<- subset(dat,tagnumber %in% xx)
	tagDat<- aggregate(setdate~tagnumber,out,min)
	names(tagDat)[2]<-"tagdate"
	tagDat$tagLength<-NA
	tagDat$tagWeight<-NA
	for(i in 1:nrow(tagDat))
		{
		indx<- which(dat$tagnumber%in% tagDat$tagnumber[i] & dat$setdate==tagDat$tagdate[i])
		indx<- indx[which.min(dat[indx,]$mr_id)]# get first one entered
		tagDat$tagLength[i]<- dat[indx ,]$length	
		tagDat$tagWeight[i]<- dat[indx,]$weight
		}
	out<- merge(out,tagDat,by="tagnumber")
	out$dpt<- out$setdate-out$tagdate
	# convert seconds to days
	out$dpt<-as.numeric(out$dpt/86400)
	
	# dL
	out$dl<- out$length-out$tagLength
	out$dw<- out$weight-out$tagWeight
	
	out<- out[order(out$tagnumber,out$setdate),]
	out$dl_prop<-1
	out$dw_prop<-1
	out$dl<-out$dt<-0
	for(i in 2:nrow(out))
		{
		out$dt[i]<- ifelse(out$tagnumber[i]==out$tagnumber[i-1],out$setdate[i]-out$setdate[i-1],0)
		out$dl[i]<- ifelse(out$tagnumber[i]==out$tagnumber[i-1],out$length[i]-out$length[i-1],0)
		out$dl_prop[i]<- ifelse(out$tagnumber[i]==out$tagnumber[i-1],out$length[i]/out$length[i-1],1)
		out$dw_prop[i]<- ifelse(out$tagnumber[i]==out$tagnumber[i-1],out$weight[i]/out$weight[i-1],1)	
		
		}
	out<- subset(out, dt < 30)
	# MORE THEN 1 TAG
	tags<-aggregate(dl~tagnumber,out,length)
	tags<- subset(tags, dl>1)
	out<- subset(out, tagnumber %in% tags$tagnumber)
	return(out)
	}
	
if(n==4)
	{
	# DATASET FOR FABENS ANALYSIS
	tags<-aggregate(tmp~tagnumber,dat,sum)
    
    ## GET RID OF UNUSABLE DATA
	tags<- subset(tags, tmp>1)
	tags<- subset(tags,  !(tagnumber %in% toupper(c("N/A", "nofishscan","xxxxxxxxxx", "XXXXXXXXXX","0000000000"))))
	tags<- subset(tags,  !is.na(tagnumber))
	tmp<- subset(dat, tagnumber %in% tags$tagnumber)
	# SUBSET OUT TRAINING DATA
    tmp<- subset(dat, validate==0)
    ## ORDER BY TAG AND DATE
    tmp<- tmp[order(tmp$tagnumber,tmp$setdate),]
    ## CREATE A SEQUENTIAL INDEX FOR EACH FISH
    ## TO LOOP WITHIN IN JAGS
	tmp$win<-1
	for(i in 2:nrow(tmp))
		{
		tmp$win[i]<- ifelse(tmp$tagnumber[i]==tmp$tagnumber[i-1],tmp$win[i-1]+1,1)
		}
	indx<- match(c("basin","tagnumber","setdate","length","weight"),names(tmp))
	
    # CALCULATE CHANGIN IN LENGTH
    ## 1,2
	x1<- tmp[tmp$win==1,indx]
	names(x1)<-c("basin","tagnumber","setdate1","l1","w1")
	x2<- tmp[tmp$win==2,indx]	
	names(x2)<-c("basin","tagnumber","setdate2","l2","w2")
	out<- merge(x1,x2, by="tagnumber",all.y=TRUE)
	## 2,3		
	x1<- tmp[tmp$win==2,indx]
	names(x1)<-c("basin","tagnumber","setdate1","l1","w1")
	x2<- tmp[tmp$win==3,indx]	
	names(x2)<-c("basin","tagnumber","setdate2","l2","w2")
	out<- rbind(out,merge(x1,x2, by="tagnumber",all.y=TRUE))	
	## 3,4		
	x1<- tmp[tmp$win==3,indx]
	names(x1)<-c("basin","tagnumber","setdate1","l1","w1")
	x2<- tmp[tmp$win==4,indx]	
	names(x2)<-c("basin","tagnumber","setdate2","l2","w2")
	out<- rbind(out,merge(x1,x2, by="tagnumber",all.y=TRUE))	
	## 4,5		
	x1<- tmp[tmp$win==4,indx]
	names(x1)<-c("basin","tagnumber","setdate1","l1","w1")
	x2<- tmp[tmp$win==5,indx]	
	names(x2)<-c("basin","tagnumber","setdate2","l2","w2")
	out<- rbind(out,merge(x1,x2, by="tagnumber",all.y=TRUE))		
	## 5,6		
	x1<- tmp[tmp$win==5,indx]
	names(x1)<-c("basin","tagnumber","setdate1","l1","w1")
	x2<- tmp[tmp$win==6,indx]	
	names(x2)<-c("basin","tagnumber","setdate2","l2","w2")
	out<- rbind(out,merge(x1,x2, by="tagnumber",all.y=TRUE))	
	## 6,7		
	x1<- tmp[tmp$win==6,indx]
	names(x1)<-c("basin","tagnumber","setdate1","l1","w1")
	x2<- tmp[tmp$win==7,indx]	
	names(x2)<-c("basin","tagnumber","setdate2","l2","w2")
	out<- rbind(out,merge(x1,x2, by="tagnumber",all.y=TRUE))	
	## 7,8		
	x1<- tmp[tmp$win==7,indx]
	names(x1)<-c("basin","tagnumber","setdate1","l1","w1")
	x2<- tmp[tmp$win==8,indx]	
	names(x2)<-c("basin","tagnumber","setdate2","l2","w2")
	out<- rbind(out,merge(x1,x2, by="tagnumber",all.y=TRUE))	
	
    ## CALCULATE DL
    out$dl<- out$l2-out$l1
    ## CALCULATE DT
	out$dt<- as.numeric(out$setdate2-out$setdate1)/86400
	out$dy<- out$dt/365.25
    
    ## FLAG VALUES WHERE DL IS NEGATIVE
	out$flag<- ifelse(out$dl>0,0,1)
	
	# leave in fish with negative growth
	# measurement error
	#out<- subset(out, flag==0)
	#out$G<- log((out$l2+0.001)-out$l1)/out$dt	
	#out$flag<- 0
	#out<- subset(out, flag==0)
	out$dldt<- out$dl/out$dt # daily growth	
	#out$flag<- ifelse(out$dldt<4,0,1)
	#out$flag<- ifelse(out$tagnumber=="unknown",1,out$flag)
	#out<- subset(out, flag==0)	
	tmp<-aggregate(dl~basin.y+tagnumber,out,length)
	tmp<- tmp[order(tmp$basin.y,tmp$tagnumber),]
	tmp$ind_id<- c(1:nrow(tmp[tmp$basin.y=="lower",]),c(1:nrow(tmp[tmp$basin.y=="upper",])))
	tmp<- tmp[,-3]
	out<- merge(out, tmp, by=c("basin.y","tagnumber"), all.x=TRUE)
	out<- out[,-3]
	names(out)[1]<-"basin"
    out<-subset(out, !(is.na(basin)))
    # FOR INDEXING IN JAGS
    out$basin_id<-ifelse(out$basin=="lower",1,2)
    
    # MAKE VALIDATION DATASET
    val<- subset(dat,validate==1)
    val<- val[,match(c("tagnumber","basin","setdate",
        "segment_id","age","length","weight"),names(val))]
     # FOR INDEXING IN JAGS
    val$basin_id<-ifelse(val$basin=="lower",1,2)       
    out<- list(out=out,val=val)
	return(out)
	}
	
if(n=="tbl5")
	{
	options(scipen=15)

	tmp<-data.frame(parm=rownames(low_cor_vbgf$BUGSoutput$summary),
		estimate=low_cor_vbgf$BUGSoutput$summary[,1],
		estimate_sd=low_cor_vbgf$BUGSoutput$summary[,2],
		lbci=low_cor_vbgf$BUGSoutput$summary[,3],
		ubci=low_cor_vbgf$BUGSoutput$summary[,7],stringsAsFactors=FALSE)
	tmp$basin<-"Lower"
	tmp$parm[3]<- "Correlation"
	tmp$estimate[3]<- tmp$estimate[3]/(sqrt(tmp$estimate[1])*sqrt(tmp$estimate[4]))	
	out<-tmp
	tmp<-data.frame(parm=rownames(upp_cor_vbgf$BUGSoutput$summary),
		estimate=upp_cor_vbgf$BUGSoutput$summary[,1],
		estimate_sd=upp_cor_vbgf$BUGSoutput$summary[,2],
		lbci=upp_cor_vbgf$BUGSoutput$summary[,3],
		ubci=upp_cor_vbgf$BUGSoutput$summary[,7],stringsAsFactors=FALSE)
	tmp$basin<-"Upper"
	out<- rbind(out,tmp)
	out[out$parm %in% c("beta[1]","beta[2]"),c(2:5)]<- exp(out[out$parm %in% c("beta[1]","beta[2]"),c(2:5)])
	return(format(out,digits=3))
	}
}