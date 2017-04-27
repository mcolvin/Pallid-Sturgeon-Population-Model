tables<- function(n){
	if(n=="1a")
		{
		models<-expand.grid(pred=c("L","W"),
			c("b0 + b1","b0[basin] + b1",
				"b0 + b1[basin]","b0[basin] + b1[basin]"),
			disp=c("N","Y","YY"),
			offset=c("N","Y"))
		models<-subset(models, !(pred == "W" & offset =="Y"))
		}
	if(n==1)
		{# MODEL SELECTION TABLE
		tmp<-data.frame(model=c(1:5),DIC=c(
			out00$BUGSoutput$DIC,
			out0$BUGSoutput$DIC,
			out1$BUGSoutput$DIC,
			out2$BUGSoutput$DIC,
			out3$BUGSoutput$DIC))
		tmp$dDIC<- tmp$DIC-min(tmp$DIC)
		tmp$lik<- exp(-0.5*tmp$dDIC)
		tmp$weight<- tmp$lik/sum(tmp$lik)
		tmp<- tmp[order(tmp$DIC),]
		tmp<-format(tmp,digits=1,nsmall=3,scientific = FALSE)
		return(tmp)
		}

	if(n==2)
		{
		options(scipen=15)
		tmp<- data.frame(model=c(5,5,3,2,4,4),
			parameter="B0",
			basin=c("RPMA4","RMPA2","RMPA2 and 4","RMPA2 and 4","RPMA4","RMPA2"),
			est=round(c(out3$BUGSoutput$mean$a,		
				out1$BUGSoutput$mean$a,		
				out0$BUGSoutput$mean$a,	
				out2$BUGSoutput$mean$a),2),
			bci=c(paste0(round(quantile(out3$BUGSoutput$sims.matrix[,"a[1]"],c(0.05,0.95)),2),collapse=","),
				paste0(round(quantile(out3$BUGSoutput$sims.matrix[,"a[2]"],c(0.05,0.95)),2),collapse=","),
				paste0(round(quantile(out1$BUGSoutput$sims.matrix[,"a"],c(0.05,0.95)),2),collapse=","),
				paste0(round(quantile(out0$BUGSoutput$sims.matrix[,"a"],c(0.05,0.95)),2),collapse=","),
				paste0(round(quantile(out2$BUGSoutput$sims.matrix[,"a[1]"],c(0.05,0.95)),2),collapse=","),
				paste0(round(quantile(out2$BUGSoutput$sims.matrix[,"a[1]"],c(0.05,0.95)),2),collapse=",")))

		tmp<- rbind(tmp,data.frame(model=c(5,5,3,2,4),
			parameter="B1",
			basin=c("RPMA4","RMPA2","RMPA2 and 4","RMPA2 and 4","RMPA2 and 4"), 			
			est=round(c(out3$BUGSoutput$mean$b_fec,		
				out1$BUGSoutput$mean$b_fec,
				out0$BUGSoutput$mean$b_fec,	
				out2$BUGSoutput$mean$b_fec),5),
			bci=c(paste0(round(quantile(out3$BUGSoutput$sims.matrix[,"b_fec[1]"],c(0.05,0.95)),5),collapse=","),
				paste0(round(quantile(out3$BUGSoutput$sims.matrix[,"b_fec[2]"],c(0.05,0.95)),5),collapse=","),
				paste0(round(quantile(out1$BUGSoutput$sims.matrix[,"b_fec"],c(0.05,0.95)),5),collapse=","),
				paste0(round(quantile(out0$BUGSoutput$sims.matrix[,"b_fec"],c(0.05,0.95)),5),collapse=","),
				paste0(round(quantile(out2$BUGSoutput$sims.matrix[,"b_fec"],c(0.05,0.95)),5),collapse=","))))
			
		tmp<- rbind(tmp,data.frame(model=c(5,5,3,3,2,4,4),
				parameter="sigma",
			basin=c("RPMA4","RMPA2","RPMA4","RMPA2","RMPA2 and 4","RPMA4","RMPA2"), 
			est=round(c(out3$BUGSoutput$mean$sigma,		
				out1$BUGSoutput$mean$sigma,		
				out0$BUGSoutput$mean$sigma,	
				out2$BUGSoutput$mean$sigma),2),
			bci=c(paste0(round(quantile(out3$BUGSoutput$sims.matrix[,"sigma[1]"],c(0.05,0.95)),2),collapse=","),
				paste0(round(quantile(out3$BUGSoutput$sims.matrix[,"sigma[2]"],c(0.05,0.95)),2),collapse=","),
				paste0(round(quantile(out1$BUGSoutput$sims.matrix[,"sigma[1]"],c(0.05,0.95)),2),collapse=","),
				paste0(round(quantile(out1$BUGSoutput$sims.matrix[,"sigma[2]"],c(0.05,0.95)),2),collapse=","),
				paste0(round(quantile(out0$BUGSoutput$sims.matrix[,"sigma"],c(0.05,0.95)),2),collapse=","),
				paste0(round(quantile(out2$BUGSoutput$sims.matrix[,"sigma[1]"],c(0.05,0.95)),2),collapse=","),
				paste0(round(quantile(out2$BUGSoutput$sims.matrix[,"sigma[2]"],c(0.05,0.95)),2),collapse=","))))
		tmp$bci<-paste0("[",tmp$bci,"]",sep="")
		tmp$selectionOrder<-1
		tmp[tmp$model==3,]$selectionOrder<-2
		tmp[tmp$model==2,]$selectionOrder<-3
		tmp[tmp$model==4,]$selectionOrder<-4
		tmp<- tmp[order(tmp$selectionOrder,tmp$parameter),]
		tmp<- tmp[,-6]
		return(tmp)

		
		}
		
	
}