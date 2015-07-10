figures<- function(n)
	{
	if(n=="plot_02")
		{
		#output$plot_02<- renderPlot({
		# MATURITY FUNCTION PLOT
		dat<- maturity()
		par(mfrow=c(1,2),mar=c(4,4,1,0),oma=c(3,1,1,1),cex.lab=1.3)
		plot(maturity*100~age,dat,type='l',las=1,lwd=3,
			ylab="Sexually mature (%)",
			xlab="Age", col="darkgrey")
		#},width=750,height = 300)
		}
	if(n=="plot_03")
		{
output$plot_03<- renderPlot({
  # INITIAL POPULATION PLOT
  dat<- inits()$inits
  par(mfrow=c(1,2),mar=c(2,3,1,0),oma=c(3,1,1,1))
  plot(0,0, 
       ylim=c(0,max(dat[,c(6,7)]/1000)), 
       xlim=c(0,input$maxAge),
       type='n',
       ylab="",
       xlab="",
       las=1)
  points(a_n_f/1000~age,dat,type='p',pch=1)
  points(a_n_m/1000~age,dat,type='p',pch=19)
  legend("topright",legend=c("Females","Males"),pch=c(1,19),bty='n',title="Natural origin adults")
  plot(0,0, 
       ylim=c(0,max(dat[,c(4,5)]/1000)), 
       xlim=c(0,input$maxAge),
       type='n',
       ylab="",
       xlab="",
       las=1)  
  points(a_h_f/1000~age,dat,type='p',pch=1)
  points(a_h_m/1000~age,dat,type='p',pch=19) 
  mtext(side=2,"Abundance (x 1000)",outer=TRUE,line=0,cex=1.3)
  mtext(side=1,"Age",outer=TRUE,line=1,cex=1.3)
  legend("topright",legend=c("Females","Males"),pch=c(1,19),bty='n',title="Hatchery origin adults")
  },width=750,height = 300)# end plot_01


output$plot_03z<- renderPlot({
  # FECUNDITY PLOT
  maxAge<-input$maxAge
  Linf=1683
  K=0.036
  t0=-5.9
  #sigma=93.77
  dat<- data.frame(age=c(1:maxAge))
  dat$FL<-Linf*(1-exp(-K*(dat$age-t0)))
  dat$fec<- ifelse(dat$age>=input$aa,
                   input$a_fec*10^-8 *dat$FL^input$b_fec,
                   NA)
  
  plot(fec/1000~age,dat, 
       type='l',
       ylab="Fecundity (x 1000)",
       xlab="Age",
       las=1,cex.lab=1.5,lwd=3, col="darkgrey")
   },width = "auto", height = "auto")



output$plot_044<- renderPlot({
  plot_dat<- rbind(xx()$j_n, xx()$a_n_f,
                   xx()$a_n_m,xx()$j_h, xx()$a_h_f,
                   xx()$a_h_m)
  foo<- c()
  for(x in 1:max(plot_dat$rep))
    {
    dat<- subset(plot_dat, rep==x)
    total<- unlist(apply(dat[,1:input$nyears],2,sum))
    foo<- c(foo,total)
    } 
  ymax<- max(foo)/1000
  #trans_grey<- rgb(120,120,120,alpha=10,maxColorValue=255)
  trans_grey<- rgb(0,0,0,alpha=20,maxColorValue=255)
  par(mar=c(4,7,1,1))
  plot(c(1:input$nyears), rep(10000,input$nyears), xlab="Year",
       ylab="Population abundance \n (natural and hatchery produced x 1000)",
       las=1,type="n",ylim=c(0,ymax))
 for(x in 1:max(plot_dat$rep))
    {
    dat<- subset(plot_dat, rep==x)
    total<- apply(dat[,1:input$nyears],2,sum)
    points(c(1:input$nyears),total/1000,col=trans_grey,type='l',
         lwd=3)
    } 
 # out$tmp<- 1
  #out<- subset(out,year==50)
#  #total<- sum(out$tmp)
#  out<- subset(out,total<=50) 
#  extinct<- sum(out$tmp)
#  pep<- extinct/total
#  legend("bottomleft", 
#         legend=paste("Psuedo-extinction probability:", round(pep,2)
 #              ),bty='n')
},width=750,height = 300)  
  
output$plot_01<- renderPlot({
    dat<- inits()$survivals
    par(mar=c(4,4,0.1,0.1))
    plot(survival~age,dat,type='l',las=1,ylab="Survival (%)",
         xlab="Age",lwd=3,col="darkgrey")
    },width=500,height = 200)  

# old figures

	if(n==1)
		{
		trans_grey<- rgb(120,120,120,alpha=10,maxColorValue=255)
	plot(total~year,out, xlab="Year",
		ylab="Population (natural and hatchery produced)",type='n')
	for(x in 1:max(out$rep)) 
		{points(total~year,out,subset=rep==x,col=trans_grey,type='l',
			lwd=2)}
	panLab(paste("Psuedo-extinction probability:", 
		round(
			(nrow(out[out$year==50& out$total < 50,]))/max(out$rep),
				2)))

}
	if(n==2)
		{
		matplot(c(2:100),vals[,-1],type='l',xlab="Year",ylab="Lambda")
		}	
	if(n==3)
		{
		xyplot(total~year,out, group=rep,type='l',xlab="Year",ylab="Population (hatural and hatchery produced)")
		}	
	if(n==4)
		{
		matplot(c(2:100),vals[,-1],type='l',xlab="Year",ylab="Lambda")
		}	
	if(n==5)
		{
		boxplot(mn_lambda~scenario, g_mn,ylab="Lambda (geometric mean)")
		}	
	if(n==6)
		{
		par(mar=c(4,15,1,1))
		plot(c(min(tornado$mns),max(tornado$mxs)),c(1,14),type="n",las=1,xlab="Expected population size",
			yaxt="n",ylab="")
		axis(2, at=c(1:14),labels=tornado$parms,las=1)
		segments(tornado$mns,tornado$y,tornado$mxs,tornado$y,lwd=4)
		}	
	if(n==7){par(mar=c(4,15,1,1))
		plot(c(500,3000),c(1,14),type="n",las=1,xlab="Expected population size",
			yaxt="n",ylab="")
		axis(2, at=c(1:14),labels=tornado$parms,las=1)
		segments(tornado$mns,tornado$y,tornado$mxs,tornado$y,lwd=4)
		}
	if(n==8){par(mar=c(4,15,1,1))
		plot(c(0,65000),c(1,14),type="n",las=1,xlab="Expected population size",
			yaxt="n",ylab="")
		axis(2, at=c(1:14),labels=tornado$parms,las=1)
		segments(tornado$mns,tornado$y,tornado$mxs,tornado$y,lwd=4)
		}
	if(n==9){par(mar=c(4,15,1,1))
		plot(c(0,2000),c(1,14),type="n",las=1,xlab="Expected population size",
			yaxt="n",ylab="")
		axis(2, at=c(1:14),labels=tornado$parms,las=1)
		segments(tornado$mns,tornado$y,tornado$mxs,tornado$y,lwd=4)
		}
	if(n==10){}
	if(n==11){}
	if(n==12){}
	if(n==13){}
	if(n==14){}
	}