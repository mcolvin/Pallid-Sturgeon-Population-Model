library(triangle)
library(reshape2)
library(plyr)
library(lattice)
library(shiny)
#added outer loop parameteric uncertainty
# need to modify initialization and sims to account 
# for the pulling a mean survival from a beta 


#rest of sex:origins, and uncert, pull from beta dist


sim_nxt_adults<- function(matured,sexRatio,prev,S,tmax)
{
  nxt<-rbinom(tmax,prev,S)
  nxt<- nxt+round(matured*sexRatio,0)
  return(c(0,nxt[-tmax]))  
}

bundleReps<- function(appendTo,append,rep)
{
  app<-as.data.frame(append)
  names(app)<- paste("yr",c(1:ncol(app)))
  app$rep<-rep
  out<- rbind(appendTo,app)
  return(out)
}

shinyServer(function(input, output) {
	
  hypo<- reactive({
    out<- data.frame(h=c(1,2,3),desc=c(
      "Processes and conditions affecting reproductive behaviors and synchrony limit probability of producing viable gametes.  ",
      "Functional spawning sites with supportive hydraulics and substrate cannot be accessed by reproductive adults because of barriers to upstream migration.",
      "Aggregations of reproductive adults are inadequate to allow mates to find each other because of inadequate hydrology-related cues."))
    out_x<- as.character(out[out$h==input$hypo,]$desc)
    return(out_x)
  })
  output$hypo_value <- renderUI({h4(hypo())})
 
  
  inits<- reactive({
    # INITIALIZE POPULATION

    ## CREATE SURVIVOR FUNCTION WITH OUTER LOOP UNCERTAINTY
    #mu<-  c(input$S1,input$S2,input$S3plus)
    #var_mu<- 0.007
    #a <- ((1 - mu) / var_mu - 1 / mu) * mu ^ 2
    #b <- alpha * (1 / mu - 1)
    # for(i in 1:length(mu))
    #   {
    #   x<- rbeta(n=input$nreps, shape1=a[i], shape2=b[1])  
    #   S<-c(S,x)
    #   }
    # S<- matrix(S, nrow=3,ncol=input$nreps,byrow=TRUE)
    
    ## CREATE SURVIVOR FUNCTION (NO OUTER LOOP)
    S<- c(input$S1,input$S2,rep(input$S3plus, input$maxAge-2))
    S<- S[c(1,2,rep(3,input$maxAge-2))]
    ## INITIALIZE JUVENILES
    max_juv<- 9
    S_juv<- S
    S_juv[max_juv:input$maxAge]<-0
    S_juv<- cumprod(S_juv)
    S_juv<- S_juv/sum(S_juv)
    juv_n<- rmultinom(1,(input$juv_ini_n*1000),S_juv)
    juv_h<- rmultinom(1,(input$juv_ini_h*1000),S_juv)

    ## ASSIGN MALES AND FEMALES
    adults_n_f<- rbinom(1, (input$adults_ini_n*1000), input$sr_n)
    adults_n_m<- (input$adults_ini_n*1000)-adults_n_f
    adults_h_f<- rbinom(1,(input$adults_ini_h*1000),input$sr_h)
    adults_h_m<- (input$adults_ini_h*1000)-adults_h_f
    
    ## ASSIGN ADULTS TO AGE
    S_ad<-S
    S_ad[1:max_juv]<-0
    S_ad[(max_juv+1):input$maxAge]<-cumprod(S[(max_juv+1):input$maxAge])
    adult_n_f_age<-rmultinom(1,adults_n_f,prob=S_ad)
    adult_n_m_age<-rmultinom(1,adults_n_m ,prob=S_ad)
    adult_h_f_age<-rmultinom(1,adults_h_f,prob=S_ad)
    adult_h_m_age<-rmultinom(1,adults_h_m,prob=S_ad)
    
    out<- list(survivals=data.frame(age=c(0:input$maxAge),survival=c(input$S0,S),S_ad=c(0,S_ad)),
               inits=data.frame(age=c(1:input$maxAge),
                                j_h=juv_h,
                                j_n=juv_n,                                
                                a_h_f=adult_h_f_age,
                                a_h_m=adult_h_m_age,
                                a_n_f=adult_n_f_age,
                                a_n_m=adult_n_m_age))
    return(out)
    })

output$plot_01<- renderPlot({
    dat<- inits()$survivals
    par(mar=c(4,4,0.1,0.1))
    plot(survival~age,dat,type='l',las=1,ylab="Survival (%)",
         xlab="Age",lwd=3,col="darkgrey")
    },width=500,height = 200)  

maturity<- reactive({
  # 1. MATURITY FUNCTION
  x<-c(0,input$aa, input$a,input$b,input$c,input$d,input$maxAge)
  y<-c(0,0,        0.25,    0.5,   0.75,   1,    1)
  mat<- approxfun(x,y)
  out<- data.frame(age=c(1:input$maxAge),maturity=mat(c(1:input$maxAge)))
  return(out)
  })

output$plot_02<- renderPlot({
  # MATURITY FUNCTION PLOT
  dat<- maturity()
  par(mfrow=c(1,2),mar=c(4,4,1,0),oma=c(3,1,1,1),cex.lab=1.3)
  plot(maturity*100~age,dat,type='l',las=1,lwd=3,
       ylab="Sexually mature (%)",
       xlab="Age", col="darkgrey")
  },width=750,height = 300)


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


xx<- reactive({
  out<- data.frame()
  j_n_out<-j_h_out<-a_h_m_out<-a_h_f_out<-a_n_f_out<-a_n_m_out<-data.frame()
  for(j in 1:input$nreps)
  {
  maxAge<- input$maxAge
  Linf=1683
  K=0.036
  t0=-5.9
  #sigma=93.77
  FL<-Linf*(1-exp(-K*(c(1:maxAge)-t0)))
  mat_dat<- maturity()
  dat<- inits()$inits
  survival<-inits()$survivals$survival[-1]# drop age-0 survival
  a_n_f<-a_n_m<-a_h_f<-a_h_m<-j_h<-j_n<- matrix(0,nrow=maxAge,ncol=input$nyears)
  a_n_f[,1]<- dat$a_n_f
  a_n_m[,1]<- dat$a_n_m
  a_h_f[,1]<- dat$a_h_f
  a_h_m[,1]<- dat$a_h_m
  j_h[,1]<- dat$j_h
  j_n[,1]<- dat$j_n  
  
  for(i in 2:input$nyears)
  {
  stocked<-rpois(1,round(input$fe_stock*0.051+
                   input$efl_stock*0.051^0.666+
                   input$juv_stock*0.051^0.333,0))
  nxt<- rbinom(maxAge,j_h[,i-1],survival)
  nxt_mat_h<- rbinom(maxAge,nxt,mat_dat$maturity)
  nxt<- nxt-nxt_mat_h
  j_h[,i]<-c(stocked,nxt[-maxAge])
  
  age0<- sum(a_h_f[,i]*a_n_f[,i])*0.25*2
  nxt<- rbinom(maxAge,j_n[,i-1],survival)
  nxt_mat_n<- rbinom(maxAge,nxt,mat_dat$maturity)
  nxt<- nxt-nxt_mat_n  
  j_n[,i]<-c(age0,nxt[-maxAge])  
    
  # NUMBER OF ADULT NATURAL ORIGIN FEMALES  
  a_n_f[,i]<-sim_nxt_adults(matured=nxt_mat_n,sexRatio=0.5,
      prev=a_n_f[,i-1],S=survival,tmax=maxAge)

  # NUMBER OF ADULT NATURAL ORIGIN MALES    
  a_n_m[,i]<-sim_nxt_adults(matured=nxt_mat_n,sexRatio=0.5,
       prev=a_n_m[,i-1],S=survival,tmax=maxAge)

  a_h_f[,i]<-sim_nxt_adults(matured=nxt_mat_h,sexRatio=0.5,
                            prev=a_h_f[,i-1],S=survival,tmax=maxAge)
  
  a_h_m[,i]<-sim_nxt_adults(matured=nxt_mat_h,sexRatio=0.5,
                            prev=a_h_m[,i-1],S=survival,tmax=maxAge)
  }
  
  a_n_f_out<- bundleReps(appendTo=a_n_f_out,append=a_n_f,rep=j)
  a_n_m_out<- bundleReps(appendTo=a_n_m_out,append=a_n_m,rep=j)
  a_h_f_out<- bundleReps(appendTo=a_h_f_out,append=a_h_f,rep=j)  
  a_h_m_out<- bundleReps(appendTo=a_h_m_out,append=a_h_m,rep=j)
  j_h_out<- bundleReps(appendTo=j_h_out,append=j_h,rep=j)  
  j_n_out<- bundleReps(appendTo=j_n_out,append=j_n,rep=j)
  
  }# end J (reps)
  out<- list(age=c(1:maxAge),survival=survival,
             a_n_f=a_n_f_out,
             a_n_m=a_n_m_out,
             a_h_f=a_h_f_out,
             a_h_m=a_h_m_out,
             j_h=j_h_out,
             j_n=j_n_out)
  return(out)    
  })
  

output$tbl1<- renderTable({
  out<- as.data.frame(xx()$a_n_m)
  return(out)
})


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
  
  
})