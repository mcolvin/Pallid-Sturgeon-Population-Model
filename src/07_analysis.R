

## INPUT
input<-list()

input$basin<-"Upper"
input$maxage<-c(41,60)
input$sexratio<-c(0.33,0.5)
input$natural<- c(0,12)
input$hatchery<- c(0,43000)
input$natural_age0<- c(200,0)
input$hatchery_age0<- c(200,0)

# WEIGHT-LENGTH
input$a_prime<-c(-13.84,-14.09)
input$b<-c(3.188,3.24)
input$lw_er<-c(0.1371,0.165)

# FECUNITY-LENGTH
input$fec_a<-c(10.77,11.26)
input$fec_b<-c(0.62,0.57)
input$fec_er<-c(0.30,0.39)

# GROWTH
## PROGRAMMED INTO INITIILAIZATION FUNCTION

## SEXUAL MATURITY AND RETURN TO SPAWNING	
input$age_mat<- c(8,8)  ####fixme#### make it work for lower and upper
input$mat_k<-c(0.2,0.2) ####fixme#### make it work for lower and upper
input$spn_a<-c(-17.5,17.5)
input$spn_B<- c(0.35,0.35)

## SURVIVAL
input$phi_age0_mean<-c(0.001,0.001)
input$phi_age0_er<-c(0.01,0.001)
input$phi_age1_mean<-c(0.68,0.95)
input$phi_age1_er<-c(0.1,0.1)
input$phi_age2_mean<-c(0.92,0.95)
input$phi_age2_er<-c(0.01,0.01)
input$recruitment<- FALSE


## STOCKING ##
### FINGERLINGS
input$fingerling<-c(0)
input$fingerling_month<-c(5)
#input$fingerling_mn<-c(30)
#input$fingerling_sd<-c(0.1)
input$fingerling_stocking_rkm<-c(50)
### YEARLINGS
input$yearling<- 0
input$yearling_month<- 9
input$yearling_mn<- 100
input$yearling_sd<- 15
input$yearling_age<- 15 # months
input$yearling_stocking_rkm<- 50
## END STOCKING ##


## SPATIAL INPUTS ##
input$spatial<- FALSE
### SPATIAL STRUCTURE
input$adult_spatial_structure<- "Uniform" # "Emperical"
input$age0_n_spatial_structure<- "Uniform"# "Emperical"
input$age0_h_spatial_structure<- "Uniform"# "Emperical"
### SPAWNING HOTSPOTS
## END SPATIAL INPUTS ##



## SIMULATION INPUTS
input$nreps<- 100
input$startYear<- 2015
input$nyears<- 50
input$daug<- 45000
input$size_indices<-TRUE



# PROCESS INPUTS FOR INITIALIZATON AND SIMULATION
inputs<-modelInputs(input=input)
# RUN MODEL
out<- sim(inputs=inputs)

 
dput(out,"./output/2016-001/2016-001-output.txt")
dput(inputs,"./output/2016-001/2016-001-inputs.txt")

indx<-which(rowSums(out$qp)>1)
tmp<-data.frame(year=c(2015+0:49),
	sq=apply(out$sq[indx,],1,mean),
	qp=apply(out$qp[indx,],1,mean),
	pm=apply(out$pm[indx,],1,mean),
	mt=apply(out$mt[indx,],1,mean),
	tr=apply(out$tr[indx,],1,mean))


matplot(tmp$year,tmp[,-1],type='l',las=1,ylab="Incrimental PSD value",
	xlab="Year",lwd=2)
	text(2015+45,tmp[nrow(tmp),],c("PSD-SQ","PSD-QP","PSD-PM","PSD-MT","PSD-T"),
		pos=3)
savePlot("./output/2016-001/figure-01.wmf",type='wmf') 
 
# PSD TABLE FOR VARYING YEARS
tbl1<-data.frame(PSD=c("PSD-SQ","PSD-QP","PSD-PM","PSD-MT","PSD-T"),
	yr2015=unlist(tmp[tmp$year==2015,-1]),
	yr2025=unlist(tmp[tmp$year==2025,-1]),
	yr2050=unlist(tmp[tmp$year==2050,-1]),
	yrLast=unlist(tmp[tmp$year==max(tmp$year),-1]))
write.csv(format(tbl1,digits=0),"./output/2016-001/table-01.csv")


# BIOMASS
x<-out$years
y<-(apply(out$biomass,1,mean)/(1000*1000))
yup<-(apply(out$biomass,1,max)/(1000*1000))
ylo<-(apply(out$biomass,1,min)/(1000*1000))
plot(x,y,ylab="Biomass (kg; x1000)",
	xlab="Year",las=1,type='l')
polygon(c(x,rev(x)),c(ylo,rev(yup)),col="lightgrey",border="lightgrey")
points(x,y,type='l')
savePlot("./output/2016-001/figure-02.wmf",type='wmf')
 
 
# TOTAL ABUNDANCE 
y<-(apply((out$hatchery+out$natural)[-1,],1,mean))/1000
yup<-(apply((out$hatchery+out$natural)[-1,],1,max))/1000
ylo<-(apply((out$hatchery+out$natural)[-1,],1,min))/1000
plot(out$years,y,ylab="Total abundance (x1000)",
	xlab="Year",las=1,type='l')
polygon(c(x,rev(x)),c(ylo,rev(yup)),col="lightgrey",border="lightgrey")
points(x,y,type='l')
savePlot("./output/2016-001/figure-03.wmf",type='wmf')

# MEAN WEIGHT 
y<-(apply(out$mn_wght,1,mean))/1000
yup<-(apply(out$mn_wght,1,max))/1000
ylo<-(apply(out$mn_wght,1,min))/1000
plot(out$years,y,ylab="Mean weight (kg)",
	xlab="Year",las=1,type='l')
polygon(c(x,rev(x)),c(ylo,rev(yup)),col="lightgrey",border="lightgrey")
points(x,y,type='l')
savePlot("./output/2016-001/figure-04.wmf",type='wmf')


