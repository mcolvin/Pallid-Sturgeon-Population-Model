

# DROP V1
sims_lower<- sims_lower[,V1:=NULL]
inputs_lower<- inputs_lower[,V1:=NULL]
# ONLY LOOK AT YEARS 1 TO 50
#sims<- subset(sims, year<=50)
sims_lower<- sims_lower[,j=list(abundance=sum(abundance)),
	by=list(origin, year,scenario)]
sims_lower<- merge(sims_lower,inputs_lower,by="scenario")
pex<- subset(sims_lower, origin=='n' & year%in%c(50,100))
#pex<- merge(pex,inputs_lower,by="scenario")
pex$ex<- ifelse(pex$abundance<=1,1,0)
pex_age0<- pex[,j=list(pex=mean(ex)),
	by=list(year,age0_stock)]
pex_age1<- pex[,j=list(pex=mean(ex)),
	by=list(year,age1_stock)]

# BINNING FOR CPTS
sims_lower$adults_ini_n_bin<-findInterval(sims_lower$adults_ini_n,c(0,250,500,750,1000))
sims_lower$adults_ini_h_bin<-findInterval(sims_lower$adults_ini_h,c(18000,19750,21500,23250,25000)) 
sims_lower$juv_ini_n_bin<-findInterval(sims_lower$juv_ini_n,c(0,250,500,750,1000))
sims_lower$juv_ini_h_bin<-findInterval(sims_lower$juv_ini_h,c(3750,3875,4000,4125))
sims_lower$S2_bin<-findInterval(sims_lower$S2,c(0.9,0.9125,0.925,0.9375,0.95))
sims_lower$S1_bin<- findInterval(sims_lower$S1,c(0.6,0.6375,0.675,0.7125,0.75))
sims_lower$S0_bin<-findInterval(sims_lower$S0,c(0,0.0002,0.0004,0.0006,0.0008))
sims_lower$viableGam_bin<- findInterval(sims_lower$viableGam,c(0,0.000025,0.00005,0.000075,0.0001))

# STOCKING TYPE
sims_lower$stock_type<- 0
sims_lower[sims_lower$age1_stock_bin>0,]$stock_type<- 1
sims_lower[sims_lower$age1_stock_bin==0 &sims_lower$age0_stock_bin==0 ,]$stock_type<- -1

sims_lower$stock_type<- paste(sims_lower$stock_type,sims_lower$age0_stock, sims_lower$age1_stock)

# 5k natural fish?
tmp<- subset(sims_lower, origin=='n' & year==50)
tmp$tmp<- 1
tmp$fiveK<- ifelse(tmp$abundance>=5000, 1, 0)
cpt<- dcast(tmp, adults_ini_n_bin+adults_ini_h_bin+juv_ini_n_bin+juv_ini_h_bin+
	S2_bin+S1_bin+S0_bin+viableGam_bin+stock_type~fiveK, value.var='tmp',sum)

	
	
tmp$abundance_bin<-findInterval(tmp$S0,c(0,20,40,60,80))
cpt<- dcast(tmp, adults_ini_n_bin+adults_ini_h_bin+juv_ini_n_bin+juv_ini_h_bin+
	S2_bin+S1_bin+S0_bin+viableGam_bin+stock_type~abundance_bin, value.var='tmp',sum,
	drop=FALSE)


	