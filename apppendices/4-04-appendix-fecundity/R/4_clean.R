

dat[dat==-99]<-NA
dat$id<-1:nrow(dat)

# SCALE VARIABLES
mean_fl<-mean(na.omit(dat$FL))
sd_fl<-sd(na.omit(dat$FL))

dat$fl_std<-scale(dat$FL, center = mean_fl, scale = sd_fl)
dat$w_std<-scale(dat$W, center = mean(dat$W), scale = sd(dat$W))



dat$ws<- (10^-6.2561)*(dat$FL^3.2932)/1000
dat$kn<- dat$W/dat$ws 
	