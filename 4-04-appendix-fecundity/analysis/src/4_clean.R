

dat[dat==-99]<-NA
dat$id<-1:nrow(dat)

# SCALE VARIABLES
dat$fl_std<-scale(dat$FL, center = mean(na.omit(dat$FL)), scale = sd(na.omit(dat$FL)))
dat$w_std<-scale(dat$W, center = mean(dat$W), scale = sd(dat$W))



