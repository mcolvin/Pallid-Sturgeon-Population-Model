

# CREATE EMPERICAL DRIFT MATRICES
setwd("C:/Users/sreynolds/Documents/GitHub/Pallid-Sturgeon-Population-Model/pallid-sturgeon-population-model")

# LOAD BEND METADATA 
bend_meta<-readRDS("./inputs/bend_data.rds")

# LOAD DRIFT DATA 
drift_dat<- read.csv("./inputs/dat/sampleforsara.csv")

## LOWER BASIN
drift_dat<- drift_dat[1:nrow(bend_meta$lower),]
names(drift_dat)[2:ncol(drift_dat)]<- gsub("X", "", names(drift_dat)[2:ncol(drift_dat)])

# SCENARIOS
d<- c("10pct", "30pct", "50pct", "70pct", "90pct")
drift_mat<-lapply(d, function(x)
{
  out<-drift_dat[,grep(x, names(drift_dat))]
  names(out)<- lapply(strsplit(names(out), "_"), "[[", 1)
  names(out)<- rkm2bend(basin="lower", bend_meta = bend_meta, 
                        rkm=as.numeric(names(out)))
  nbends<-ncol(out)
  out[,(nbends+1):nrow(bend_meta$lower)]<- 0
  names(out)[(nbends+1):nrow(bend_meta$lower)]<-
    setdiff(as.character(1:nrow(bend_meta$lower)), names(out))
  out<- out[,order(as.numeric(names(out)))]
  out<- t(as.matrix(out))
  check<- max(abs(drift_dat$X-bend_meta$lower$UPPER_RIVER_RKM))
  if(check>0.1){return("Need to adjust matrix bends.")}
  return(out)
})
names(drift_mat)<- d
drift_matrices<-list()
drift_matrices$lower<-drift_mat
drift_matrices$upper<-NULL

saveRDS(drift_mat, "./inputs/drift_matrices.rds")
