## a big csv,fread is faster than read.csv
abund<-data.table::fread("lower-basin-abundance.csv")
# PULL 1 COMBINATION FORM THE POSTERIOR
id<-unique(abund$id)
indx<-which(abund$id%in%sample(id,1))
abund[indx,]
# EXPECTED ABUDANDANCE
vals<- plyr::ddply(abund,~segment, summarise,
    unknown=mean(unknown),
    hybrid=mean(hybrid),
    hatchery=mean(hatchery),
    total=mean(total))

# MEDIAN ABUDANDANCE
vals2<- plyr::ddply(abund,~segment, summarise,
                   unknown=median(unknown),
                   hybrid=median(hybrid),
                   hatchery=median(hatchery),
                   total=median(total))


tmp<- subset(abund, segment=="S14")
tmp<- subset(tmp, id %in% 10000:20000)
plot(tmp$hybrid)
