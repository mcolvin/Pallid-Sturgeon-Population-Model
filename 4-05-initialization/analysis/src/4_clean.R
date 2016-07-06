
# FORMAT PSPAP DATA
names(dat)<- tolower(names(dat))
dat$tmp<- 1 # FOR COUNTING
dat$tagnumber<- as.character(dat$tagnumber)
dat$tagnumber<- ifelse(dat$tagnumber %in% c("","..........","\\…",
	"0000000000","N/A","NOFISHSCAN","unknown","XXXXXXXXXX"),
	"unknown",dat$tagnumber)
# MAKE ALL CHARACTERS UPPER CASE
dat$tagnumber<- toupper(dat$tagnumber)

## IDENTIFY UPPER AND LOWER BASIN
dat$basin<- ifelse(dat$segment_id%in% c(1:4, 21,22), "upper", "lower")


## FORMAT STOCKING DATA
names(stocked)<- tolower(names(stocked))
stocked$tagnumber<- as.character(stocked$tagnumber)
stocked$tagnumber<- toupper(stocked$tagnumber)

# ASSIGN ORIGIN TO CAPTURED FISH
dat$origin<-"natural"
dat[which(dat$tagnumber %in% stocked$tagnumber),]$origin<- "hatchery"
dat<- subset(dat, length>=0)
dat<- dat[order(dat$tagnumber,dat$setdate,decreasing=FALSE),]
dat$mr_id<-1
dat$mr_id<-c(1,
sapply(2:nrow(dat),function(x)
	{
	ifelse(dat$tagnumber[x]==dat$tagnumber[x-1] & dat$tagnumber[x]!="unknown",dat$mr_id[x-1]+1,1)
	}))
dat$tmp<-1

dat<-subset(dat,year<2016)


