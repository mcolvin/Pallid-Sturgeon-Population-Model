
# FORMAT PSPAP DATA
names(dat)<- tolower(names(dat))
dat$tmp<- 1 # FOR COUNTING
dat$tagnumber<- as.character(dat$tagnumber)

# MAKE ALL CHARACTERS UPPER CASE
dat$tagnumber<- toupper(dat$tagnumber)

## IDENTIFY UPPER AND LOWER BASIN
dat$basin<- ifelse(dat$segment_id%in% c(1:4, 21,22), "upper", "lower")

## ADD STOCKED FISH TO MIX ABOVE

## FORMAT STOCKING DATA
names(stocked)<- tolower(names(stocked))
stocked$hatchery<-as.character(stocked$hatchery)
names(stocked)[5]<-'setdate' 
stocked[which(is.na(stocked$hatchery)==TRUE),]$hatchery<- "UNKNOWN"
dat<-rbind.fill(dat,stocked)
dat<- subset(dat, length>=0)
dat$tagnumber<- ifelse(dat$tagnumber %in% c("","..........","\\…",
	"0000000000","N/A","NOFISHSCAN","unknown","XXXXXXXXXX"),
	"unknown",dat$tagnumber)
xxxx<-table(dat$tagnumber)
ppp<-names(xxxx[xxxx>5])[1]
dat$tagnumber<- ifelse(dat$tagnumber ==ppp,
	"unknown",dat$tagnumber)
# ASSIGN ORIGIN
tmp<- aggregate(tmp~tagnumber,dat,min)
tmp$origin<- ifelse(tmp$tmp==0,"hatchery","natural")
dat<- merge(dat, tmp[,-2],by="tagnumber",all.x=TRUE)

dat<- dat[order(dat$tagnumber,dat$setdate,decreasing=FALSE),]

dat$mr_id<-1
dat$mr_id<-c(1,
sapply(2:nrow(dat),function(x)
	{
	ifelse(dat$tagnumber[x]==dat$tagnumber[x-1] & dat$tagnumber[x]!="unknown",dat$mr_id[x-1]+1,1)
	}))
dat$lw_type<- ifelse(dat$tmp==0,"stocking","capture")
dat$tmp<-1



