
# SET UP COMMUNICATION TO DBASE
com8<- odbcConnectAccess2007("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Data/pallids.accdb")

## IMPORT LENGTH WEIGHT DATA
dat<- sqlFetch(com8,"Pallid-Sturgeon-Length-Weight-Data")
## IMPORT DATA FOR STOCKED FISH
stocked<- sqlFetch(com8,"stocked-fish")

# FORMAT PSPAP DATA
names(dat)<- tolower(names(dat))
dat$tmp<- 1 # FOR COUNTING
dat$tagnumber<- as.character(dat$tagnumber)

# MAKE ALL CHARACTERS UPPER CASE
dat$tagnumber<- toupper(dat$tagnumber)

## IDENTIFY UPPER AND LOWER BASIN
dat$basin<- ifelse(dat$segment_id%in% c(1:4, 21,22), "upper", "lower")

## ADD STOCKED FISH TO MIX ABOVE

## FORMAT STOCKING DATA SO IT CAN
## BE APPENDED TO FIELD CAPTURE DATA
names(stocked)<- tolower(names(stocked))
stocked$hatchery<-as.character(stocked$hatchery)
names(stocked)[5]<-'setdate' # assign stocking data to set date
stocked[which(is.na(stocked$hatchery)==TRUE),]$hatchery<- "UNKNOWN"
stocked$headstart<- stocked$setdate

## ADD STOCKING DATA TO FIELD DATA
dat<-rbind.fill(dat,stocked)
dat<- subset(dat, length>0)
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



# ASSIGN BIRTHDAY AND STOCKDATE TO CAPTURED FISH BY PIT TAG
tmp<- aggregate(birthday~tagnumber,dat,min)
names(tmp)[2]<- "birthdate"
tmp2<- aggregate(headstart~tagnumber,dat,min)
names(tmp2)[2]<- "stockdate"
tmp<- merge(tmp, tmp2, by="tagnumber", all.x=TRUE)
dat<- merge(dat, tmp,by="tagnumber",all.x=TRUE)
## CALCULATE THAT AGE FOR HATCHERY FISH
dat$age<- as.numeric(dat$setdate-dat$birthdate)/365
dat$headstart<- as.numeric(dat$stockdate-dat$birthdate)/365# years
## FISH TO ESTIMATE AGE FOR 
## STRATIFIED INTO FIRST 5 YEARS AND THEN 6-10, 11-15, 16-20, 20+
## AND BY BASIN
dat$age_validation_bin<- cut(dat$age, 
    breaks=c(0,1,2,3,4,5,10,15,20,200),
    include.lowest=TRUE, 
    right=FALSE,
    labels=c("0-1","1-2","2-3","3-4","4-5","5-10",
        "10-15","15-20","20+"))
n<-aggregate(age~age_validation_bin+basin,dat,length, subset=age>0)
n$n_to_sample<- floor(n$age*0.05)
dat$indx<-1:nrow(dat)
## SUBSET INVIDUALS TO VALIDATE
set.seed(1232)
indx<- unlist(lapply(1:nrow(n), function(x)
    {
    subs<-dat[which(dat$age_validation_bin == n$age_validation_bin[x] &
        dat$basin==n$basin[x] & 
        dat$age>0),]$indx
    nsample<-  sample(subs, 
        n$n_to_sample[x],
        replace=FALSE)
    }))

## ASSIGN VALIDATION STATUS    
dat$validate<-0 # TRAINING
dat[which(dat$indx %in% indx),]$validate<-1 # VALIDATION

