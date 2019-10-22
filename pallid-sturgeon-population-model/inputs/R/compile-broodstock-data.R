

setwd("C:/Users/sreynolds/Documents/GitHub/Pallid-Sturgeon-Population-Model/pallid-sturgeon-population-model")
library(plyr)
require(xlsx)

##########################
#                        #
#   CAPTIVE BROODSTOCK   #
#                        #
##########################
captive_UB<- read.xlsx("./inputs/broodstock/Captive Broodstock at GPNFH 4-19-19.xlsx", 
                       "Captive Broodstock")
captive_MB<- read.xlsx("./inputs/broodstock/Data for Landon.xlsx", 
                       "Captive Broodstock Family")
## CHANGE PIVOTS TO ONLY INCLUDE MALES 
captive_MB_male<- read.xlsx("./inputs/broodstock/Data for Landon - Males.xlsx", 
                            "Captive Broodstock Family")
## CHANGE PIVOTS TO ONLY INCLUDE FEMALES 
captive_MB_female<- read.xlsx("./inputs/broodstock/Data for Landon - Females.xlsx", 
                              "Captive Broodstock Family")
## COMBINE INTO DATAFRAME BY FAMILY LOT
captive_MB[which(captive_MB$Row.Labels=="Triploid "),]$Row.Labels<- "Triploid"
captive_MB$Female<- ifelse(captive_MB$Row.Labels %in% captive_MB_female$Row.Labels,
                          1,0)
captive_MB$Male<- ifelse(captive_MB$Row.Labels %in% captive_MB_male$Row.Labels,
                          1,0)
captive_MB$Both<- ifelse(captive_MB$Female==1 & captive_MB$Male==1, 1, 0)
indx<-which(captive_MB$Both==1)
indx<- indx[-which(captive_MB$Row.Labels[indx] %in% 
                     c("(blank)", "UNKNOWN", "Triploid", "Grand Total"))]
check<-sapply(1:length(indx), function(i)
{
  if(i != length(indx))
  {
    tmp<- which(captive_MB[(indx[i]+1):(indx[i+1]-1),]$Both==1)
  }
  if(i == length(indx))
  {
    tmp<- which(captive_MB[(indx[i]+1):(nrow(captive_MB)-1),]$Both==1)
  }
  return(length(tmp))
})

dat<-lapply(1:length(indx), function(i)
{
  yr<-as.numeric(as.character(captive_MB$Row.Labels[indx[i]]))
  if(i!=length(indx))
  {
    indxF<- which(captive_MB[(indx[i]+1):(indx[i+1]-1),]$Female==1)
  }
  if(i==length(indx))
  {
    indxF<- which(captive_MB[(indx[i]+1):(nrow(captive_MB)-1),]$Female==1)   
  }
  diff<- sapply(1:length(indxF), function(j)
  {
    if(j != length(indxF))
    {
      tmp<- indxF[j+1]-indxF[j]-1
    }
    if(j == length(indxF) & i!=length(indx))
    {
      tmp<- indx[i+1]-(indxF[j]+indx[i])-1
    }
    if(j == length(indxF) & i==length(indx))
    {
      tmp<- nrow(captive_MB)-(indxF[j]+indx[i])-1
    }
    return(tmp)
  })
  f<- rep(as.character(captive_MB$Row.Labels[indx[i]+indxF]), diff)
  mn<- lapply(1:length(indxF), function(j)
  {
    if(j != length(indxF) & diff[j]!=0)
    {
      tmp<- list(m=as.character(captive_MB$Row.Labels[(indx[i]+indxF[j]+1):(indx[i]+indxF[j+1]-1)]),
                 n=captive_MB$Count.of.PIT.Tag[(indx[i]+indxF[j]+1):(indx[i]+indxF[j+1]-1)])
    }
    if(j == length(indxF) & diff[j]!=0 & i!=length(indx))
    {
      tmp<- list(m=as.character(captive_MB$Row.Labels[(indx[i]+indxF[j]+1):(indx[i+1]-1)]),
                 n=captive_MB$Count.of.PIT.Tag[(indx[i]+indxF[j]+1):(indx[i+1]-1)])
    }
    if(j == length(indxF) & diff[j]!=0 & i==length(indx))
    {
      tmp<- list(m=as.character(captive_MB$Row.Labels[(indx[i]+indxF[j]+1):(nrow(captive_MB)-1)]),
                 n=captive_MB$Count.of.PIT.Tag[(indx[i]+indxF[j]+1):(nrow(captive_MB)-1)])
    }
    if(diff[j]==0)
    {
      tmp<- list(m=NA, n=NA)
    }
    return(tmp)
  })
  m<-unlist(sapply(mn, "[[", 1))
  m<- m[!is.na(m)]
  n<-unlist(sapply(mn, "[[", 2))
  n<- n[!is.na(n)]
  if(any(diff==0))
  {
    if(check[i]!=1)
    {
      f<-c(f, as.character(captive_MB$Row.Labels[indx[i]+indxF[diff==0]]))
      m<-c(m, as.character(captive_MB$Row.Labels[indx[i]+indxF[diff==0]]))
      n<-c(n, captive_MB$Count.of.PIT.Tag[indx[i]+indxF[diff==0]])
    }
    if(check[i]==1)
    {
      f<-c(f, as.character(captive_MB$Row.Labels[indx[i]+indxF[which(diff==0)-1]]))
      m<-c(m, as.character(captive_MB$Row.Labels[indx[i]+indxF[diff==0]]))
      n<-c(n, captive_MB$Count.of.PIT.Tag[indx[i]+indxF[diff==0]])
    }
  }
  out<-data.frame(year=rep(yr, length(f)), female=f, male=m, offspring=n)
  out<- out[!duplicated(out),]
  return(out)
})
dat<-do.call(rbind, dat)
captive_MB<-dat

# FIX PIT TAGS WITH ADDED SPACES
levels(captive_MB$female)<- c(levels(captive_MB$female), 
                              gsub(" ", "", as.character(captive_MB[grep(" ", as.character(captive_MB$female)),]$female)))
captive_MB[grep(" ", as.character(captive_MB$female)),]$female<- 
  gsub(" ", "", as.character(captive_MB[grep(" ", as.character(captive_MB$female)),]$female))
levels(captive_MB$male)<- c(levels(captive_MB$male), 
                              gsub(" ", "", as.character(captive_MB[grep(" ", as.character(captive_MB$male)),]$male)))
captive_MB[grep(" ", as.character(captive_MB$male)),]$male<- 
  gsub(" ", "", as.character(captive_MB[grep(" ", as.character(captive_MB$male)),]$male))
rm(dat, captive_MB_male, captive_MB_female, check, indx)

### CHECK:  WHAT IS MB CAPTIVE BROODSTOCK?
# setdiff(captive_MB$female, captive_UB$Family.Female)
# "(blank)"     "Triploid"    "UNKNOWN"
# setdiff(captive_UB$Family.Female, captive_MB$female)
# "4310187B69"  "462268474B"
# setdiff(captive_MB$male, captive_UB$Family.Male)
# "(blank)"    "mix"        "Triploid"   "UNKNOWN"    
# "224076523"  "7F7D316F73"
# setdiff(captive_UB$Family.Male, captive_MB$male)
# "41476A0462"  "411D0E2C5F"  "115675486A" "1F48472D25" "1F4A206A0D" 
# "115669294A"  "1F4A24076C"  "2224076523" "487F4B1745" "452738076E" 
# "220F0E6207"  "220E3F2578"  "1F4772396F" "0A180E0E7E"



######################
#                    #
#     CRYO BANKS     #
#                    #
######################
# FROM JEFF KALIE
## I BELIEVE THIS IS THE MASTER FOR UMOR
## BUT THERE ARE SOME DIFFERENCES WITH THE DATA SENT FROM GARRISON
cryo<- read.xlsx("./inputs/broodstock/Cryo Repository 4-24-19.xlsx",
                 "Cryo Repository")
names(cryo)[3:4]<-gsub("X", "", names(cryo)[3:4])
levels(cryo$Pit.Tag)<- c(levels(cryo$Pit.Tag), 
                         gsub(" ", "", as.character(cryo[grep(" ", as.character(cryo$Pit.Tag)),]$Pit.Tag)))
cryo[grep(" ", as.character(cryo$Pit.Tag)),]$Pit.Tag<- 
  gsub(" ", "", as.character(cryo[grep(" ", as.character(cryo$Pit.Tag)),]$Pit.Tag))


# GARRISON
cryo_Garrison<- read.xlsx("./inputs/broodstock/CRYO REPOSITORY  6-28-2018.xlsx",
                          " PIT Tag")
names(cryo_Garrison)[4:5]<-gsub("X", "", names(cryo_Garrison)[4:5])
names(cryo_Garrison)[9]<-gsub("X..", "", names(cryo_Garrison)[9])
cryo_Garrison<-cryo_Garrison[1:320,1:15]
levels(cryo_Garrison$Pit.Tag)<- c(levels(cryo_Garrison$Pit.Tag), 
                                  gsub(" ", "", as.character(cryo_Garrison[grep(" ", as.character(cryo_Garrison$Pit.Tag)),]$Pit.Tag)))
cryo_Garrison[grep(" ", as.character(cryo_Garrison$Pit.Tag)),]$Pit.Tag<- 
  gsub(" ", "", as.character(cryo_Garrison[grep(" ", as.character(cryo_Garrison$Pit.Tag)),]$Pit.Tag))

setdiff(cryo_Garrison$Pit.Tag, cryo$Pit.Tag)
setdiff(cryo$Pit.Tag, cryo_Garrison$Pit.Tag)


# MIDDLE BASIN -- IS THIS LMOR?
cryo_MB<- read.xlsx("./inputs/broodstock/Data for Landon.xlsx",
                          "Middle Basin Cryo")
names(cryo_MB)[5:6]<-gsub("X..", "", names(cryo_MB)[5:6])
 
# NEOSHO
cryo_Neosho<- read.xlsx("./inputs/broodstock/Pallid Cyro Inventory 8-18.xlsx",
                        "Sheet1")
cryo_Neosho<-cryo_Neosho[,-c(6,8:10)]
for(i in 1:ncol(cryo_Neosho))
{
  names(cryo_Neosho)[i]<-as.character(cryo_Neosho[1,i])
}
cryo_Neosho<- cryo_Neosho[2:nrow(cryo_Neosho),]
cryo_Neosho$Hatchery<-"Neosho"
cryo_MB<-(merge(cryo_MB, cryo_Neosho, by.x="PITTAG", by.y="Male ID", all=TRUE))
rm(cryo_Neosho)

intersect(cryo_MB$PITTAG, cryo$Pit.Tag)


# cryo_Neosho$Year<- sapply(strsplit(as.character(cryo_Neosho$`Collection Date`),
#                                    " "), "[[", 1)
# cryo_Neosho$Year<- as.numeric(cryo_Neosho$Year)
# 
# # NOTES:
# ## ALL NEOSHO MALE FISH INCLUDED IN MIDDLE BASIN DATA
# all(cryo_Neosho$`Male ID` %in% cryo_MB$PITTAG)
# indx<-sapply(1:nrow(cryo_Neosho), function(i)
# {
#   return(which(as.character(cryo_MB$PITTAG)==as.character(cryo_Neosho$`Male ID`)[i]))
# })
# ### BUT THE YEAR DIFFERS FOR ONE ENTRY
# which(sapply(strsplit(as.character(cryo_Neosho$`Collection Date`)," "), "[[", 1)!=as.character(cryo_MB[indx,]$Cryo.Year))
# ## NO OVERLAP OF MIDDLE BASIN DATA & GARRISON DAM NFH DATA 
# any(cryo_MB$PITTAG %in% cryo_Garrison$Pit.Tag)
# any(cryo_MB$PITTAG %in% cryo_Garrison$`2nd.Pit.Tag`)
# any(cryo_MB$PITTAG %in% cryo_Garrison$`3rd.Pit.Tag`)
# ## NOT SURE HOW TO DETERMINE WHICH MU THE FISH ARE FROM
# ### WHAT DOES SOURCE STAND FOR IN THE GARRISON DATA?
# ### NO INFO FOR GARRISON FISH  ON IF USED STOCKED OR RECAPPED
# ## I THINK WE ARE STILL WAITING FOR GAVINS POINT NFH CRYO DATA 
# ### (SEE JEFF KALIE's EMAIL)
# 
# cryo_G<- cryo_Garrison[,3:7]
# names(cryo_G)[1:3]<-c("PIT_Tag", "PIT_Tag2", "PIT_Tag3")
# cryo_G$Hatchery<-"Garrison"
# 
# cryo_N<- cryo_Neosho[,c(1,4,7)]
# names(cryo_N)[1]<- "PIT_Tag"
# cryo_N$Hatchery<- "Neosho"
# 
# cryo2<-rbind.fill(cryo_G, cryo_N)

