
setwd("C:/Users/sreynolds/Documents/GitHub/Pallid-Sturgeon-Population-Model/pallid-sturgeon-population-model")


# LOAD BEND METADATA 
bends<- read.csv("./inputs/bends_access.csv")
# REMOVE ROW NAMES
bends<-bends[,2:ncol(bends)]
# ADD IN BASIN
bends$BASIN<- ifelse(bends$B_SEGMENT %in% 1:4, "upper",
                     ifelse(bends$B_SEGMENT %in% c(7:10,13,14), "lower",
                            "other"))
bends<-bends[order(bends$LOWER_RIVER_MILE),]

# SEGMENT 7 BEND 34 IS IN THE MIDDLE OF BEND 16, CHANGING BOUNDARIES
# TO BE MOST INCLUSIVE BUT NEED TO VERIFY WHAT IS ACTUALLY THE CASE
# SEE END OF FILE FOR OTHER DISCREPANCIES
bends[c(320:324),]
end<-bends[which(bends$B_SEGMENT==7 & bends$BEND_NUM==16),]$UPPER_RIVER_MILE
bends[which(bends$B_SEGMENT==7 & bends$BEND_NUM==16),]$UPPER_RIVER_MILE<-
  bends[which(bends$B_SEGMENT==7 & bends$BEND_NUM==34),]$LOWER_RIVER_MILE
bends[which(bends$B_SEGMENT==7 & bends$BEND_NUM==34),]$LOWER_RIVER_MILE<-
  bends[which(bends$B_SEGMENT==7 & bends$BEND_NUM==34),]$UPPER_RIVER_MILE
bends[which(bends$B_SEGMENT==7 & bends$BEND_NUM==34),]$UPPER_RIVER_MILE<-
  end
rm(end)
bends[c(320:324),]

# EXTEND UPPER RIVER MILE TO MEET THE LOWER RIVER MILE OF THE NEXT 
# UPSTREAM BEND 
bends$UPPER_RIVER_MILE<- bends$UPPER_RIVER_MILE+0.1
# RECALCULATE BEND LENGTHS
bends$LENGTH.RM<- bends$UPPER_RIVER_MILE-bends$LOWER_RIVER_MILE
bends$LENGTH.RKM<- bends$LENGTH.RM*1.60934
# CALCULATE BOUNDARIES IN RKMS
bends$UPPER_RIVER_RKM<- bends$UPPER_RIVER_MILE*1.60934
bends$LOWER_RIVER_RKM<- bends$LOWER_RIVER_MILE*1.60934
# CREATE IDS
bends<-subset(bends, BASIN!="other")
bends$id<- c(1:nrow(bends))
row.names(bends)<- bends$id

# CREATE & SAVE BEND_META TO BE USED IN THE POPULATION MODEL
bend_meta<-list()
bend_meta$lower<- subset(bends, BASIN=="lower")
bend_meta$upper<- subset(bends, BASIN=="upper")
saveRDS(bend_meta, "./inputs/bend_data.rds")

# all(abs(bend_meta$lower$UPPER_RIVER_MILE[1:314]-
#           bend_meta$lower$LOWER_RIVER_MILE[2:315])<0.000000000001)
# all(abs(bend_meta$upper$UPPER_RIVER_MILE[1:155]-
#           bend_meta$upper$LOWER_RIVER_MILE[2:156])<0.000000000001)

# CREATE & SAVE A CVS FILE FOR ED B.
write.csv(bends, "./inputs/bend_rkm_data.csv", row.names = FALSE)

library(plyr)
setwd("C:/Users/sreynolds/Desktop/Population Model/Drift/Bend-Assignment-And-Shapefile/analysis")
excel_segs<-read.xlsx("./dat/PSPAP_bend_river_mile_lk.xlsx", "RPMAs And Segments")
comp<-ddply(bends, .(B_SEGMENT), summarize,
            lower=min(LOWER_RIVER_RKM),
            upper=max(UPPER_RIVER_RKM))
indx<-c()
for(i in 1:nrow(comp))
{
  indx<-c(indx, which(excel_segs$Segment==comp$B_SEGMENT[i]))
}
comp$excel_lower<- excel_segs$rkm_down[indx]
comp$excel_upper<- excel_segs$rkm_up[indx]
comp$lower_diff<-comp$lower-comp$excel_lower
comp$upper_diff<-comp$upper-comp$excel_upper
comp

# PSPAP_VOL_1.8_PG21 (PDF PG. 28)
# SEGMENTS 14-13, 13-10, 9-8, 7UB, 4-3, & 3-2 MATCH WELKER & DROBISH 
#   WITHIN 0.6 MILES
# BOUNDARY BETWEEN 10 & 9 IN WELKER & DROBISH IS 367.5 AND WE HAVE 369
# BOUNDARY BETWEEN 8 & 7 IN WELKER & DROBISH IS 750 AND WE HAVE 753.1
# WELKER & DROBISH HAVE 4LB @ 1568 & WE HAVE 1530
# BOUNDARY BETWEEN 2 & 1 IN WELKER & DROBISH IS 1760 AND WE HAVE 1761.1
# WELKER & DROBISH HAVE 1UB @ 1771.1 & WE HAVE 1766.1
