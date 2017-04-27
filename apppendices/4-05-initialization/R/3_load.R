

com8<- odbcConnectAccess2007("C:/Users/mcolvin/Documents/projects/analysis/Data/Pallid Sturgeon/Data/pallids.accdb")
dat<- sqlFetch(com8,"Pallid-Sturgeon-Length-Weight-Data")
stocked<- sqlFetch(com8,"stocked-fish")
recaptured<- sqlFetch(com8,"Recaptured Pallids")

manunits<- readOGR("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/GIS", 
    "PallidSturgeon_ManagementUnits",
	verbose=FALSE)

# LOAD FUNCTIONS
len_ini_low_hatchery_nospace<-readRDS("./output/function_len_ini_low_hatchery_nospace.rds")
len_ini_low_natural_nospace<-readRDS("./output/function_len_ini_len_ini_low_natural_nospace.rds")
len_ini_upp_hatchery_nospace<-readRDS("./output/function_len_ini_upp_hatchery_nospace.rds")
len_ini_upp_natural_nospace<-readRDS("./output/function_len_ini_upp_natural_nospace.rds")
length_initialization<-readRDS("./output/function_length_initialization.rds")




