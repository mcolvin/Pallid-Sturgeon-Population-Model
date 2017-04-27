

com8<- odbcConnectAccess2007("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Data/pallids.accdb")
dat<- sqlFetch(com8,"Pallid-Sturgeon-Length-Weight-Data")
stocked<- sqlFetch(com8,"stocked-fish")


# BASE GIS COVERAGES
#hucs<- readOGR("C:/Users/mcolvin/Documents/projects/gis coverages", "hucs00p020")
#states<- readOGR("C:/Users/mcolvin/Documents/projects/gis coverages", "statep010")

# MISSOURI RIVER SPECIFIC COVERAGES
#lmo_miss<- readOGR("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Study Area/dat", "Lower_Mo_River_And_Miss")
#lmo<- readOGR("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Study Area/dat", "Lower_Mo_River_No_Miss")
manunits<- readOGR("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Study Area/dat", "Pallid_Sturgeon_management_units",
	verbose=FALSE)
#umo_yellowstone<- readOGR("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Study Area/dat", "upper_mo_river")
#states<- readOGR("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Study Area/analysis/dat", "states")
#wb<- readOGR("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Study Area/dat", "water_bodies_mo_river")
#umo<- readOGR("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Study Area/dat", "umo_to_intake1")


load("./output/out_lower-mod1.RData")
m1<- out_lower
load("./output/out_lower-mod1-gomp.RData")
m2<- out_lower
load("./output/out_lower-mod2.RData")
m3<- out_lower
load("./output/out_lower-mod2-gomp.RData")
m4<- out_lower
load("./output/out_upper-mod1.RData")
m5<- out_upper
load("./output/out_upper-mod1-gomp.RData")
m6<- out_upper
load("./output/out_upper-mod2.RData")
m7<- out_upper
load("./output/out_upper-mod2-gomp.RData")
m8<- out_upper

# VBGF with correlated k and linf
load("./output/out_lower-mod3.RData")
low_cor_vbgf<- out_lower
load("./output/out_upper-mod3.RData")
upp_cor_vbgf<- out


