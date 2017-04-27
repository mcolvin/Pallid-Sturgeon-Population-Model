

# DENSITY FUNCTION DATA FOR LENGTH NO SPACE
year<- 2015 # CHANGE THIS TO UPDATE THE LENGTHS USED TO GENERATE THE LENGTH DISTRIBUTIONS

x<-list(
	lower_hatchery=na.omit(dat[dat$basin=="lower" & 
		dat$year==year & dat$origin=="hatchery",]$length),
	lower_natural=na.omit(dat[dat$basin=="lower" & 
		dat$year==year & dat$origin=="natural",]$length),
	upper_hatchery=na.omit(dat[dat$basin=="upper" & 
		dat$year==year & dat$origin=="hatchery",]$length),
	upper_natural=na.omit(dat[dat$basin=="upper" & 
		dat$year==year & dat$origin=="natural",]$length))	

len_ini_low_hatchery_nospace<- DiscreteDistribution(x$lower_hatchery)
len_ini_low_natural_nospace<- DiscreteDistribution(x$lower_natural)
len_ini_upp_hatchery_nospace<- DiscreteDistribution(x$upper_hatchery)
len_ini_upp_natural_nospace<- DiscreteDistribution(x$upper_natural)
### NOTE: FULL INITIALIZATION FUNCTION IS IN THE MODEL/SRC/FUNCTIONS.R

# SAVE FUNCTIONS TO CALL FROM PALLID STURGEON MODEL
# SAVE FUNCTIONS TO PALLID STURGEON INTIIALIZATION FUNCTIONS FOLDER
#file.remove(...)

fun_out<- c("len_ini_low_hatchery_nospace","len_ini_low_natural_nospace",
	"len_ini_upp_hatchery_nospace","len_ini_upp_natural_nospace")
save(list=fun_out,file="./output/initialize_length_functions.Rdata")
save(list=fun_out,file="C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Pallid-Sturgeon-Population-Model/input-functions/initialization-functions/initialize_length_functions.Rdata")
	