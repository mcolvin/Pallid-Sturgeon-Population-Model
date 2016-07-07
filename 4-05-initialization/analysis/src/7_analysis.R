

# DENSITY FUNCTION FOR LENGTH NO SPAECE

x<-list(
	lower_hatchery=na.omit(dat[dat$basin=="lower" & 
		dat$year==2015 & dat$origin=="hatchery",]$length),
	lower_natural=na.omit(dat[dat$basin=="lower" & 
		dat$year==2015 & dat$origin=="natural",]$length),
	upper_hatchery=na.omit(dat[dat$basin=="upper" & 
		dat$year==2015 & dat$origin=="hatchery",]$length),
	upper_natural=na.omit(dat[dat$basin=="upper" & 
		dat$year==2015 & dat$origin=="natural",]$length))	


len_ini_low_hatchery_nospace<- DiscreteDistribution(x$lower_hatchery)
len_ini_low_natural_nospace<- DiscreteDistribution(x$lower_natural)
len_ini_upp_hatchery_nospace<- DiscreteDistribution(x$upper_hatchery)
len_ini_upp_natural_nospace<- DiscreteDistribution(x$upper_natural)


length_initialization<- function(n=10, basin="lower",origin=1, spatial=FALSE)
	{# A FUNCTION TO INITIALIZE LENGTHS OF INDIVIDUAL FISH
	# origin [0 for natural, 1 for hatchery]
	if(tolower(basin)=="lower" & spatial==FALSE)
		{
		out<- r(len_ini_low_hatchery_nospace)(n)*origin + # hatchery
			r(len_ini_low_natural_nospace)(n)*(1-origin) # natural
		}
	if(tolower(basin)=="upper" & spatial==FALSE)
		{
		out<- r(len_ini_upp_hatchery_nospace)(n)*origin + # hatchery
			r(len_ini_upp_natural_nospace)(n)*(1-origin) # natural
		}
	# SPATIAL COMPONENT
	if(tolower(basin)=="lower" & spatial==TRUE )
		{
		####fixme####
		out<- r(len_ini_low_hatchery_nospace)(n)*origin + # hatchery
			r(len_ini_low_natural_nospace)(n)*(1-origin) # natural
		}
	if(tolower(basin)=="upper" & spatial==TRUE )
		{
		####fixme####
		out<-  r(len_ini_upp_hatchery_nospace)(n)*origin + # hatchery
			r(len_ini_upp_natural_nospace)(n)*(1-origin) # natural

		}
	}
	
# SAVE FUNCTIONS TO CALL FROM PALLID STURGEON MODEL
# SAVE FUNCTIONS TO PALLID STURGEON INTIIALIZATION FUNCTIONS FOLDER
#file.remove(...)

fun_out<- c("len_ini_low_hatchery_nospace","len_ini_low_natural_nospace",
"len_ini_upp_hatchery_nospace","len_ini_upp_natural_nospace","length_initialization")
save(list=fun_out,
	file="./output/initialize_length_functions.Rdata")
save(list=fun_out,
	file="C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Pallid-Sturgeon-Population-Model/input-functions/initialization-functions/initialize_length_functions.Rdata")
	