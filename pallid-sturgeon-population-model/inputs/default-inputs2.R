input<-list()
## INPUT
### USED IN INITIALIZATION OF THE MODEL
#[1] NUMBER OF LIVING FISH
input$lower$natural<- round(5.7*689+0.93*587,-2)
  # 4500, matches PSPSP Report density multiplied by segments 7-9 river 
  # length of 689rkm and segments 10,13,14 river length of 587rkm,
  # rounded to the nearest hundred
input$upper$natural<- round(0.28*355,-2)
  # 100, matches PSPSP Report density multiplied by UB river length of 
  # 355rkms and rounded to the nearest hundred

input$lower$hatchery<- round(32.3*689+5.53*587,-2)
  # 25500, matches PSPSP Report density multiplied by segments 7-9 river 
  # length of 689rkm and segments 10,13,14 river length of 587rkm,
  # rounded to the nearest hundred
input$upper$hatchery<- round(91.57*355,-2) 
  # 32500, matches PSPSP Report density multiplied by UB river length of
  # 355rkms and rounded to the nearest hundred


input$lower$natural_age0<- 200
input$upper$natural_age0<- 200

input$lower$hatchery_age0<- 0
input$upper$hatchery_age0<- 0


#[2] GROWTH PARAMETERS
input$lower$maxLinf<-1800
input$lower$ln_Linf_mu<-6.982160
input$lower$ln_k_mu<- -2.382711
input$lower$vcv<- matrix(c(0.0894,-0.1327,-0.1327,0.3179),nrow=2,ncol=2, byrow=TRUE)

input$upper$maxLinf<-2100
input$upper$ln_Linf_mu<-7.136028770
input$upper$ln_k_mu<- -3.003764445
input$upper$vcv<- matrix(c(0.2768,-0.364,-0.364,0.6342),nrow=2,ncol=2, byrow=TRUE)



#[3] INITIAL LENGTH DISTRIBUTIONS
load("./src/initialization-functions/initialize-length-functions.Rdata")



#[4] WEIGHT-LENGTH PARAMETERS
input$lower$a_prime<- -13.84
input$upper$a_prime<- -14.09

input$lower$b<-3.188
input$upper$b<-3.24

input$lower$lw_er<-0.1371
input$upper$lw_er<-0.165



#[5] SEX RATIO
input$lower$sexratio<-0.33  #MATCH TO PSPAP???
input$upper$sexratio<-0.5



#[7] MATURATION PARAMETERS  
input$lower$age_mat_50<-8  ####fixme#### make it work for lower and upper
input$upper$age_mat_50<-8  ####fixme#### make it work for lower and upper

input$lower$age_mat_min<-8  ####fixme#### make it work for lower and upper
input$upper$age_mat_min<-8  ####fixme#### make it work for lower and upper

input$lower$mat_k<-0.2 	####fixme#### make it work for lower and upper
input$upper$mat_k<-0.2 	####fixme#### make it work for lower and upper



#[8] TIME SINCE SPAWNING PARAMETERS
input$lower$spn_a<- -5  ####fixme#### make it work for lower and upper
input$upper$spn_a<- -5  ####fixme#### make it work for lower and upper

input$lower$spn_b<- 2.5 ####fixme#### make it work for lower and upper
input$upper$spn_b<- 2.5 ####fixme#### make it work for lower and upper



# FECUNITY-LENGTH
input$lower$fec_a<-10.77
input$upper$fec_a<-11.26

input$lower$fec_b<-0.62
input$upper$fec_b<-0.57

input$lower$fec_er<-0.30
input$upper$fec_er<-0.39


## SURVIVAL
input$lower$pr_embryo<- 0.0001
input$upper$pr_embryo<- 0.0001

input$lower$phi_embryo<- 0.0001
input$upper$phi_embryo<- 0.0001

input$lower$phi_free_embryo<- 0.0001
input$upper$phi_free_embryo<- 0.0001

input$lower$phi_age0_mean<-0.0001
input$upper$phi_age0_mean<-0.0001

input$lower$phi_age0_er<-0.01
input$upper$phi_age0_er<-0.01

input$lower$phi_age1_mean<-0.68
input$upper$phi_age1_mean<-0.95

input$lower$phi_age1_er<-0.1
input$upper$phi_age1_er<-0.1

input$lower$phi_age2_mean<-0.92
input$upper$phi_age2_mean<-0.95

input$lower$phi_age2_er<-0.01
input$upper$phi_age2_er<-0.01

input$lower$maxage<-41  #OKAY?
input$upper$maxage<-60

## RECRUITMENT
input$lower$recruit_mean_length <- 200    #FIX!!!
input$upper$recruit_mean_length <- 200    #FIX!!!

input$lower$recruit_length_sd <- 25  #FIX!!!
input$upper$recruit_length_sd <- 25  #FIX!!!



## STOCKING ##
### OLD
#### FINGERLINGS
input$stockingInput$fingerling<-c(0)
input$stockingInput$fingerling_month<-c(5)
input$stockingInput$fingerling_mn<- 100
input$stockingInput$fingerling_sd<- 15
input$stockingInput$fingerling_age<- 3 # months
input$stockingInput$fingerling_stocking_rkm<-c(50)
#### YEARLINGS
input$stockingInput$yearling<- 0
input$stockingInput$yearling_month<- 9
input$stockingInput$yearling_mn<- 100
input$stockingInput$yearling_sd<- 15
input$stockingInput$yearling_age<- 15 # months
input$stockingInput$yearling_stocking_rkm<- 50
### UPPER BASIN
#### FINGERLINGS
input$stockingInput$upper$fingerling<-c(0)
input$stockingInput$upper$fingerling_month<-c(5)
input$stockingInput$upper$fingerling_mn<- 100
input$stockingInput$upper$fingerling_sd<- 15
input$stockingInput$upper$fingerling_age<- 3 # months
input$stockingInput$upper$fingerling_stocking_rkm<-c(50)
#### YEARLINGS
input$stockingInput$upper$yearling<- 0
input$stockingInput$upper$yearling_month<- 9
input$stockingInput$upper$yearling_mn<- 100
input$stockingInput$upper$yearling_sd<- 15
input$stockingInput$upper$yearling_age<- 15 # months
input$stockingInput$upper$yearling_stocking_rkm<- 50
### LOWER BASIN
#### FINGERLINGS
input$stockingInput$lower$fingerling<-c(0)
input$stockingInput$lower$fingerling_month<-c(5)
input$stockingInput$lower$fingerling_mn<- 100
input$stockingInput$lower$fingerling_sd<- 15
input$stockingInput$lower$fingerling_age<- 3 # months
input$stockingInput$lower$fingerling_stocking_rkm<-c(50)
#### YEARLINGS
input$stockingInput$lower$yearling<- 0
input$stockingInput$lower$yearling_month<- 9
input$stockingInput$lower$yearling_mn<- 100
input$stockingInput$lower$yearling_sd<- 15
input$stockingInput$lower$yearling_age<- 15 # months
input$stockingInput$lower$yearling_stocking_rkm<- 50
## END STOCKING INPUTS ##


## SPATIAL INPUTS ##
### SPATIAL STRUCTURE
input$spatialInput$adult_spatial_structure<- "Uniform" # "Emperical"
input$spatialInput$age0_n_spatial_structure<- "Uniform"# "Emperical"
input$spatialInput$age0_h_spatial_structure<- "Uniform"# "Emperical"
### SPAWNING HOTSPOTS
## END SPATIAL INPUTS ##



## SIMULATION INPUTS
input$simulationInput$nreps<- 10
input$simulationInput$startYear<- 2015
input$simulationInput$nyears<- 50
input$simulationInput$daug_H<- 100000
input$simulationInput$daug_N<- 100000
input$simulationInput$size_indices<-TRUE



