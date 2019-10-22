input<-list()
## INPUT
### USED IN INITIALIZATION OF THE MODEL
#[1] NUMBER OF LIVING FISH
input$lower$natural<- round(5.5*689+0.93*587,0)
  # 4335, matches Steffensen et al. (2017a) mean density (see 2017 AM 
  # Report for full citation) multiplied by segments 7-9 river length of 
  # 689rkm and Steffensen et al. (2014) segments 10,13,14 river length 
  # of 587rkm, rounded to the nearest fish
input$upper$natural<- 125
  # Jaeger et al. (2009) 2017 AM Report Table 3-1

input$lower$hatchery<- round(12.9*689+5.53*587,0)
  # 12134, matches Steffensen et al. (2017a)  mean density (see 2017 AM 
  # Report for full citation) multiplied by segments 7-9 river length of 
  # 689rkm and Steffensen et al. (2014) segments 10,13,14 river length 
  # of 587rkm, rounded to the nearest fish
input$upper$hatchery<- 16444
  # Rotella (2017)  2017 AM Report Table 3-1

## NUMBER OF FISH IN OUTSIDE REGIONS
### IN THE CASE OF THE LOWER BASIN THESE ARE ONLY COUNTING THE 
### POPULATION OF FISH THAT MOVE BETWEEN MO AND MS, IT IS NOT THE TOTAL 
### NUMBER OF MS FISH
input$lower$MS_natural<- 0
input$lower$MS_hatchery<- 500

input$upper$LS_natural<- 0
input$upper$UYR_natural<- 0
input$upper$LS_hatchery<- 0
input$upper$UYR_hatchery<- 0

# LIKELY WANT THIS TO LINE UP WITH SPAWNING THE YEAR BEFORE...
input$lower$natural_age0<- 200
input$upper$natural_age0<- 200
input$upper$UYR_natural_age0<- 0


input$stockingHistory<- readRDS("./inputs/StockingHistory.rds") 


input$stockingHistory$upper$survival_est<- NULL
  #data.frame(hatchery=NA,
  #           mother=NA,
  #           father=NA,
  #           year=NA,
  #           survival_to_date=NA,
  #           phi00=NA,
  #           phi0=NA,
  #           phi1=NA,
  #           phi2=NA)
input$stockingHistory$lower$survival_est<- NULL
  #data.frame(hatchery=NA,
  #           mother=NA,
  #           father=NA,
  #           year=NA,
  #           survival_to_date=NA,
  #           phi00=NA,
  #           phi0=NA,
  #           phi1=NA,
  #           phi2=NA)


#[2] GROWTH PARAMETERS
  # SOURCE:  FIT TO PSPAP DATA BY BASIN FOR THE YEARS ??-??
  # SEE <FILE NAME HERE> FOR DETAILS
input$lower$maxLinf<-1800
input$lower$ln_Linf_mu<-6.982160
input$lower$ln_k_mu<- -2.382711
input$lower$vcv<- matrix(c(0.0894,-0.1327,-0.1327,0.3179),
                         nrow=2, ncol=2, byrow=TRUE)

input$upper$maxLinf<-2100
input$upper$ln_Linf_mu<-7.136028770
input$upper$ln_k_mu<- -3.003764445
input$upper$vcv<- matrix(c(0.2768,-0.364,-0.364,0.6342), nrow=2, 
                         ncol=2, byrow=TRUE)



#[3] INITIAL LENGTH DISTRIBUTIONS
  # SOURCE:  FIT TO PSPAP DATA BY BASIN FOR THE YEARS 2016-EARLY 2018
  # FOR DETAILS SEE "./src/initialization-functions/initial-dists/
  #                     initialize-length-distributions.R" 
load("./src/initialization-functions/initialize-length-functions2.Rdata")




#[4] WEIGHT-LENGTH PARAMETERS
  # SOURCE:
input$lower$a_prime<- -13.84
input$upper$a_prime<- -14.09

input$lower$b<-3.188
input$upper$b<-3.24

input$lower$lw_er<-0.1371
input$upper$lw_er<-0.165




#[5] SEX RATIO CONVERTED TO PROBABILITY A FISH IS FEMALE
input$lower$sexratio<-0.33
  # SOURCE: 
input$upper$sexratio<-0.32 
  # SOURCE: THE 40:85 ADULT F:M RATIO ESTIMATED BY JAEGER ET AL. (2009) 
  # IN THE 2017 AM REPORT (TABLE 3-1)   



#[7] MATURATION PARAMETERS 
  # SOURCE: 
  #################################################
  #   NEEDS TO BE BASED ON BASIN-SPECIFIC DATA    #
  #################################################
input$lower$age_mat_50<-8  
input$upper$age_mat_50<-8 

input$lower$mat_k<-0.2 	
input$upper$mat_k<-0.2 	

input$lower$age_mat_min<-8  
input$upper$age_mat_min<-8 

input$lower$age_mat_max<-16  
input$upper$age_mat_max<-16 





#[8] TIME SINCE SPAWNING PARAMETERS
  # SOURCE: 
  #################################################
  #   NEEDS TO BE BASED ON BASIN-SPECIFIC DATA    #
  #################################################
input$lower$spn_a<- -5  
input$upper$spn_a<- -5  

input$lower$spn_b<- 2.5 
input$upper$spn_b<- 2.5 



# FECUNITY-LENGTH
  # SOURCE:
input$lower$fec_a<-10.77
input$upper$fec_a<-11.26

input$lower$fec_b<-0.62
input$upper$fec_b<-0.57

input$lower$fec_er<-0.30
input$upper$fec_er<-0.39
    


## SURVIVAL
input$lower$pr_embryo<- 0.0001
input$upper$pr_embryo<- 0.0001
#input$lower$pr_embryo<- 0.001
#input$upper$pr_embryo<- 0.001
  # SOURCE: WAG
  # MOTIVATION: LIKELY VERY SMALL

input$lower$phi_embryo<- 0.0001
input$upper$phi_embryo<- 0.0001
#input$lower$phi_embryo<- 0.1
#input$upper$phi_embryo<- 0.1
  # SOURCE: WAG
  # MOTIVATION: LIKELY VERY SMALL

input$lower$phi_free_embryo<- 0.0001
input$upper$phi_free_embryo<- 0.0001
#input$lower$phi_free_embryo<- 0.9
#input$upper$phi_free_embryo<- 0.9
  # SOURCE: WAG
  # MOTIVATION: LIKELY VERY SMALL

input$lower$phi_age0_mean<-0.0001
input$upper$phi_age0_mean<-0.0001
#input$lower$phi_age0_mean<-0.9
#input$upper$phi_age0_mean<-0.9
  # SOURCE: WAG
  # MOTIVATION: LIKELY VERY SMALL

input$lower$phi_age0_er<-0.01
input$upper$phi_age0_er<-0.01
  # SOURCE: WAG
  # MOTIVATION: GIVES A LARGE RSD: 100  

input$lower$phi0_Hcap_mean<-0.5
input$upper$phi0_Hcap_mean<-0.5

input$lower$phi0_Hcap_er<-0.1
input$upper$phi0_Hcap_er<-0.1

input$lower$phi0_dwnstrm<-input$lower$phi_age0_mean #MISSISSIPPI RIVER
input$upper$phi0_dwnstrm<-input$upper$phi_age0_mean*0.01 #LAKE SAKAKAWEA
input$upper$phi0_upper_YR<-input$upper$phi_age0_mean #ABOVE INTAKE

input$lower$phi_age1_mean<-0.68
  # SOURCE:
input$upper$phi_age1_mean<-0.95
  # SOURCE:

input$lower$phi_age1_er<-0.1
input$upper$phi_age1_er<-0.1

input$lower$phi1_dwnstrm<-input$lower$phi_age1_mean #MISSISSIPPI RIVER
input$upper$phi1_dwnstrm<-input$upper$phi_age1_mean #LAKE SAKAKAWEA
input$upper$phi1_upper_YR<-input$upper$phi_age1_mean #ABOVE INTAKE

input$lower$phi_age2_mean<-0.92
  # SOURCE:
input$upper$phi_age2_mean<-0.95
  # SOURCE:

input$lower$phi_age2_er<-0.01
input$upper$phi_age2_er<-0.01

input$lower$phi2_dwnstrm<-input$lower$phi_age2_mean #MISSISSIPPI RIVER
input$upper$phi2_dwnstrm<-input$upper$phi_age2_mean #LAKE SAKAKAWEA
input$upper$phi2_upper_YR<-input$upper$phi_age2_mean #ABOVE INTAKE

input$lower$maxage<-41  #OKAY?
  # SOURCE:  Keenlyne et al. (1992) 
input$upper$maxage<-60
  # SOURCE:

### DRIFT SURVIVAL (NON-SPATIAL)
input$lower$p_retained<- 0.5
input$upper$p_retained<- 0.5

## RECRUITMENT
  #################################################
  #   NEEDS TO BE BASED ON BASIN-SPECIFIC DATA    #
  #################################################
input$lower$recruit_mean_length <- 200    
input$upper$recruit_mean_length <- 200    

input$lower$recruit_length_sd <- 25  
input$upper$recruit_length_sd <- 25  



## STOCKING PROGRAM ##
### UPPER BASIN
#### BROODSTOCK DATA
input$stockingInput$upper$broodstock$breeder_no<-4
input$stockingInput$upper$broodstock$cp<-0.5
tmp<-readRDS("./inputs/BroodstockBank.rds")
input$stockingInput$upper$broodstock$bank_F<-tmp$upper$bankF
input$stockingInput$upper$broodstock$bank_M<-tmp$upper$bankM
#input$stockingInput$upper$broodstock$BROOD_1<-
#  data.frame(mother=paste0("TAG", 11:14),
#             father=paste0("TAG", 15:18),
#             hatchery=sample(c("Neosho", "Gavins"), 4, replace=TRUE),
#             no_available=runif(4,1000,5000))
  # NUMBER OF AGE-1's AVAILABLE FOR STOCKING BY FAMILY LOT
  ## INITIALIZE THESE LASAT THREE IN PROCESS_INPUTS.R USING STOCKING 
  ## HISTORY AND BACK CALCULATION
#### FINGERLINGS
input$stockingInput$upper$fingerling<-data.frame(month=c(9),
                                                 stocking_rkm=c(2500),
                                                 stocking_no=c(0),
                                                 #hatchery="Neosho",
                                                 age=c(3), # months
                                                 length_mn=c(50),
                                                 length_sd=c(5),
                                                 phi0_mn=c(0.25^(4/3)),
                                                 phi0_sd=c(0.1)) 
#### YEARLINGS
input$stockingInput$upper$yearling<-data.frame(month=c(9),
                                               stocking_rkm=c(2500),
                                               stocking_no=c(0),
                                               #hatchery="Neosho",
                                               age=c(15), # months
                                               length_mn=c(100),
                                               length_sd=c(15),
                                               phi_mn=c(0.25),
                                               phi_sd=c(0.1))
### LOWER BASIN
#### BROODSTOCK DATA
input$stockingInput$lower$broodstock$breeder_no<-4
input$stockingInput$lower$broodstock$cp<-0.5
input$stockingInput$lower$broodstock$bank_F<-tmp$lower$bankF
input$stockingInput$lower$broodstock$bank_M<-tmp$lower$bankM
rm(tmp)
#### FINGERLINGS
input$stockingInput$lower$fingerling<-data.frame(month=c(9),
                                                 stocking_rkm=c(50),
                                                 stocking_no=c(0),
                                                 #hatchery=c("Neosho"),
                                                 age=c(3), # months
                                                 length_mn=c(50),
                                                 length_sd=c(5),
                                                 phi0_mn=c(0.25^(4/3)),
                                                 phi0_sd=c(0.1)) 
#### YEARLINGS
input$stockingInput$lower$yearling<-data.frame(month=c(9),
                                               stocking_rkm=c(50),
                                               stocking_no=c(0),
                                               #hatchery=c("Neosho"),
                                               age=c(15), # months
                                               length_mn=c(100),
                                               length_sd=c(15),
                                               phi_mn=c(0.25),
                                               phi_sd=c(0.1))
## END STOCKING PROGRAM INPUTS ##



## SPATIAL INPUTS ##
### SPATIAL STRUCTURE
input$spatialInput$adult_spatial_structure<- "Uniform" # "Emperical"
input$spatialInput$drift_spatial_structure<- "Emperical" # "Uniform"
input$spatialInput$age0_n_spatial_structure<- "Uniform"# "Emperical"
input$spatialInput$age0_h_spatial_structure<- "Uniform"# "Emperical"
### SPAWNING HOTSPOTS
input$spatialInput$upper$spn_bends<- c(154, 156, 157) 
input$spatialInput$lower$spn_bends<- c(316)
### DRIFT DYNAMICS   <!-- fix: improve inputs -->
#### FOR "EMPERICAL" STRUCTURE:
input$spatialInput$discharge_structure<- "Probabilistic"  #"Fixed"
input$spatialInput$discharge_scenarios<- c("10pct", "30pct", "50pct", "70pct", "90pct")
input$spatialInput$drift_matrices<-readRDS("./inputs/drift_matrices.rds")
##### FOR "PROBABILISTIC":
input$spatialInput$upper$discharge_probs<- c(0.1, 0.2, 0.4, 0.2, 0.1) 
input$spatialInput$lower$discharge_probs<- c(0.1, 0.2, 0.4, 0.2, 0.1)
##### FOR "FIXED":
input$spatialInput$upper$discharge<- NULL
input$spatialInput$lower$discharge<- NULL
#### FOR "UNIFORM" STRUCTURE: 
input$spatialInput$upper$p_retained<- rep(0.5, nrow(bend_meta$upper))
input$spatialInput$lower$p_retained<- rep(0.5, nrow(bend_meta$lower))
input$spatialInput$upper$LYR$p_retained<- 0.5
input$spatialInput$upper$UYR$p_passage<- 0.4
input$spatialInput$upper$UYR$p_retained_given_passage<- 0.9
### EMIGRATION AND IMMIGRATION
  ## DO WE WANT THIS TO BE AGE OR STAGE SPECIFIC?
  ## CAN MAKE LOCATION SPECIFIC BY MAKING EACH LIST ENTRY A VECTOR 
  ## OF LENGTH N_BENDS
input$spatialInput$upper$p_upper_YR <-list(to=0.05, from=0.5) #ABOVE INTAKE
input$spatialInput$upper$p_dwnstrm <- list(to=0.001, from=0.99) #LAKE SAKAKAWEA
input$spatialInput$lower$p_dwnstrm <- list(to=0.05, from=0.5) #MISSISSIPPI RIVER
### SPAWNING MIGRATION
input$spatialInput$upper$UYR$spawn <- TRUE
input$spatialInput$upper$UYR$p_YR_spn_passage <- 0.3
input$spatialInput$upper$UYR$p_spn_mig <- 0.1
input$spatialInput$upper$p_spn_mig <- 1
input$spatialInput$lower$p_spn_mig <- 0.3
  ## TO ALLOW SPAWNING MIGRATIONS FROM THE MS TO THE MO WE WOULD NEED TO
  ## KEEP TRACK OF SPAWNING STATUS OF MS FISH -- THIS IS ONLY UPDATED 
  ## ONCE A YEAR, SO MAY NOT BE TOO PROBLEMATIC
  ## OR
  ## WE COULD DO A BATCH DESIGNTATION WHERE APPROXIMATELY 1/3 OF THE 
  ## POP AGE A_MAT_MIN AND OLDER MAKES A SPAWNING RUN TO THE MO SO THAT
  ##      N_MIGRATE~BINOM(1/3, N_A_MAT_MIN+)
  ##      SAMPLE N_MIGRATE FROM THE POP WITH EQUAL PROBS (OR PERHAPS 
  ##        AGE ADJUSTED PROBS)
  ##      ASSUME EACH OF THESE FISH IS MATURE AND READY TO SPAWN    
  ## WOULD THE ASSUMPTION THEN BE THAT THESE FISH RETURN TO THE MS AFTER
  ## THEIR SPAWNINGN RUN?
## END SPATIAL INPUTS ##



## SIMULATION INPUTS
input$simulationInput$nreps<- 10
input$simulationInput$startYear<- 2019
input$simulationInput$nyears<- 50
input$simulationInput$daug_H<- 100000
input$simulationInput$daug_N<- 100000
input$simulationInput$size_indices<-TRUE



## GENETICS ##
input$catchHistory<- readRDS("./inputs/CatchHistory.rds") 
### UPPER BASIN
# input$geneticsInput$upper$fingerling<- data.frame(hatchery=c("Neosho"),
#                                                   mother=c("TAG1"),
#                                                   father=c("TAG2"),
#                                                   no_offspring=c(50))
# input$geneticsInput$upper$yearling<- data.frame(hatchery=c("Neosho"),
#                                                 mother=c("TAG3"),
#                                                 father=c("TAG4"),
#                                                 no_offspring=c(50))
# input$geneticsInput$upper$age0<- data.frame(hatchery=c("Neosho"),
#                                             mother=c("TAG5"),
#                                             father=c("TAG6"),
#                                             no_stocked=c(0),
#                                             yr_stocked=input$simulationInput$startYear-1,
#                                             age_stocked=0)
# input$geneticsInput$upper$age1plus<- data.frame(hatchery=c("Neosho"),
#                                                 mother=c("TAG7"),
#                                                 father=c("TAG8"),
#                                                 no_stocked=c(20000),
#                                                 yr_stocked=c(2012),
#                                                 age_stocked=c(1))
### LOWER BASIN
# input$geneticsInput$lower$fingerling<- data.frame(hatchery=c("Neosho"),
#                                                   mother=c("TAG1"),
#                                                   father=c("TAG2"),
#                                                   no_offspring=c(50))
# input$geneticsInput$lower$yearling<- data.frame(hatchery=c("Neosho"),
#                                                 mother=c("TAG3"),
#                                                 father=c("TAG4"),
#                                                 no_offspring=c(50))
# input$geneticsInput$lower$age0<- data.frame(hatchery=c("Neosho"),
#                                             mother=c("TAG5"),
#                                             father=c("TAG6"),
#                                             no_stocked=c(0),
#                                             yr_stocked=input$simulationInput$startYear-1,
#                                             age_stocked=0)
# input$geneticsInput$lower$age1plus<- data.frame(hatchery=c("Neosho"),
#                                                 mother=c("TAG7"),
#                                                 father=c("TAG8"),
#                                                 no_stocked=c(20000),
#                                                 yr_stocked=c(2012),
#                                                 age_stocked=c(1))
## END GENETICS INPUTS ##

#input$lower$hatchery_age0<- sum(input$geneticsInput$lower$age0$no_stocked)
#input$upper$hatchery_age0<- sum(input$geneticsInput$upper$age0$no_stocked)
