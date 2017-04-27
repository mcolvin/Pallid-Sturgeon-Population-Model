input<-list()
## INPUT
input$lower$maxage<-41
input$upper$maxage<-60

input$lower$sexratio<-0.33
input$upper$sexratio<-0.5

input$lower$natural<- 0
input$upper$natural<- 12

input$lower$hatchery<- 0
input$upper$hatchery<- 43000

input$lower$natural_age0<- 200
input$upper$natural_age0<- 200

input$lower$hatchery_age0<- 0
input$upper$hatchery_age0<- 0

# WEIGHT-LENGTH
input$lower$a_prime<- -13.84
input$upper$a_prime<- -14.09

input$lower$b<-3.188
input$upper$b<-3.24

input$lower$lw_er<-0.1371
input$upper$lw_er<-0.165

# FECUNITY-LENGTH
input$lower$fec_a<-10.77
input$upper$fec_a<-11.26

input$lower$fec_b<-0.62
input$upper$fec_b<-0.57

input$lower$fec_er<-0.30
input$upper$fec_er<-0.39

# GROWTH
input$lower$maxLinf<-1800
input$lower$ln_Linf_mu<-6.982160
input$lower$ln_k_mu<- -2.382711
input$lower$vcv<- matrix(c(0.0894,-0.1327,-0.1327,0.3179),nrow=2,ncol=2, byrow=TRUE)

input$upper$maxLinf<-2100
input$upper$ln_Linf_mu<-7.136028770
input$upper$ln_k_mu<- -3.003764445
input$upper$vcv<- matrix(c(0.2768,-0.364,-0.364,0.6342),nrow=2,ncol=2, byrow=TRUE)




## SEXUAL MATURITY AND RETURN TO SPAWNING	
input$lower$age_mat<-8  ####fixme#### make it work for lower and upper
input$upper$age_mat<-8  ####fixme#### make it work for lower and upper

input$lower$mat_k<-0.2 	####fixme#### make it work for lower and upper
input$upper$mat_k<-0.2 	####fixme#### make it work for lower and upper

input$lower$spn_a<- -5
input$upper$spn_a<- -5

input$lower$spn_b<- 2.5
input$upper$spn_b<- 2.5

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



## STOCKING ##
### FINGERLINGS
input$stockingInput$fingerling<-c(0)
input$stockingInput$fingerling_month<-c(5)
#input$stockingInput$fingerling_mn<-c(30)
#input$stockingInput$fingerling_sd<-c(0.1)
input$stockingInput$fingerling_stocking_rkm<-c(50)
### YEARLINGS
input$stockingInput$yearling<- 0
input$stockingInput$yearling_month<- 9
input$stockingInput$yearling_mn<- 100
input$stockingInput$yearling_sd<- 15
input$stockingInput$yearling_age<- 15 # months
input$stockingInput$yearling_stocking_rkm<- 50



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
input$simulationInput$daug<- 100000
input$simulationInput$size_indices<-TRUE



