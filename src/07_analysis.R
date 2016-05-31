

## INPUTS
input<-list()

input$basin<-"Lower"
input$maxage<-41
input$sexratio<-c(0.33,0.5)
input$natural<- c(200,200)
input$hatchery<- c(200,200)
input$natural_age0<- c(200,200)
input$hatchery_age0<- c(200,200)

# WEIGHT-LENGTH
input$a_prime<-c(-13.84,-14.09)
input$b<-c(3.188,3.24)
input$lw_er<-c(0.1371,0.165)

# FECUNITY-LENGTH
input$fec_a<-c(10.77,11.26)
input$fec_b<-c(0.62,0.57)
input$fec_er<-c(0.30,0.39)

# GROWTH
## PROGRAMMED INTO INITIILAIZATION FUNCTION

## SEXUAL MATURITY AND RETURN TO SPAWNING	
input$age_mat<- c(8,8)  ####fixme#### make it work for lower and upper
input$mat_k<-c(0.2,0.2) ####fixme#### make it work for lower and upper
input$spn_a<-c(-17.5,17.5)
input$spn_B<- c(0.35,0.35)

## SURVIVAL
input$phi_age0_mean<-c(0.001,0.001)
input$phi_age0_er<-c(0.01,0.001)
input$phi_age1_mean<-c(0.68,0.68)
input$phi_age1_er<-c(0.1,0.1)
input$phi_age2_mean<-c(0.92,0.92)
input$phi_age2_er<-c(0.01,0.01)
input$recruitment<- FALSE
input$spatial<- FALSE

## STOCKING
### FINGERLINGS
input$fingerling<-c(200)
input$fingerling_month<-c(5)
#input$fingerling_mn<-c(30)
#input$fingerling_sd<-c(0.1)
input$fingerling_stocking_rkm<-c(50)
### YEARLINGS
input$yearling<- 200
input$yearling_month<- 9
input$yearling_mn<- 100
input$yearling_sd<- 15
input$yearling_age<- 15 # months
input$yearling_stocking_rkm<- 50


## SIMULATION INPUTS
input$nreps<- 10
input$nyears<- 50
input$daug<- 30000

input$agestructure<-"Approximate equilibrium"
input$adult_spatial_structure<- "Uniform" # "Emperical"
input$age0_n_spatial_structure<- "Uniform"# "Emperical"
input$age0_h_spatial_structure<- "Uniform"# "Emperical"

# SPAWNING HOTSPOTS




# PROCESS INPUTS FOR INITIALIZATON AND SIMULATION
inputs<-modelInputs(input=input)




out<- sim(inputs=inputs)

