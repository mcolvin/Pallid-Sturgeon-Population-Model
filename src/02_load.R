
# returns string w/o leading or trailing whitespace
# trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# LOAD INPUTS
input<- list()
input$n_bends<- 317
input$rkm<- runif(317,1,5)
pp<- runif(input$n_bends)
input$rel_density<-pp/sum(pp)  # A VECTOR OF RELATIVE DENSITIES FOR EACH BEND

## INITIAL ABUNDANCES
input$natural<- 200 # NUMBER OF NATURAL FISH
input$hatchery<- 600 # NUMBER OF HATCHERY FISH
input$natural_age0<- 200 # NUMBER OF NATURAL FISH
pp<- runif(input$n_bends)
input$natural_age0_rel_dens<-  pp/sum(pp)
input$hatchery_age0<- 600 # NUMBER OF HATCHERY FISH
pp<- runif(input$n_bends)
input$hatchery_age0_rel_dens<- pp/sum(pp)

## MOVEMENT
input$spread<- 10 # SPREAD OF MOVEMENT

# prob = matrix of 300 x 300 
input$prob<- matrix(runif(300*300),nrow=300,ncol=300)
input$prob[upper.tri(input$prob)]<-0
input$prob<- input$prob/apply(input$prob,1,sum)

input$recruit_mean_length<-200
input$recruit_length_sd<-30


input$phi<- 0.92
input$sample_month<- 8
input$nprim<- 5
input$nsec<-4
input$p<- 0.4 # CAPTURE PROBABILITY
input$maxage<- 41

input$sexratio<- 0.33 # INITIAL SEX RATIO OF NATURAL FISH
input$age_mat<- 8 # AGE AT 50% MATURITY
input$mat_k<- 0.2 # HOW FAST CURVE REACHES 1

# SURVIVAL
input$pr_fert<- 0.01# EGG TO E
input$phi_1<- 0.001# E TO FE
input$phi_2<- 0.001# FE TO EFL
input$phi_3<- 0.001 # EFL TO AGE-0
input$phi_4<- 0.051 # AGE-0 TO AGE-1
input$phi<- c(0.6,rep(0.92,input$maxage-1)) # AGE-1 TO AGE-2



# AGE AND STUFF
input$rel_age<- cumprod(input$phi)/sum(cumprod(input$phi))

# GROWTH
input$k<- 0.3
input$t0<- 0
input$linf<- 1400
input$vb_er<- 0.2

# LENGTH-WEIGHT
input$a<- 0.000001
input$b<- 3
input$lw_er<-0.2

# FECUDNITY
input$fec_a<- 0.1
input$fec_b<- 1.1
input$fec_er<- 0.1 # lognormal error

## SIMULATION PARAMETERS
input$nreps<- 5
input$nyears<- 50
input$daug<- 30000 # DATA AUGMENTATION (SUPER POPULATION)
