
# returns string w/o leading or trailing whitespace
# trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# LOAD INPUTS
input<- list()

# POPULATION CHARACTERISTICS
input$basin<- "Upper"

input$n_bends<- 317 # PULLED FROM GLOBAL IN APP
input$rkm<- runif(317,1,5) # PULLED FROM GLOBAL IN APP


input$maxage<- 41
input$sexratio<- 0.33 # INITIAL SEX RATIO OF NATURAL FISH
input$natural<- 200 # NUMBER OF NATURAL FISH
input$hatchery<- 600 # NUMBER OF HATCHERY FISH
input$natural_age0<- 200 # NUMBER OF NATURAL FISH
input$hatchery_age0<- 600 # NUMBER OF HATCHERY FISH
# END

# WEIGHT-LENGTH
input$a<- 0.000001
input$b<- 3
input$lw_er<-0.2
# END


# FECUDNITY
input$fec_a<- 0.1
input$fec_b<- 1.1
input$fec_er<- 0.1 # lognormal error
# END

# GROWTH
input$k<- 0.3
input$t0<- 0
input$linf<- 1400
input$vb_er<- 0.2
# END

# SEXUAL MATURITY AND RETURN TO SPAWNING
input$age_mat<- 8 # AGE AT 50% MATURITY
input$mat_k<- 0.2 # HOW FAST CURVE REACHES 1
# END


## MOVEMENT
input$spread<- 10 # SPREAD OF MOVEMENT
# prob = matrix of 300 x 300 
input$prob<- matrix(runif(input$n_bends*input$n_bends),nrow=input$n_bends,ncol=input$n_bends)
input$prob[upper.tri(input$prob)]<-0
input$prob<- input$prob/apply(input$prob,1,sum)


# SURVIVAL
# input$pr_fert<- 0.01# EGG TO E function of aggregate need to pull from function
input$phi_1<- 0.001# E TO FE
input$phi_2<- 0.001# FE TO EFL
input$phi_3<- 0.051 # EFL TO AGE-1
input$phi<- c(0.6,rep(0.92,input$maxage-1)) # AGE-1 TO AGE-2

# INITIALIZATION
input$agestructure="Approximate equilibrium"
input$adult_spatial_structure="Random"
input$age0_n_spatial_structure="Uniform"
input$age0_h_spatial_structure="Uniform"				



# STOCKING MODULE
input$stocking_amount<- 200
input$stocking_month<- 4
input$stocking_bend<-200
input$recruit_mean_length<-200 
input$recruit_length_sd<-30 
# END

## SIMULATION PARAMETERS
input$nreps<- 5
input$nyears<- 10
input$daug<- 30000 # DATA AUGMENTATION (SUPER POPULATION)
# END

tmp<- input

# INITIALIZATION
## AGE STRUCTURE
if(input$agestructure=="Approximate equilibrium")
	{
	tmp$rel_age<- cumprod(tmp$phi)/sum(cumprod(tmp$phi))
	}
if(input$agestructure=="Uniform")
	{
	pp<- rep(1,tmp$n_bends)
	tmp$rel_age<- pp/sum(pp)
	}
if(input$agestructure=="Random")
	{
	pp<- runif(tmp$n_bends)
	tmp$rel_age<- pp/sum(pp)
	}
## SPATIAL STRUCTURE
if(input$adult_spatial_structure=="Uniform")
	{
	pp<- rep(1,tmp$n_bends)
	tmp$rel_density<-pp/sum(pp) 
	}
if(input$adult_spatial_structure=="Random")
	{
	pp<- runif(tmp$n_bends)
	tmp$rel_density<-pp/sum(pp)		
	}
if(input$age0_n_spatial_structure=="Uniform")
	{
	pp<- rep(1,tmp$n_bends)
	tmp$natural_age0_rel_dens<-pp/sum(pp) 
	}
if(input$age0_n_spatial_structure=="Random")
	{
	pp<- runif(tmp$n_bends)
	tmp$natural_age0_rel_dens<-pp/sum(pp)		
	}		
if(input$age0_h_spatial_structure=="Uniform")
	{
	pp<- rep(1,tmp$n_bends)
	tmp$hatchery_age0_rel_dens<-pp/sum(pp) 
	}
if(input$age0_h_spatial_structure=="Random")
	{
	pp<- runif(tmp$n_bends)
	tmp$hatchery_age0_rel_dens<-pp/sum(pp)		
	}			

inputs=tmp
