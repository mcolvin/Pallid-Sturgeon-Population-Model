
# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# LOAD INPUTS

	
input<- list()
input$n_bends<- 317
input$rkm<- runif(317,1,5)
input$den<- 0.2
input$den_sd<- 0.3
input$spread<- 0 # SPREAD: 0 IS RANDOM MIXING
input$ini_adults<- 100
input$nyears<- 50
input$daug<- 500000 # DATA AUGMENTATION (SUPER POPULATION)
input$phi<- 0.92
input$sample_month<- 8
input$nprim<- 5
input$nsec<-4
input$p<- 0.4 # CAPTURE PROBABILITY
input$maxage<- 41
input$natural<- 200
input$hatchery<- 600
input$sexratio<- 0.33
input$age_mat<- 8 # AGE AT 50% MATURITY
input$mat_k<- 0.7 # HOW FAST CURVE REACHES 1

# SURVIVALS
input$pr_fert<- 0.01# EGG TO E
input$phi_1<- 0.001# E TO FE
input$phi_2<- 0.001# FE TO EFL
input$phi_3<- 0.001 # EFL TO AGE-0
input$phi_4<- 0.051 # AGE-0 TO AGE-1
input$phi_5<- 0.6 # AGE-1 TO AGE-2
input$phi_6<- 0.92 # AGE-2+

input$k<- 0.3
input$t0<- 0
input$linf<- 1400
input$vb_er<- 0.2
input$fec_a<- 0.1
input$fec_b<- 1.1
input$fec_er<- 0.1 # lognormal error


## SIMULATION PARAMETERS
input$nreps<- 5




## LOWER BASIN
#input_low<- read.xlsx("./dat/inputs.xlsx",
#	sheetName="lower",header=FALSE,
#	colClasses=c("character", 
#		"numeric","numeric"))
#input_low<- input_low[,-4]
#input_low[,1]<- trim(input_low[,1])
#input_low<-split(input_low[,-1],input_low[,1])
#input_low<-lapply(input_low,function (x) x[!is.na(x)]) 
#
#input_low_wh<- read.xlsx("./dat/inputs.xlsx",
#	sheetName="lower_wh",header=FALSE,
#	colClasses=c("character", 
#		"numeric","numeric"))
#input_low_wh<- input_low_wh[,-4]
#input_low_wh[,1]<- trim(input_low_wh[,1])
#input_low_wh<-split(input_low_wh[,-1],input_low_wh[,1])
#input_low_wh<-lapply(input_low_wh,function (x) x[!is.na(x)]) 
#input_low_wh$type<- ifelse(input_low_wh$type==1,"triag","unif")
#
#input_upp_wh<- read.xlsx("./dat/inputs.xlsx",
#	sheetName="upper_wh",header=FALSE,
#	colClasses=c("character", 
#		"numeric","numeric"))
#input_upp_wh<- input_upp_wh[,-4]
#input_upp_wh[,1]<- trim(input_upp_wh[,1])
#input_upp_wh<-split(input_upp_wh[,-1],input_upp_wh[,1])
#input_upp_wh<-lapply(input_upp_wh,function (x) x[!is.na(x)])
#input_upp_wh$type<- ifelse(input_upp_wh$type==1,"triag","unif")
#
### UPPER BASIN
#input_up<- read.xlsx("./dat/inputs.xlsx",
#	sheetName="upper",header=FALSE,
#	colClasses=c("character", 
#		"numeric","numeric"))
#input_up<- input_up[,-4]
#input_up[,1]<- trim(input_up[,1])
#input_up<-split(input_up[,-1],input_up[,1])
#input_up<-lapply(input_up,function (x) x[!is.na(x)])
#
#
#
# 