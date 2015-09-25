
# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# LOAD INPUTS

## LOWER BASIN
input_low<- read.xlsx("./dat/inputs.xlsx",
	sheetName="lower",header=FALSE,
	colClasses=c("character", 
		"numeric","numeric"))
input_low<- input_low[,-4]
input_low[,1]<- trim(input_low[,1])
input_low<-split(input_low[,-1],input_low[,1])
input_low<-lapply(input_low,function (x) x[!is.na(x)]) 

input_low_wh<- read.xlsx("./dat/inputs.xlsx",
	sheetName="lower_wh",header=FALSE,
	colClasses=c("character", 
		"numeric","numeric"))
input_low_wh<- input_low_wh[,-4]
input_low_wh[,1]<- trim(input_low_wh[,1])
input_low_wh<-split(input_low_wh[,-1],input_low_wh[,1])
input_low_wh<-lapply(input_low_wh,function (x) x[!is.na(x)]) 
#input_low_wh$type<- ifelse(input_low_wh$type==1,"triag","unif")

input_upp_wh<- read.xlsx("./dat/inputs.xlsx",
	sheetName="upper_wh",header=FALSE,
	colClasses=c("character", 
		"numeric","numeric"))
input_upp_wh<- input_upp_wh[,-4]
input_upp_wh[,1]<- trim(input_upp_wh[,1])
input_upp_wh<-split(input_upp_wh[,-1],input_upp_wh[,1])
input_upp_wh<-lapply(input_upp_wh,function (x) x[!is.na(x)])
#input_upp_wh$type<- ifelse(input_upp_wh$type==1,"triag","unif")

## UPPER BASIN
input_up<- read.xlsx("./dat/inputs.xlsx",
	sheetName="upper",header=FALSE,
	colClasses=c("character", 
		"numeric","numeric"))
input_up<- input_up[,-4]
input_up[,1]<- trim(input_up[,1])
input_up<-split(input_up[,-1],input_up[,1])
input_up<-lapply(input_up,function (x) x[!is.na(x)])



 