library(shiny)
require(markdown)
library(shinythemes)
library(triangle)
library(reshape2)
library(plyr)
library(lattice)


# HELPER FUNCTIONS 
sim_nxt_adults<- function(matured,sexRatio,prev,S,tmax)
{
  nxt<-rbinom(tmax,prev,S)
  nxt<- nxt+round(matured*sexRatio,0)
  return(c(0,nxt[-tmax]))  
}

bundleReps<- function(appendTo,append,rep)
{
  app<-as.data.frame(append)
  names(app)<- paste("yr",c(1:ncol(app)))
  app$rep<-rep
  out<- rbind(appendTo,app)
  return(out)
}

