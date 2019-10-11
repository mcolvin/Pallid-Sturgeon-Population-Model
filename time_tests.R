



# PULL DEFAULT INPUTS AND SET UP FOLDERS
setwd("C:/Users/sreynolds/Documents/GitHub/Pallid-Sturgeon-Population-Model/pallid-sturgeon-population-model")
source("global2.R")
source("./src/SAVE-SIMULATION-OUTPUT.r")
input$output_name<- "2019-04-11"
input$commit<- "time_test"
# CREATE DIRECTORY IF NOT ALREADY THERE
dir.create(file.path(paste0(getwd(),"/analyses/",input$output_name)), showWarnings = FALSE)

# TABLE OF ALL INPUT OPTIONS
switches<-expand.grid(basin=c("upper", "lower"),
                      spatial=c(TRUE, FALSE), 
                      migration=c(TRUE,FALSE), 
                      genetics=c(TRUE,FALSE),
                      hatchery_name=c(TRUE, FALSE),
                      recruitmentFreq=c(0,1),
                      stockingFreq=c(0,1),
                      demographicOnly=c(FALSE,TRUE),
                      weightCalc=c(TRUE, FALSE))


# PREPARE INPTUS FOR MODEL
times<-lapply(1:nrow(switches), function(i)
{
  ptm <- proc.time()
  input$version<- ifelse(i<10, paste0("00", i, "-10"),
                         ifelse(i<100, paste0("0", i, "-10"), 
                                paste0(i, "-10")))
  dir.create(file.path(paste0(getwd(),"/analyses/",input$output_name, "/",
                              input$output_name, "-", input$version)), showWarnings = FALSE)
  inputs<<- modelInputs(input, basin=switches[i,]$basin, bend_meta=bend_meta,
                       spatial=switches[i,]$spatial, 
                       migration=switches[i,]$migration,
                       genetics=switches[i,]$genetics, 
                       hatchery_name=switches[i,]$hatchery_name)
  inputs$nreps<<-10
  inputs$daug_H<<-60000	
  inputs$daug_N<<-20000	
  inputs$nyears<<- 10
  
  # INITIALIZE OBJECTS NEEDED FOR SIMUALTION
  dyn<<- initialize(inputs=inputs)

  # RUN SIMULATION
  out<<- sim(inputs=inputs,
            dyn=dyn,
            recruitmentFreq=switches[i,]$recruitmentFreq,
            stockingFreq = switches[i,]$stockingFreq,
            demographicOnly = switches[i,]$demographicOnly,
            weightCalc = switches[i,]$weightCalc)
  saveSimulationOutput(out)
  tm<-proc.time() - ptm
  elaps<- unname(tm[3])
  fn0<-paste0("./analyses/",inputs$output_name,"/",inputs$output_name,"-",inputs$version,"/")
  write.csv(elaps, paste0(fn0, "time.csv"), row.names = FALSE)
  return(tm)
})

times<-do.call(rbind, times)
times<-times[, 1:3]

switches$nyears<- 10
switches$nreps<- 10
switches$elapsed_time<- times[,3]






