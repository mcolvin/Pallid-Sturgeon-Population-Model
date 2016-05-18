



com9<-odbcConnectAccess2007("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Data/pallids.accdb")

dat<-sqlFetch(com9,"fecundity")
