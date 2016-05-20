

com8<- odbcConnectAccess2007("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Data/pallids.accdb")
dat<- sqlFetch(com8,"Pallid-Sturgeon-Length-Weight-Data")
stocked<- sqlFetch(com8,"stocked-fish")
