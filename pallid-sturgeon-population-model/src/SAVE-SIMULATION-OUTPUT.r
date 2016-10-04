
saveSimulationOutput<- function(output)
	{
	fn0<-paste0("./analyses/",inputs$output_name,"/",inputs$output_name,"-",inputs$version,"/")
	fn<-paste0(fn0,inputs$output_name,"-",inputs$version,"-output.rds")
	saveRDS(list(output=out, input=inputs),fn)
	}