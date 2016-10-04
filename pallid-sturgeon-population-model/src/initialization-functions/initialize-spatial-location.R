

# INnITIALIZE RIVER LOCATION
initialize_spatial_location<- function(n,nbends,relativeDensity)
	{
	x<- sample(c(1:nbends),n,relativeDensity,replace=TRUE)
	return(x)
	}