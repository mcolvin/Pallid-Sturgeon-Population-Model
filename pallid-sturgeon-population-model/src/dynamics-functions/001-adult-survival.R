dSurvival<- function(phi_age,age,maxAge)
	{
	# phi_age: vector of age specific survival
	# age: vector of age in months for live individuals
	phi<- c(phi_age^(1/12))# convert annual to monthly, add 0 to zero out survival of unalive fish
	a<- floor(age/12)
	out<- rbinom(length(a),1,phi[a])
	out<- ifelse(a+out>maxAge,0,out) #SENESCENCE
	return(out)
	}
