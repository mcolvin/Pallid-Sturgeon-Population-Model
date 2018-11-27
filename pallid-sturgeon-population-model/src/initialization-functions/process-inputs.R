
### PROCESS INPUTS TO INITILIZE AND SIMULATE POPULATION	
modelInputs<- function(input,spatial=FALSE)
	{
	tmp<-list()
	tmp$basin_inp=ifelse(input$basin=="Lower",0,1)	

    
	## POPULATION CHARACTERISTICS
	tmp$basin= input$basin
	tmp$maxage=input[[input$basin]]$maxage
	tmp$sexratio=input[[input$basin]]$sexratio
	tmp$natural=input[[input$basin]]$natural
	tmp$hatchery=input[[input$basin]]$hatchery
	tmp$natural_age0=input[[input$basin]]$natural_age0
	tmp$hatchery_age0=input[[input$basin]]$hatchery_age0
	

	## LENGTH-WEIGHT
	tmp$a			= exp(input[[input$basin]]$a_prime)
	tmp$a_prime		= input[[input$basin]]$a_prime
	tmp$b			= input[[input$basin]]$b
	tmp$lw_er		=input[[input$basin]]$lw_er
	
	
	## FECUNDITY
	tmp$fec_a=input[[input$basin]]$fec_a
	tmp$fec_b=input[[input$basin]]$fec_b
	tmp$fec_er=input[[input$basin]]$fec_er
	
	
	# GROWTH
	tmp$maxLinf<-input[[input$basin]]$maxLinf
	tmp$ln_Linf_mu<-input[[input$basin]]$ln_Linf_mu
	tmp$ln_k_mu<- input[[input$basin]]$ln_k_mu
	tmp$vcv<- input[[input$basin]]$vcv


	# SEXUAL MATURITY AND RETURN TO SPAWNING
	tmp$age_mat=input[[input$basin]]$age_mat
	tmp$mat_k=input[[input$basin]]$mat_k
	tmp$spn_a=input[[input$basin]]$spn_a
	tmp$spn_b=input[[input$basin]]$spn_b

	
	# SURVIVALS
	tmp$pr_embryo<-input[[input$basin]] $pr_embryo 
	tmp$phi_embryo<-input[[input$basin]]$phi_embryo
	tmp$phi_free_embryo<-input[[input$basin]]$phi_free_embryo
	tmp$phi0=input[[input$basin]]$phi_age0_mean
	tmp$phi1=input[[input$basin]]$phi_age1_mean
	tmp$phi2=input[[input$basin]]$phi_age2_mean
	tmp$phi=c(tmp$phi1,rep(tmp$phi2,tmp$maxage-1))
	
	
	# STOCKING
	
	### FINGERGLINGS
	tmp$fingerling<- data.frame(
			month=input$stockingInput$fingerling_month,
			mean_length=input$stockingInput$fingerling_mn,
			length_sd=input$stockingInput$fingerling_sd	,	
			number=input$stockingInput$fingerling,
			age=input$stockingInput$fingerling_age,
			bend=NA)#input$stockingInput$bend)
	
	### YEARLINGS
	tmp$yearling<-data.frame(
		month=input$stockingInput$yearling_month,
		mean_length=input$stockingInput$yearling_mn,
		length_sd=input$stockingInput$yearling_sd	,	
		number=input$stockingInput$yearling,
		age=input$stockingInput$yearling_age,
		bend=NA)#input$stockingInput$bend)

	# SIMULATION STUFF
	tmp$nreps=input$simulationInput$nreps
	tmp$nyears=input$simulationInput$nyears
	tmp$daug_H=input$simulationInput$daug_H
	tmp$daug_N=input$simulationInput$daug_N
	tmp$startYear<-input$simulationInput$startYear
	tmp$spatial=spatial

	tmp$commit<-input$commit
	tmp$output_name<- input$output_name	
	tmp$version<- input$version	
	
	# SPATIAL
	## SPATIAL STRUCTURE ADULTS
	if(spatial==TRUE)
		{	
		## BEND DATA & META
		tmp$bend_meta<- bend_meta[[input$basin]]
		tmp$n_bends<- nrow(bend_meta[[input$basin]])	
		tmp$bend_lengths<- diff(c(0,bend_meta[[input$basin]]$bend_start_rkm))
		  #Same as $Length.RKM (w/in error)
		
		
		## MONTHLY MOVEMENT MATRIX
		
		### FREE EMBRYOS
		tmp$drift_prob<- matrix(runif(tmp$n_bends*tmp$n_bends),nrow=tmp$n_bends,ncol=tmp$n_bends)
		tmp$drift_prob[upper.tri(tmp$drift_prob)]<-0
		tmp$drift_prob<- tmp$drift_prob/apply(tmp$drift_prob,1,sum)

		### ADULTS
		tmp$adult_prob<- matrix(runif(tmp$n_bends*tmp$n_bends,0,0.1),nrow=tmp$n_bends,ncol=tmp$n_bends)
		diag(tmp$adult_prob)<-0.7
		tmp$adult_prob<- tmp$adult_prob/apply(tmp$adult_prob,1,sum)		
		
		## SPATIAL STRUCTURE AGE-0
		pp<- runif(tmp$n_bends)
		tmp$natural_age0_rel_dens<- pp/sum(pp)
		pp<- runif(tmp$n_bends)
		tmp$hatchery_age0_rel_dens<- pp/sum(pp)

		## SPATIAL STRUCTURE AGE-1+
		pp<- runif(tmp$n_bends)
		tmp$natural_age1plus_rel_dens<- pp/sum(pp)
		pp<- runif(tmp$n_bends)
		tmp$hatchery_age1plus_rel_dens<- pp/sum(pp)		 
		}## END SPATIAL
		
		
	return(tmp)
}
