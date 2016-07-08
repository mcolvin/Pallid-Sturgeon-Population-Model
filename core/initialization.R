
### PROCESS INPUTS TO INITILIZE AND SIMULATE POPULATION	
modelInputs<- function(input){#reactive({
	tmp<-list()
	if(input$basin=="Lower"){indx<-1}else{indx<-2}
	tmp$basin_inp=ifelse(input$basin=="Lower",0,1)	
	
	
	## POPULATION CHARACTERISTICS
	tmp$basin= input$basin
	tmp$maxage=input$maxage[indx]
	tmp$sexratio=input$sexratio[indx]
	tmp$natural=input$natural[indx]
	tmp$hatchery=input$hatchery[indx]
	tmp$natural_age0=input$natural_age0[indx]
	tmp$hatchery_age0=input$hatchery_age0[indx]
	

	## LENGTH-WEIGHT
	tmp$a= exp(input$a_prime)[indx]
	tmp$a_prime= input$a_prime[indx]
	tmp$b= input$b[indx]
	tmp$lw_er=input$lw_er[indx]
	
	## FECUNDITY
	tmp$fec_a=input$fec_a[indx]
	tmp$fec_b=input$fec_b[indx]
	tmp$fec_er=input$fec_er[indx]
	
	# GROWTH
	tmp$ln_Linf_mu<-c(6.982160,7.136028770)[indx]
	tmp$ln_k_mu<- c(-2.382711,-3.003764445)[indx]
	tmp$vcv<- as.matrix(cbind(c(0.0894,-0.1327,-0.1327,0.3179),(c(0.2768,-0.364,-0.364,0.6342))))


	# SEXUAL MATURITY AND RETURN TO SPAWNING
	tmp$age_mat=input$age_mat[indx]
	tmp$mat_k=input$mat_k[indx]
	tmp$spn_a=input$spn_a[indx]
	tmp$spn_b=input$spn_b[indx]

	# SURVIVALS
	tmp$pr_embryo<-input$pr_embryo[indx] 
	tmp$phi_embryo<-input$phi_embryo[indx] 
	tmp$phi_free_embryo<-input$phi_free_embryo[indx] 
	tmp$phi0=input$phi_age0_mean[indx] 
	tmp$phi1=input$phi_age1_mean[indx]
	tmp$phi2=input$phi_age2_mean[indx]
	tmp$phi=c(tmp$phi1,rep(tmp$phi2,tmp$maxage-1))
	tmp$recruitment<- input$recruitment # TURN RECRUITMENT ON OR OFF
	
	
	# STOCKING
	tmp$fingerling=input$fingerling
	tmp$fingerling_month=input$fingerling_month
	tmp$fingerling_mn=input$fingerling_mn
	tmp$fingerling_sd=input$fingerling_sd 
	tmp$fingerling_stocking_rkm=input$fingerling_stocking_rkm

	
	### YEARLINGS
	tmp$yearling<-input$yearling
	tmp$yearling_month<-input$yearling_month
	tmp$yearling_mn<-input$yearling_mn
	tmp$yearling_sd<-input$yearling_sd
	tmp$yearling_age<-input$yearling_age
	tmp$yearling_stocking_rkm<-input$yearling_stocking_rkm
	
	
	# SIMULATION STUFF
	tmp$nreps=input$nreps
	tmp$nyears=input$nyears
	tmp$daug=input$daug
	tmp$startYear<-input$startYear
	


	tmp$commit<-input$commit
	tmp$output_name<- input$output_name	
	
	# SPATIAL
	## BEND DATA & META
	tmp$bend_meta<- subset(bend_meta,basin==tmp$basin)
	tmp$n_bends<- nrow(tmp$bend_meta)	
	tmp$bend_lengths<- diff(c(0,bend_meta[which(bend_meta$basin==input$basin),]$bend_start_rkm))
	## MONTHLY MOVEMENT MATRIX
	tmp$prob<- matrix(runif(tmp$n_bends*tmp$n_bends),nrow=tmp$n_bends,ncol=tmp$n_bends)
	tmp$prob[upper.tri(tmp$prob)]<-0
	tmp$prob<- tmp$prob/apply(tmp$prob,1,sum)
	tmp$spatial<- input$spatial
	## SPATIAL STRUCTURE ADULTS
	tmp$adult_spatial_structure<-input$adult_spatial_structure
	## SPATIAL STRUCTURE AGE-0
	if(input$age0_n_spatial_structure=="Uniform")
		{
		pp<- runif(tmp$n_bends)
		tmp$natural_age0_rel_dens<- pp/sum(pp)
		}	
	if(input$age0_h_spatial_structure=="Uniform")
		{
		pp<- runif(tmp$n_bends)
		tmp$hatchery_age0_rel_dens<- pp/sum(pp)
		}	
	tmp$size_indices<-input$size_indices
	## END SPATIAL 


	return(tmp)
	} #})

	