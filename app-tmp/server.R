

shinyServer(function(input, output) {

## 1. MAKE A VECTOR OF SURVIVALS
modelInputs<- reactive({
	tmp<-list(
		# POPULATION CHARACTERISTICS
		basin= input$basin,
		maxage=input$maxage,
		sexratio=input$sexratio,
		natural=input$natural,
		hatchery=input$hatchery,
		natural_age0=input$natural_age0,
		hatchery_age0=input$hatchery_age0,
		
		## LENGTH-WEIGHT
		a= exp(input$a_prime),
		b= input$b,
		lw_er=input$lw_er,
		
		# FECUNDITY
		fec_a=input$fec_a,
		fec_b=input$fec_b,
		fec_er=input$fec_er,
		
		# GROWTH
		k=input$k,
		t0=input$t0,
		linf=input$linf,
		vb_er=input$vb_er,
	
		# SEXUAL MATURITY AND RETURN TO SPAWNING
		age_mat=input$age_mat,
		mat_k=input$mat_k,
		spn_a=input$spn_a,
		spn_b=input$spn_b,
	
		# SURVIVALS
		phi1=input$phi_e2fe, # embryo to free embryo
		phi2=input$phi_fe2efl,
		phi3=input$phi_efl2age1,
		phi=c(input$phi1,rep(input$phi2,input$maxage-1)),
		
		# MOVEMENT [NOT ON UI YET]
		spread=10, #input$spread<- 10
		
		# STOCKING
		stocking_amount=input$stocking_amount,
		stocking_month=input$stocking_month,
		recruit_mean_length=input$recruit_mean_length,
		recruit_length_sd=input$recruit_length_sd ,
		stocking_bend=input$stocking_bend,

		# SIMULATION STUFF
		nreps=input$nreps,
		nyears=input$nyears,
		daug=input$daug
		)
	tmp$basin_inp=ifelse(input$basin=="Lower",0,1)
	# BEND DATA & META
	tmp$bend_meta<- subset(bend_meta,basin==tmp$basin)
	tmp$n_bends<- nrow(tmp$bend_meta)	

	## MOVEMENT MATRIX
	# BEND_NUM Length.RKM
	
	tmp$prob<- matrix(runif(tmp$n_bends*tmp$n_bends),nrow=tmp$n_bends,ncol=tmp$n_bends)
	tmp$prob[upper.tri(tmp$prob)]<-0
	tmp$prob<- tmp$prob/apply(tmp$prob,1,sum)
	
	# INITIALIZATION
	
	## AGE STRUCTURE
	if(input$agestructure=="Approximate equilibrium")
		{
		tmp$rel_age<- cumprod(tmp$phi)/sum(cumprod(tmp$phi))
		}
	if(input$agestructure=="Uniform")
		{
		pp<- rep(1,tmp$maxage)
		tmp$rel_age<- pp/sum(pp)
		}
	if(input$agestructure=="Random")
		{
		pp<- runif(tmp$maxage)
		tmp$rel_age<- pp/sum(pp)
		}



	## SPATIAL STRUCTURE
	
	if(input$adult_spatial_structure=="Uniform")
		{
		pp<- rep(1,tmp$n_bends)
		tmp$rel_density<-pp/sum(pp) 
		}
	if(input$adult_spatial_structure=="Random")
		{
		pp<- runif(tmp$n_bends)
		tmp$rel_density<-pp/sum(pp)		
		}
		
	if(input$age0_n_spatial_structure=="Uniform")
		{
		pp<- rep(1,tmp$n_bends)
		tmp$natural_age0_rel_dens<-pp/sum(pp) 
		}
	if(input$age0_n_spatial_structure=="Random")
		{
		pp<- runif(tmp$n_bends)
		tmp$natural_age0_rel_dens<-pp/sum(pp)		
		}	
		
	if(input$age0_h_spatial_structure=="Uniform")
		{
		pp<- rep(1,tmp$n_bends)
		tmp$hatchery_age0_rel_dens<-pp/sum(pp) 
		}
	if(input$age0_h_spatial_structure=="Random")
		{
		pp<- runif(tmp$n_bends)
		tmp$hatchery_age0_rel_dens<-pp/sum(pp)		
		}			
	return(tmp)
	})
	
## 1.2. DYNAMIC UI GIVEN INPUTS
### 1.2.1. LENGTH-FECUNDITY BASIN-SPECIFIC PARAMETERS
output$fecundity_ui_a<- renderUI({
	if(input$basin=="Lower"){val<-10.77  } 
	if(input$basin=="Upper"){val<-11.26 }
	numericInput('fec_a', "Fecundity-length a parameter", val, -1000000,1000000)
	})
output$fecundity_ui_b<- renderUI({
	if(input$basin=="Lower"){val<- 0.62} 
	if(input$basin=="Upper"){val<- 0.57}
	numericInput('fec_b', "Fecundity-length b parameter", val, -1000000,1000000)
	})
output$fecundity_ui_c<- renderUI({
	if(input$basin=="Lower"){val<- 0.30} 
	if(input$basin=="Upper"){val<- 0.39}
	numericInput('fec_er', "Fecundity-length uncertainty", val, -1000000,1000000)
	})	

### 1.2.2. LENGTH-FECUNDITY BASIN-SPECIFIC PARAMETERS
output$fecundity_ui_lw_a<- renderUI({
	if(input$basin=="Lower"){val<- -13.84  } 
	if(input$basin=="Upper"){val<- -14.09 }
	numericInput('a_prime', "Fecundity-length a' parameter", val, -1000000,1000000)
	})
output$fecundity_ui_lw_b<- renderUI({
	if(input$basin=="Lower"){val<- 3.188} 
	if(input$basin=="Upper"){val<- 3.24}
	numericInput('b', "Fecundity-length b parameter", val, -1000000,1000000)
	})
output$fecundity_ui_lw_c<- renderUI({
	if(input$basin=="Lower"){val<- 0.1371} 
	if(input$basin=="Upper"){val<- 0.165}
	numericInput('lw_er', "Fecundity-length uncertainty", val, -1000000,1000000)
	})		
	
### 1.2.3 SURVIVAL BASIN-SPECIFIC
output$fecundity_ui_phi_age0<- renderUI({# AGE-0 SURVIVAL
	if(input$basin=="Lower"){val<- -9.2} 
	if(input$basin=="Upper"){val<- -9.2}
	numericInput('phi_age0_mean', "Mean age-0 Survival", val, -1000000,1000000)
	})
output$fecundity_ui_phi_age0_er<- renderUI({# AGE-0 SURVIVAL
	if(input$basin=="Lower"){val<- -13.84  } 
	if(input$basin=="Upper"){val<- -14.09 }
	numericInput('phi_age0_er', "Age-0 Survival uncertainty", val, -1000000,1000000)
	})	
output$fecundity_ui_phi_age1<- renderUI({# AGE-0 SURVIVAL
	if(input$basin=="Lower"){val<- 0.75} 
	if(input$basin=="Upper"){val<- 0.75}
	numericInput('phi_age1_mean', "Mean age-1 Survival", val, -1000000,1000000)
	})
output$fecundity_ui_phi_age1_er<- renderUI({# AGE-0 SURVIVAL
	if(input$basin=="Lower"){val<- -13.84  } 
	if(input$basin=="Upper"){val<- -14.09 }
	numericInput('phi_age1_er', "Age-1 survival uncertainty", val, -1000000,1000000)
	})	

output$fecundity_ui_phi_age2<- renderUI({# AGE-0 SURVIVAL
	if(input$basin=="Lower"){val<- 2.44} 
	if(input$basin=="Upper"){val<- 2.44}
	numericInput('phi_age2_mean', "Mean age-2 Survival", val, -1000000,1000000)
	})
output$fecundity_ui_phi_age2_er<- renderUI({# AGE-2+ SURVIVAL
	if(input$basin=="Lower"){val<- 1} ### FIXME ### FOR UPPER
	if(input$basin=="Upper"){val<- 1}
	numericInput('phi_age2_er', "Age-2+ survival uncertainty", val, -1000000,1000000)
	})		
	
	
	
	
	
## 2. 	
output$initialization_plot1<- renderPlot({
	xx<-modelInputs()
	
	bend_meta<- subset(bend_meta, basin==input$basin)
	bend_meta$mid<-bend_meta$UPPER_RIVER_MILE*0.5+bend_meta$LOWER_RIVER_MILE*0.5	
	
	
	bend_meta$natural<- xx$rel_density*xx$natural
	bend_meta$hatchery<- xx$rel_density*xx$hatchery
	
	bend_meta$dist_natural_age0<- xx$natural_age0_rel_dens*xx$natural_age0
	bend_meta$dist_hatchery_age0<- xx$hatchery_age0_rel_dens*xx$hatchery_age0
	
	par(mfrow=c(2,2),mar=c(2,3,2,1),oma=c(3,3,2,1))
	plot(natural~mid,bend_meta,xlab="",ylab="",type='h',las=1,main="Natural origin adults")
	plot(hatchery~mid,bend_meta,xlab="",ylab="",type='h',las=1,main="Hatchery origin adults")
	plot(dist_natural_age0~mid,bend_meta,xlab="",ylab="",type='h',las=1,main="Natural origin age-0")
	plot(dist_hatchery_age0~mid,bend_meta,xlab="",ylab="",type='h',las=1,main="Hatchery origin age-0")
	mtext(side=2,"Expected abundance",outer=TRUE,line=0)
	mtext(side=1,"Bend midpoint (km)",outer=TRUE,line=1)
	mtext(side=3, paste("Expected Pallid Sturgeon Distribution within the ",xx$basin," Missouri River Basin", sep=""),outer=TRUE, line=0,cex=1.3)
	
	})	
	
## 3. LENGTH-WEIGHT MODULE
output$lw_module<- renderPlot({			
	###fixme### contrain weigths for lower and upper basin to realistic values
	a<- exp(input$a_prime)
	b<-input$b
	er<-input$lw_er	
	L<-seq(1,2000,1)
	W<- a*L^b 
	lower<-qlnorm(0.025,log(W),er) /1000
	upper<-qlnorm(0.975,log(W),er)/1000
	plot(L,W/1000,ylim=c(0,max(upper)),type='n',
		xlab="Length (mm)",
		ylab="Weight (kg)",las=1)
	polygon(x=c(L,rev(L)),
		y=c(lower,rev(upper)),
		col='lightgrey',
		border='lightgrey')
	points(L,W/1000,type='l')
	})
	
## 4. LENGTH-FECUNDITY MODULE
output$lf_module<-renderPlot({

	inputs<- modelInputs()
	if(is.null(input$fec_a)){return()}
	fl<-matrix(runif(5000,1,1650),ncol=1)
	Eggs<-fecundity(x=1,
		fl=fl,
		a=inputs$fec_a,
		b=inputs$fec_b,
		er=inputs$fec_er,
		sex=matrix(rep(1,nrow(fl)),ncol=1),
		live=matrix(rep(1,nrow(fl)),ncol=1),
		spawn=matrix(rep(1,nrow(fl)),ncol=1))	
	plot(fl,Eggs/1000,type='p',
		xlab="Fork length (mm)",
		ylab="Fecundity (x1000)",las=1)
	})

## 5. GROWTH MODULE
output$growth_module<-renderPlot({
	t0<-input$t0
	linf<- input$linf
	k<- input$k
	er<-input$vb_er	
	age<- seq(1,input$maxage,0.1)
	L<-linf*(1-exp(-k*(age-t0)))
	lower<-qlnorm(0.025,log(L),er)
	upper<-qlnorm(0.975,log(L),er)
	plot(age,L,ylim=c(0,max(upper)),type='n',
		xlab="Age",
		ylab="Length (mm)",las=1)
	polygon(x=c(age,rev(age)),
		y=c(lower,rev(upper)),
		col='lightgrey',
		border='lightgrey')
	points(age,L,type='l')
	})
## EXECUTE MODEL
sim_dat<- eventReactive(input$go, {
	out<- sim(inputs=modelInputs())
	return(out)
	})
	
output$plot1<-renderPlot({
	dat<- sim_dat()
	par(mfrow=c(2,1),mar=c(2,3,0,1),oma=c(1,3,1,1))
	matplot(dat$natural,type='l',xlab="",ylab="",
		las=1,xaxt='n',cex.axis=1.3)
	axis(1,at=axTicks(1),labels=FALSE,cex.axis=1.3)
	matplot(dat$hatchery,type='l',xlab="Months",ylab="",
		las=1,cex.lab=1.5,cex.axis=1.3)	
	mtext(side=2,"Abundance",outer=TRUE,line=1,cex=1.5)
	mtext(side=1,"Months",outer=TRUE,line=1,cex=1.5)
	})
    
})

