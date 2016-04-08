shinyUI(pageWithSidebar(
	headerPanel('Pallid sturgeon individual based model'),

	sidebarPanel(
		selectInput("parmgroups","Parameter Modules",
		list("1. Default",
			"2. Population characteristics",
			"3. Weight-length",
			"4. Fecundity-length",
			"5. Growth", 
			"6. Sexual Maturity and return to spawning",
			"7. Survival",
			"8. Initial age and spatial structure",
			"9. Stocking amounts",
			"10. Run"),selected="1. Default"),
			width=3
		),# END SIDEBAR PANEL
	

	mainPanel(
		# DEFAULT
		conditionalPanel
			(
			condition = "input.parmgroups == '1. Default'",
			includeMarkdown("default.md")
			),
		# POPULATION CHARACTERISTICS
		conditionalPanel
			(
			condition = "input.parmgroups == '2. Population characteristics'",
			selectInput("basin", "Select basin", list("Upper", "Lower")),
			numericInput('maxage', "Maximum age", 41, 1,100),
			numericInput('sexratio', "Sex ratio", 0.33,0,1),			
			numericInput('natural','Number of natural origin fish (Age-1+)',200,0,10000) ,
			numericInput('hatchery','Number of hatchery origin fish (Age-1+)',200,0,10000),
			numericInput('natural_age0','Number of age-0 hatchery origin fish',200,0,10000), 
			numericInput('hatchery_age0','Number of age-0  hatchery origin fish',200,0,10000) 
			),

		# WEIGHT-LENGTH
		conditionalPanel
			(
			condition = "input.parmgroups == '3. Weight-length'",
			plotOutput("lw_module"),
      includeMarkdown("length-weight.md"),
			numericInput('a', "Weight-length parameter (a'; a'=ln(a))", -13.83994,-20, 0),
			numericInput('b', "Weight-length b parameter", 3.2, 2.8,3.5),
			numericInput('lw_er', "Weight-length uncertainty", 0.15,0.00001,0.5)
			),	
			
		# FECUNITY-LENGTH
		conditionalPanel
			(
			condition = "input.parmgroups == '4. Fecundity-length'",
			numericInput('fec_a', "Fecundity-length a parameter", 0, 0.000001,1),
			numericInput('fec_b', "Fecundity-length b parameter", 1.1, 0,3.5),
			numericInput('fec_er', "Fecundity-length uncertainty", 0.2,0.0001,0.5)
			),		
		
		
		# GROWTH
		conditionalPanel
			(
			condition = "input.parmgroups == '5. Growth'",
				numericInput('k','Growth coefficient (1.year;k)',0.3,0.0001,0.5),
				numericInput('t0','Age when length is 0 (years; t0)',0,-1,1),
				numericInput('linf','Length at infinity (mm; L_inf)',1400,400,2000),
				numericInput('vb_er','Error; lognormal distribution',0.2,0.001,0.5)
			),

			
		# SEXUAL MATURITY AND RETURN TO SPAWNING	
		conditionalPanel
			(
			condition = "input.parmgroups == '6. Sexual Maturity and return to spawning'",
				numericInput('age_mat','Age at 50% maturity',8,1,75),
				numericInput('mat_k','How fast curve reaches 100%',0.2,0,1),
				numericInput('spn_a','Spawning intercept',-17.5,-23,23),
				numericInput('spn_b','Spawning beta',0.35,0,1)
			),			
		# END
		
		
		# SURVIVAL
		conditionalPanel
			(
			condition = "input.parmgroups == '7. Survival'",
				numericInput('phi_e2fe',"Survival (Embryo to free embryo)" , 0.001,0,0.1),
				numericInput('phi_fe2efl',"Survival (free embryo to EFL)" , 0.001,0,0.1),
				numericInput('phi_efl2age1',"Survival (EFL to Age-1)" , 0.05,0,0.6),
				numericInput('phi1', "Survival (Age-1)", 0.6 ,0,1),
				numericInput('phi2', "Survival (Age-2)", 0.92,0,1)
			),	
		# MOVEMENT
		
		# INITIALIZATION
		conditionalPanel
			(
			condition = "input.parmgroups == '8. Initial age and spatial structure'",
			plotOutput("initialization_plot1"),
			selectInput('agestructure',"Initial age structure",list("Approximate equilibrium","Uniform","Random"),
				selected="Approximate equilibrium"),
			selectInput('adult_spatial_structure',"Initial adult spatial structure",
				list("Uniform","Random"),selected="Random"),
			selectInput('age0_n_spatial_structure',"Initial age-0 natural origin spatial structure",
				list("Uniform","Random"),selected="Random"),	
			selectInput('age0_h_spatial_structure',"Initial age-0 hatchery origin spatial structure",
				list("Uniform","Random"),selected="Random"),
			includeMarkdown("notes_initialization.md")
			),# end conditionaPanel
		
		# STOCKING
		conditionalPanel
			(
			condition = "input.parmgroups == '9. Stocking amounts'",
			numericInput('stocking_amount',"Number stocked",0, 0,4000),
			numericInput('stocking_month',"Month of stocking", 4,1,12),
			numericInput('recruit_mean_length','Mean length of stocked fish',200 ,0,500),
			numericInput('recruit_length_sd','Std. Deviation of stocking length',30,0,100) ,			
			numericInput('stocking_bend',"Stocking Bend",1,1,317)# MAKE UI RENDER GIVEN UPPER/LOWER
			),# end conditionaPanel
		# RUN THE MODEL	
		conditionalPanel
			(
			condition = "input.parmgroups == '10. Run'",
			includeMarkdown("run.md"),
			sliderInput('nreps', "Number of replicate runs", 1,20,5),
			sliderInput('nyears', 'Number of years to simulate',3,50,5),
			numericInput('daug', "Super population size",30000,5000,35000),	
			p("Click the button below to submit the model for execution. Model execution may take some time.  The progress bar above will update you on the calculations"),
			actionButton("go", "Run model") ,# run model on click
			plotOutput("plot1")
			)# end conditionaPanel
	  
	  
		
  )
))