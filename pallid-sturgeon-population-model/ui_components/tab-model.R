# CODE FOR THE "IN A NUTSHELL" TAB
tabPanel("In a nutshell",
	sidebarLayout(
		sidebarPanel(
			# CHECK BOXES FOR SPATIAL, RECRUIT, STOCKING
			checkboxGroupInput("checkGroup", 
				label = h3("Model modules to initialize"), 
				choices = list(
					"Population" = 1, 
					"Recruitment" = 2, 
					"Stocking" = 3,
					"Spatial" = 4),
				selected = 1),
		actionButton("initModules","Initialize modules")
		),# end sidebarPanel
		
		
		
	mainPanel(
		conditionalPanel("input.initModules==1",
			h3("Please initialize analysis modules")
			),
		conditionalPanel("input.initModules!=1",
			uiOutput("value")
			)						
		) # end mainPanel
	)# end sidbarlayout
)# end tabpanel