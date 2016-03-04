

shinyUI(
	navbarPage("Pallid Sturgeon Effects Analysis",theme = shinytheme("flatly"),
		tabPanel("In a nutshell",
			sidebarLayout(
        sidebarPanel(includeMarkdown("nutshell.md"),style = "color:red"),# end sidebarPanel
  			mainPanel(
  			  tabsetPanel(position="right",
  					tabPanel("Introduction",includeMarkdown("include.md")),
  					tabPanel("Spatial",includeMarkdown("spatial.md")),  
            tabPanel("Model details",includeMarkdown("model_details.md")), 
  					tabPanel("Hypotheses",
                     #includeMarkdown("hypotheses.md"),
  					         selectInput("hypo", label = h3("Select a hypothesis"), 
  					                     choices = list("AU-1" = 1, "AU-1a" = 2, "AU-1b" = 3), 
  					                     selected = 1),
  					         hr(),
  					         fluidRow(column(12, uiOutput("hypo_value")))
                     ), # end tabPanel           
  					tabPanel("Relevant links",includeMarkdown("acknowledgments.md"))              
					)# end tabsetpanel
				) # end main panel
			)# end sidbarlayout
		),# end tabpanel
		tabPanel("Model",
		         sidebarLayout(
		           sidebarPanel(
		             h3("How to run the model"),
                 helpText("1. Set demographic rates"),
		             helpText("2. Define sexual maturity function"),
		             helpText("3. Define fecunidy function")  ,
		             helpText("4. Set initial state values (population abundances"),
		             helpText("5. Evaluate population viability, given the inputs from steps 1 - 4")
		             
		           ),# end sidebarPanel
		           mainPanel(
		             tabsetPanel(
		               	tabPanel("Demographic rates",
		               	#h3("Model inputs"),	
		               	fluidRow(
		               	  HTML("<br>"),
                      column(12,p("Change the population demographic values using the slider inputs
                                   to modify the default survival curve illustrated below")),
                      HTML("<br>"),
                      column(12,align="center",plotOutput("plot_01",width="100%",height="auto")),
                      column(12, p("ASSUMPTION: the model assumes that age specific survival is the same for natural
                                   and hatchery origin fish.",style = "color:red"))
                     ),# end fluid row
                    hr(),
		               #	submitButton("Submit"),
                    fluidRow(
                      # POPULATION DEMOGRAPHIC RATES
                      column(4,sliderInput("maxAge","Maximum age",min=0, max=90,value=41)) ,
                      column(4,sliderInput("S0","Survival age-0 ",min=0, max=0.3,value=0.051)),
                      column(4,p("NOTE: Age-0 survival is the product of embryo, free embryo, exogenously feeding 
                                 larvae, and age-0 pallid strugeon"),style = "color:red")
                      # Gametes, Gametes and Developing Embryo, Fr,ee Embryo, Exogenously feeding larvae & age-0
                      ),# end fluid row
		               	fluidRow(# POPULATION DEMOGRAPHIC RATES
		               	  column(4,sliderInput("S1","Survival age-1",min=0.2,max=1, value=0.6)),
		               	  column(4,sliderInput("S2","Survival age-2", min=0.8,max=1, value=0.8))                   
       	              ),# end fluidRow
		               fluidRow(# POPULATION DEMOGRAPHIC RATES
  	                  column(4,sliderInput("S3plus","Survival age-3+", min=0.8,max=1,value=0.92))                     
		                  )# end fluidRow
                   ),# end tab panel
# Sexual maturity ############################################################################	               
		               tabPanel("Sexual maturity",
		                 # h3("Population characteristics"),
                      fluidRow(
                        column(12, align="center",plotOutput("plot_02",width="100%",height="auto")),
                        column(12, p("ASSUMPTION: the model assumes that age specific sexual maturity is the same for natural
                                   and hatchery origin fish.",style = "color:red"))
                      ),# end fluidRow 
                   fluidRow(
                      column(4,numericInput(inputId="aa",label="Minimum age of sexual maturity",
                                           min=0,max=30,value=5,step=1)),
                      column(4,numericInput(inputId="a",label="Age at 25% maturity",
                                           min=0,max=30,value=6,step=1)),           
                      column(4,numericInput(inputId="b",label="Age at 50% maturity",
                                           min=0,max=30,value=7,step=1))
                      ),# end fluidRow
                  fluidRow(
                      column(4,numericInput(inputId="c",label="Age at 75% maturity",
                                           min=0,max=30,value=8,step=1)),   
                      column(4,numericInput(inputId="d",label="Maximum juvenile age",
                                           min=0,max=30,value=9,step=1))                       
                      ) # end fluidRow 
		               ), # end tabPanel

# Fecundity ################################################################################################
tabPanel("Fecundity",
         
        fluidRow(
          column(12, plotOutput("plot_03z")),
          column(4,sliderInput("a_fec","Fecundity intercept",min=1,max=4,value=3.48)),
          column(4,sliderInput("b_fec","Fecundity scale", min=3.9,max=4.1, value=4.05))
          )# end fluidRow
         
         ),# end tabPanel


# Initial state values #####################################################################################
              tabPanel("Initial state values",
                  fluidRow(
                  column(12,align="left",plotOutput("plot_03",width="100%",height="auto")),
                  h4("1. Current adult Pallid Strugeon sex ratio"),
                  column(4,sliderInput("sr_n","Natural origin fish",min=0,max=1,value=0.33)),  
                  column(4,sliderInput("sr_h","Hatchery origin fish",min=0,max=1,value=0.5)),
                  column(4, p("NOTE:  the model assumes a 1:1 ratio of male to female fish during simulations.
                    Specifying current sex ratio accounts for any male bias caused by the legacy
                    of sex biased harvest of females.",style = "color:red"))
                  ),# end fluid row

                  HTML("<br>"),
                  
                  fluidRow(
                  h4("2. Current juvenile Pallid Sturgeon abundances in thousands of fish"),
                  column(4,sliderInput("juv_ini_n","Natural origin fish",min=0,max=100,value=50)) ,# Juvenile (age 1-age -2)  
                  column(4,sliderInput("juv_ini_h","Hatchery origin fish",min=0,max=100,value=50))
                  ),  # end fluidRow                

                  
                  HTML("<br>"),
                  fluidRow(
                  h4("3. Current adult Pallid Strugeon abundances in thousands of fish"),
                  column(4,sliderInput("adults_ini_n","Natural origin fish",
                                       min=0,max=100,value=50)),
                  column(4,sliderInput("adults_ini_h","Hatchery origin fish",
                                       min=0,max=100,value=50))
                  ),
                  #hr(),
                  HTML("<br>"),
                  fluidRow(h4("4. Proceed to the model output tab once you have specified the intial state values"))
                  ),#end tabPanel
                  
                ###############################################################################
                tabPanel("Viability analysis",
                          fluidRow(
                        column(12,align="center",plotOutput("plot_044",width="100%",height="auto"),hr()),
                           column(4,sliderInput(inputId="fe_stock",
                                                label="Free embryos stocked",
                                                min=0, 
                                                max =1000000,
                                                step=1000,
                                                value=0)),
                           column(4,sliderInput(inputId="efl_stock",
                                                label="Exo. feeding larvae stocked",
                                                min=0, 
                                                max =1000000,
                                                step=1000,
                                                value=0)),
                           column(4,sliderInput(inputId="juv_stock",
                                                label="Age-0 stocked",
                                                min=0, 
                                                max =1000000,
                                                step=1000,
                                                value=0)),                           
                           column(4,sliderInput(inputId="nreps",label="Number of replicates",
                                                min=1, max=20,value=5,step=1)),                          
                           
                           column(4,sliderInput(inputId="nyears",label="Years to simulate",
                                                value=50,min=20,max=100,step=10))                          
                           )# end fluidRow
                        ),# end tabPanel
tabPanel("Model Output",column(12,uiOutput("tbl1")))               
		             )# end tabsetpanel
		           ) # end main panel
		         )# end sidbarlayout
		)# end tabpanel
	
	)# end navbarPage	
	
	
)# end ui

