tabPanel("Hypotheses",
                     #includeMarkdown("hypotheses.md"),
  					         selectInput("hypo", label = h3("Select a hypothesis"), 
  					                     choices = list("AU-1" = 1, "AU-1a" = 2, "AU-1b" = 3), 
  					                     selected = 1),
  					         hr(),
  					         fluidRow(column(12, uiOutput("hypo_value")))
                     ) # end tabPanel 