
# CODE FOR THE "IN A NUTSHELL" TAB
tabPanel("In a nutshell",
	sidebarLayout(
		sidebarPanel(includeMarkdown("md_files/nutshell.md"),style = "color:red"),# end sidebarPanel
	mainPanel(
		tabsetPanel(
			position="right",
			tabPanel("Introduction",includeMarkdown("md_files/include.md")),
			tabPanel("Spatial",includeMarkdown("md_files/spatial.md")),  
			tabPanel("Model details",includeMarkdown("md_files/model_details.md")),
			source("ui_components/test.R",local=TRUE)$value,
			tabPanel("Relevant links",includeMarkdown("md_files/acknowledgments.md"))              
			)# end tabsetpanel
		) # end mainPanel
	)# end sidbarlayout
)# end tabpanel