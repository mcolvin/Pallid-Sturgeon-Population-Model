library(shiny)
library(markdown)
library(xlsx)
library(shinythemes)



shinyUI(
	navbarPage(
		"Pallid Sturgeon Effects Analysis",
			theme = shinytheme("flatly"),
		source("ui_components/tab-model.R",local=TRUE)$value,
		source("ui_components/tab-nutshell.R",local=TRUE)$value
 	)# end navbarPage	
	
	
)# end ui

