shinyServer(function(input, output) {

observeEvent(input$initModules, 
	{
	modules<- paste(input$checkGroup,collapse="")
	if(modules=="1"){out<- "The population only module was initialized"}
	if(modules=="12"){out<-"2"}
	if(modules=="123"){out<-"3"}
	if(modules=="1234"){out<-"4"}
    output$value <- renderUI({out})
	})



hypo<- reactive(
	{
	out<- data.frame(h=c(1,2,3),desc=c(
		"Processes and conditions affecting reproductive behaviors and synchrony limit probability of producing viable gametes.  ",
		"Functional spawning sites with supportive hydraulics and substrate cannot be accessed by reproductive adults because of barriers to upstream migration.",
		"Aggregations of reproductive adults are inadequate to allow mates to find each other because of inadequate hydrology-related cues."))
	out_x<- as.character(out[out$h==input$hypo,]$desc)
	return(out_x)
	})
		
		
	
	output$hypo_value <- renderUI({h4(hypo())})
 
  




})