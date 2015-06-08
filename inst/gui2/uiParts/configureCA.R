output$configureCA <- renderUI(
  fluidPage(
    navlistPanel("Configure Core Area",
                 tabPanel("Method of Powell and Seaman",
                          well=FALSE), 
                 h2("Method of Powell and Seaman"),
                 helpText("No additional configuration is required, core area is calculated from kernel density estimation")
    ))) 
