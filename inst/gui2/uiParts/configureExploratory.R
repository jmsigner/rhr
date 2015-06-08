output$configureExploratory <- renderUI(
  fluidPage(
    navlistPanel("Configure exploratory analysis",
                 tabPanel("Site Fidelity",
                          h2("Site Fidelity"), 
                          numericInput("configSiteFidelityN", "Number of bootstrap replicates", value=config$pointLevel$sf$n, min=1), 
                          sliderInput("configSiteFidelityAlpha", "Alpha", min=0, max=1, step=0.01, value=config$pointLevel$sf$alpha)), 
                 tabPanel("Time to statistical independence",
                          h2("Time to statistical independence"),
                          bsAlert("generalNoTimeTTSI"), 
                          numericInput("configTTSIInit", "Initial time difference", value=config$pointLevel$ttsi$interval),
                          helpText("Initial time difference in seconds"), 
                          selectInput("configTTSISampling", "Sampling regime", choices=config$pointLevel$ttsi$sampling), 
                          numericInput("configTTSINTimes", "Number of time above critical value", value=config$pointLevel$ttsi$ntimes)
                          ##   sliderInput("configTTSINAlpha", "Alpha", value=config$pointLevel$ttsi$alpha, min=0, max=1, step=0.01)
                 ), 
                 well=FALSE)))
