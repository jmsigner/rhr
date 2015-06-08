output$configureHR <- renderUI(
  fluidPage(
    navlistPanel("Configure Home Ranges",
                 tabPanel("Unimodal Bivariate Normal",
                          h2("Estimate unimodal bivariate normal home ranges")
                 ), 
                 tabPanel("Bimodal Bivariate Normal",
                          h2("Estimate bimodal bivariate normal home ranges")
                 ), 
                 tabPanel("Minimum Convex Polygon",
                          h2("Configure Minimum Convex Polygon")
                 ),
                 tabPanel("Kernel Density Estimation",
                          h2("Kernel Density Estimation"),
                          selectInput("configKDEbandwidth", "Bandwidth", choices=config$homeRange$kde$bandwidthOptions,
                                      multiple=TRUE, selectize=FALSE),
                          helpText("Use CTRL to select several options"), 
                          uiOutput("configKDEbandwidthUserInput")
                 ),
                 tabPanel("Local Convex Polygon",
                          h2("Local Convex Polygon"),
                          hr(),
                          helpText("'n' is determined automatically, see documentation for more information"), 
                          
                          selectInput("configLOCOHtypeK", "Type k",
                                      choices=c("Include (automatic)" = "incla",
                                                "Include (manual)" = "inclm", 
                                                "Do not include" = "not"),
                                      selected="incla", selectize=FALSE),
                          uiOutput("configLOCOHtypeKField"),
                          selectInput("configLOCOHtypeA", "Type a",
                                      choices=c(
                                        "Include (automatic)" = "incla",
                                        "Include (manual)" = "inclm", 
                                        "Do not include" = "not"),
                                      selected="not", selectize=FALSE), 
                          uiOutput("configLOCOHtypeAField"),
                          
                          selectInput("configLOCOHtypeR", "Type r",
                                      choices=c("Include (automatic)" = "incla",
                                                "Include (manual)" = "inclm", 
                                                "Do not include" = "not"),
                                      selectize=FALSE, selected="not"), 
                          uiOutput("configLOCOHtypeRField"),
                          hr()
                 ), 
                 
                 tabPanel("Home range asymptote",
                          h2("Home range asymptote"),
                          numericInput("configAsymptoteMinNP", "Minimum number of points",
                                       value=config$homeRange$asymptote$minNP), 
                          numericInput("configAsymptoteSI", "Sampling interval",
                                       value=config$homeRange$asymptote$SI), 
                          numericInput("configAsymptoteNRep", "Number of replications",
                                       value=config$homeRange$asymptote$NRep), 
                          sliderInput("configAsymptoteTotA", "Tolerance to total area",
                                      min=0, max=1, step=0.01,
                                      value=config$homeRange$asymptote$TotA), 
                          ## sliderInput("configAsymptoteAlpha", "Alpha for confidence interval",
                          ##             min=0, max=1, step=0.01,
                          ##             value=config$homeRange$asymptote$TotA), 
                          numericInput("configAsymptoteNTimes", "Number of time breakup met",
                                       value=config$homeRange$asymptote$NTimes), 
                          selectInput("configAsymptoteSampling", "Sampling regime",
                                      choices=config$homeRange$asymptote$sampling, 
                                      multiple=FALSE, selectize=FALSE)
                 ), 
                 ## tabPanel("Synoptic Home Ranges",
                 ##         h1("Not yet implemented")), 
                 tabPanel("Brownian Bridges",
                          h2("Estimate home ranges with Brownian Bridges"),
                          bsAlert("generalNoTimeBBMM"), 
                          numericInput("configBBMMSigma2", "Location Error (Sigma 2)", value=10),
                          sliderInput("configBBMMRangeSigma1", "Search range for sigma 1", value=c(1, 1e4),
                                      min=1, max=1e4)
                 ), 
                 well=FALSE)
  )) 


# LoCoH -------------------------------------------------------------------

output$configLOCOHtypeKField <- renderUI({
  if (input$configLOCOHtypeK == "inclm") {
    list(
      numericInput("configLOCOHtypeKmanN", "Manual n", value=10)
      ## helpText("Several values are possible with '10,40,20', a range of values is possible with '10:40:10' to evaluate LoCoH at 10, 20, 30 and 40.")
    )
  } else {
    NULL
  }
})

output$configLOCOHtypeAField <- renderUI({
  if (input$configLOCOHtypeA == "inclm") {
    list(
      numericInput("configLOCOHtypeAmanN", "Manual n", value=10)
      ## helpText("Several values are possible with '10,40,20', a range of values is possible with '10:40:10' to evaluate LoCoH at 10, 20, 30 and 40.")
    )
  } else {
    NULL
  }
})

output$configLOCOHtypeRField <- renderUI({
  if (input$configLOCOHtypeR == "inclm") {
    list(
      numericInput("configLOCOHtypeRmanN", "Manual n", value=10)
      ## helpText("Several values are possible with '10,40,20', a range of values is possible with '10:40:10' to evaluate LoCoH at 10, 20, 30 and 40.")
    )
  } else {
    NULL
  }
})

# KDE ---------------------------------------------------------------------
output$configKDEbandwidthUserInput <- renderUI({
  if ("user" %in% input$configKDEbandwidth) {
    list(
      numericInput("configKDEbandwithUser", "Bandwidth (manual)", value=100)
    )
  } else {
    NULL
  }
}) 

