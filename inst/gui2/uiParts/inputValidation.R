# Site Fidelity -----------------------------------------------------------

observe({
  if (!is.null(input$configSiteFidelityN)) {
    x <- as.numeric(input$configSiteFidelityN)
    if(is.na(x)) {
      updateNumericInput(session, "configSiteFidelityN", value=config$pointLevel$sf$n)
    }
  }
})


# TTSI --------------------------------------------------------------------

observe({
  if (!is.null(input$configTTSIInit)) {
    ttsiinit <- as.numeric(input$configTTSIInit)
    if(is.na(ttsiinit)) {
      updateNumericInput(session, "configTTSIInit", value=config$pointLevel$ttsi$interval)
    }
  }
  
  if (!is.null(input$configTTSINTimes)) {
    ttsintimes <- as.numeric(input$configTTSINTimes)
    if(is.na(ttsintimes)) {
      updateNumericInput(session, "configTTSINTimes", value=config$pointLevel$ttsi$ntimes)
    }
  }
})

