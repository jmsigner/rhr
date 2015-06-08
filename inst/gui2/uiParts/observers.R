# Methods requiring time --------------------------------------------------

observe({
  if (!hasTime()) {
    createAlert(session, "generalNoTimeTTSI", "generalNoTimeTTSI1",
                "Please provide date and time",
                content ="This method requires date and time of relocations, but it was not provided", 
                style="error", 
                dismiss=FALSE,
                append=FALSE)
    
    createAlert(session, "generalNoTimeBBMM", "generalNoTimeBBMM1",
                "Please provide date and time",
                content = "This method requires date and time of relocations, but it was not provided", 
                style="error", 
                dismiss=FALSE,
                append=FALSE)
  } else {
    closeAlert(session, "generalNoTimeTTSI1")
    closeAlert(session, "generalNoTimeBBMM1")
    
  }
})

observe({
  if (!is.null(data4())) {
    updateButton(session, "rhrAnalyze", disable=FALSE)
    if (any(c("rhrTTSI", "rhrBBMM") %in% c(input$runSteps, input$runSteps2))) {
      if ("rhrTTSI" %in% c(input$runSteps, input$runSteps2) & !hasTime()) {
        createAlert(session, "rhrRunNoTimeTTSI", "rhrRunNoTimeTTSI1",
                    "Time to statistical independence",
                    content = "Date and time are required", 
                    style="error", 
                    dismiss=FALSE,
                    append=FALSE)
        updateButton(session, "rhrAnalyze", disable=TRUE)
      } else {
        closeAlert(session, "rhrRunNoTimeTTSI1")
      }
      if ("rhrBBMM" %in% c(input$runSteps, input$runSteps2) & !hasTime()) {
        createAlert(session, "rhrRunNoTimeBBMM", "rhrRunNoTimeBBMM1",
                    "Brownian Bridges Movement Model",
                    content = "Date and time are required", 
                    style="error", 
                    dismiss=FALSE,
                    append=FALSE)
        updateButton(session, "rhrAnalyze", disable=TRUE)
      } else {
        closeAlert(session, "rhrRunNoTimeBBMM1")
      }
    } else {
      closeAlert(session, "rhrRunNoTimeTTSI1")
      closeAlert(session, "rhrRunNoTimeBBMM1")
      updateButton(session, "rhrAnalyze", disable=FALSE)
    }
  }
})
