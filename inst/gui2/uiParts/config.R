  config <- reactive({
    list(
      general = list(
        name = "General Settings",
        content = list(
          doCp = c("Coyp Results" =  getConfigValue("configOutputCpWd", input, config2)), 
          wd = c("Working Directory" =  normalizePath(.outDir, mustWork=FALSE, winslash="/")), 
          fileName = c("File Name" = if (!is.null(input$readFileFile$name)) input$readFileFile$name else "data read from R"), 
          ## fileSize=input$readFileFile$size,
          fieldSeparator = c("Field Separator" = input$readFileFieldSep),
          decSeparator = c("Decimal Separator" = input$readFileSepDec),
          hasHeader = c("Header" = input$readFileHasHeader),
          skip = c("Skip Lines" = input$readFileSkipLines)
        )), 
      fields=list(
        name="Remapping of Fields",
        content=list(
          id=input$mfId, 
          lon=input$mfX, 
          lat=input$mfY, 
          date=input$mfDate, 
          time=input$mfTime, 
          dateFormat=input$mfDateFormat, 
          timeFormat=input$mfTimeFormat)
      )
    )
  })
