
## UI
## For some reason I couldn't move the UI here
observe({
  if (succesfullyFinishedS1()) {
  #  closeAlert(session, "remapFieldsA1")
    
    ## update map fields selectors
    mfChoices <- c(NA, names(data()$data))
    updateSelectInput(session, "mfId", choices = mfChoices)
    updateSelectInput(session, "mfX", choices = mfChoices)
    updateSelectInput(session, "mfY", choices = mfChoices)
    updateSelectInput(session, "mfDate", choices = mfChoices)
    updateSelectInput(session, "mfTime", choices = mfChoices)
    updateSelectInput(session, "mfDateFormat",
                      choices = c("ymd", "dmy", "mdy", "ymd_h", "ymd_hm",
                                  "ymd_hms", "dmy_h", "dmy_hm", "dmy_hms", "mdy_h",
                                  "mdy_hm", "mdy_hms"))
    updateSelectInput(session, "mfTimeFormat", choices = c("hm", "hms"))
    
    if (debug) cat("\n Updated Select Input \n---------------")
    if (debug) cat(mfChoices, "\n\n")
  }
})

output$data2 <- renderUI({
  if (!is.null(data2())) {
    ## ugly workaround to get date displayed properly
    dat2Temp <- head(as.data.frame(data2()$dat), 25)
    dat2Temp$timestamp <- as.character(dat2Temp$timestamp)
    list(
      h2("Missing and duplicated cases"),
      helpText("This feature is in development."), 
      h2("Preview of data"),
      p("The frist 25 relocations the data are shown below"), 
      renderTable(dat2Temp)
    )
  } else {
    list(
      h2("Data not remapped yet"),
      helpText("Please remap the data first using the panel on the left hand side")
    )
  }
})

data2 <- reactive({
  if (debug) cat("\n Entered data2 \n\n")
  if (succesfullyFinishedS1()) {
    id         <- ifelse(input$mfId == "NA", NA, input$mfId)
    lon        <- ifelse(input$mfX == "NA", NA, input$mfX)
    lat        <- ifelse(input$mfY == "NA", NA, input$mfY)
    date       <- ifelse(input$mfDate == "NA", NA, input$mfDate)
    time       <- ifelse(input$mfTime == "NA", NA, input$mfTime)
    dateFormat       <- input$mfDateFormat
    timeFormat       <- input$mfTimeFormat
    
    if (!is.na(lon) & !is.na(lat)) {
      dat2 <- rhrMapFields(data()$data,
                           fields=list(lon=lon, lat=lat, id=id, date=date, time=time), 
                           projString=NULL, dateFormat=dateFormat,
                           timeFormat=timeFormat, defaultId="Animal1")
      dat2
    } else {
      NULL
    }
  } else {
    NULL
  }
})
