output$subsetDataUI <- renderUI(sidebarLayout(
  sidebarPanel({
    if (succesfullyFinishedS2()) {
      bbx <- bbox(rgeos::gBuffer(rgeos::gEnvelope(data2()$dat),
                                 width = max(apply(bbox(data2()$dat), 1, diff)) * 0.01))
      uis <- list(
        sliderInput("subsetXSlider", "X-Range", bbx[1, 1], bbx[1, 2], value=bbx[1, ]), 
        sliderInput("subsetYSlider", "Y-Range", bbx[2, 1], bbx[2, 2], value=bbx[2, ])
      )
      
      if (!all(is.na(data2()$dat$timestamp))) {
        uis <- c(uis, list(dateRangeInput("subsetDatePicker", "Date range:",
                                          start = format(min(data2()$dat$timestamp), "%Y-%m-%d"),
                                          end = format(max(data2()$dat$timestamp), "%Y-%m-%d"))))
      }
      
      ## select ids
      pcho <- unique(data2()$dat$id)
      
      uis <- c(uis, list(
        selectInput("subsetSelectedIds", "Select animals",
                    choices=pcho,
                    selected=pcho,
                    multiple=TRUE,
                    selectize=FALSE)
        ## actionButton("subsetReset", "Reset")
        ## helpText("Reset button to come here")
      ))
      uis
    } else {
      NULL
    }
  }), 
  mainPanel(
    list(
      h2("Relocations"), 
      renderPlot({
        if (!is.null(data3())) {
          plot(rgeos::gBuffer(rgeos::gEnvelope(data2()$dat),
                              width = max(apply(bbox(data2()$dat), 1, diff)) * 0.01), border = NA)
          points(data2()$dat, col=adjustcolor("black", 0.1), pch=19)
          points(data3()$dat, col="red")
          axis(1)
          axis(2)
          abline(v=subsetXSliderValues(), col="blue", lty=2)
          abline(h=subsetYSliderValues(), col="blue", lty=2)
        } else {
          NULL
        }}), 
      ## The subset table
      renderTable({
        if (!is.null(data3())) {
          dataa <- as.data.frame(table(data2()$dat$id))
          datbb <- as.data.frame(table(data3()$dat$id))
          dat <- merge(dataa, datbb, by="Var1", all.x=TRUE)
          dat$Freq.y <- ifelse(is.na(dat$Freq.y), 0, dat$Freq.y)
          names(dat) <- c("Id", "Total number of Points", "Currently selected")
          dat
        } else {
          dataa <- as.data.frame(table(data2()$dat$id))
          datbb <- as.data.frame(table(data4()$dat$id))
          dat <- merge(dataa, datbb, by="Var1", all.x=TRUE)
          dat$Freq.y <- ifelse(is.na(dat$Freq.y), 0, dat$Freq.y)
          names(dat) <- c("Id", "Total number of Points", "Currently selected")
          dat
        }
      })
    ))))

## Reactive slider values
subsetXSliderValues <- reactive({
  input$subsetXSlider
}) 

subsetYSliderValues <- reactive({
  input$subsetYSlider
}) 

subsetDatePicker <- reactive({
  as.character(input$subsetDatePicker)
})

subsetSelectedIds <- reactive({
  input$subsetSelectedIds
})

## data 3
data3 <- reactive({
  if(!is.null(data2())) {
    
    if (length(subsetSelectedIds()) == 0L) {
      checked <- unique(data2()$dat$id)
    } else {
      checked <- subsetSelectedIds()
    }
    
    dat <- data2()$dat
    
    bbx <- if (!is.null(subsetXSliderValues()[1])) {
      rgeos::gEnvelope(rgeos::readWKT(
        paste0("MULTIPOINT((", subsetXSliderValues()[1], " ", subsetYSliderValues()[1], "), (",
               subsetXSliderValues()[2], " ", subsetYSliderValues()[2], "))")))
    } else {
      rgeos::gEnvelope(dat)
    }
    
    if (!all(is.na(data2()$dat$timestamp))) {
      b <- subsetDatePicker()
      if (length(b) == 0) {
        b <- c("1800-1-1", "2150-1-1")
      }
      dat <- dat[dat$id %in% checked & 
                   dat$timestamp >= ymd(b[1]) &
                   dat$timestamp <= ymd(b[2]), ]
    } else {
      dat <- dat[dat$id %in% checked , ]
    }
    
    dat <- dat[which(rgeos::gCovers(bbx, dat, byid=TRUE)), ]
    dataNew <- data2()
    dataNew$dat <- dat
    
    
    if (nrow(dat) == 0) {
      NULL
    } else {
      dataNew
    }
  }
})
