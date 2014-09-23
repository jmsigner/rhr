
## For the packaged version: make an optinal data argument and
## outputDir argument. They will be set static and be identical for each client.
## This shouldn't be a problem, if the app is run locally

## clean everything 
rm(list=ls())
debug <- FALSE
if (debug) {
  .datFromR <- NULL
  outDir <- "/temp"
}
#####

require(shiny)
require(lubridate)
require(knitr)
require(shinyBS)

shinyServer(function(input, output, session) {
  ## ============================================================================== ##  
  ## Set up


##  runId <- paste0("rhr-run-", format(now(), "%Y%m%d%H%M%S"))
##  outDir <- normalizePath(file.path(normalizePath(tempdir(), winslash="/"), runId), winslash="/")
##  dir.create(outDir)
##  addResourcePath("out", outDir)


  if (debug) cat("====================================================\n", file=stderr())
  if (debug) cat(outDir, "\n", file=stderr())


  ## ============================================================================== ##  
  ## Read data

  data <- reactive({

    if (is.null(.datFromR)) {

      ## Values
                                        #values <- reactiveValues()
                                        #values$undoSubset <- NULL
      if (debug) cat("\n Entered data !~u~! \n")

      res <- paste0('Unable to load data, did you select a file?')
      dat <- NULL
      exitStatus <- 1


      if (debug) cat(input$readFileFieldSep, "\n", file=stderr())
      if (!is.null(input$readFileFile)) {
        sep <- switch(input$readFileFieldSep,
                      comma=",",
                      tab="\t",
                      semi=";")

        ## asign decimal separator
        sepDec <- switch(input$readFileSepDec,
                         comma=",",
                         point=".")

        dat <- tryCatch(rhrReadData(input$readFileFile$datapath,
                                    sep=sep,
                                    skip=input$readFileSkipLines,
                                    hasHeader=input$readFileHasHeader),
                        error=function(e) e)

        if (debug) cat(class(dat))

        if (!is(dat, "error")) {
          res <- "Data successfully read"
          exitStatus <- 0
        } else {
          res <- paste0("Something went wrong:", dat$message)
          exitStatus <- 1
        }
      } 
      
### Seperate into message and preview
      list(data=dat, message=res, exitStatus=exitStatus)
    } else {
      list(data=.datFromR, message="Data read from R", exitStatus=0)
    }
  })

  observe({
    createAlert(session, "alertLoadData", "a1",
                "Reading Input Data",
                message=data()$message,
                type=if(data()$exitStatus == 1) "error" else "success",
                dismiss=TRUE,
                append=FALSE)
  })

  output$readFileTable <- renderUI({
    if (!is.null(data()$data)) {
      list(
        h2("Preview of data"),
        p("The frist 25 relocations the data are shown below"), 
        renderTable(head(data()$data, 25))
        )
    } else {
      list(
        h2("No data loaded yet"),
        helpText("Please upload data first, using the panel on the left hand side")
        )
    }
  })


  ## We can only proceed if this is true
  succesfullyFinishedS1 <- reactive({
    if (debug) cat("\n Entered successfullyFinishedS1 !~u~! \n")
    if (!is.null(data()$data) & data()$exitStatus == 0) {
      TRUE
    } else {
      FALSE
    }
  })
    
  ## All Settings
  config <- reactive({
    list(
      general=list(
        name="General Settings",
        content=list(
          doPdf=input$configOutputMkPdf,
          doCp=input$configOutputCpWd,
          wd=normalizePath(.outDir, winslash="/"), 
          ## doZip=input$configOutputZip,
          ## zipPath=normalizePath(file.path(outDir, paste0(runId, ".zip")), winslash="/"),
          fileName=if (!is.null(input$readFileFile$name)) input$readFileFile$name else "data read from R", 
          ## fileSize=input$readFileFile$size,
          fieldSeparator=input$readFileFieldSep,
          decSeparator=input$readFileSepDec,
          hasHeader=input$readFileHasHeader,
          skip=input$readFileSkipLines
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

  ## ============================================================================= ##
  ## Remap fields

  observe({
    if (!succesfullyFinishedS1()) {
      createAlert(session, "alertMapFields", "remapFieldsA1",
                  "Mapping Fields",
                  message="Data must be read before remapping", 
                  type="info", 
                  dismiss=FALSE,
                  append=FALSE)
    } else {
      closeAlert(session, "remapFieldsA1")

      ## update map fields selectors
      mfChoices <- c(NA, names(data()$data))
      updateSelectInput(session, "mfId", choices = mfChoices)
      updateSelectInput(session, "mfX", choices = mfChoices)
      updateSelectInput(session, "mfY", choices = mfChoices)
      updateSelectInput(session, "mfDate", choices = mfChoices)
      updateSelectInput(session, "mfTime", choices = mfChoices)
      updateSelectInput(session, "mfDateFormat", choices = c("ymd", "dmy", "mdy", "ymd_h", "ymd_hm", "ymd_hms", "dmy_h", "dmy_hm", "dmy_hms", "mdy_h", "mdy_hm", "mdy_hms"))
      updateSelectInput(session, "mfTimeFormat", choices = c("hm", "hms"))

      if (debug) cat("\n Updated Select Input \n")
    }
  })

  ## ------------------------------------------------------------------------------ ##  
  ## check for EPSG codes


  output$reproject <- renderUI({
    if (rhrValidEpsg(input$configInEpsg)) {
      return(list(
        helpText("Optionally you can reproject your data"),
        numericInput("configOutEpsg", "Output EPSG", NA), 
        textOutput("rhrReproject")))
    } else {
      return(list(
        helpText("Invalid or no input EPSG provided")
        ))
    }
  })

  ## Remapping of the fields happens here

  data2 <- reactive({
    if (debug) cat("\n Entered data2 \n")
    if (succesfullyFinishedS1()) {
      id         <- ifelse(input$mfId == "NA", NA, input$mfId)
      lon        <- ifelse(input$mfX == "NA", NA, input$mfX)
      lat        <- ifelse(input$mfY == "NA", NA, input$mfY)
      date       <- ifelse(input$mfDate == "NA", NA, input$mfDate)
      time       <- ifelse(input$mfTime == "NA", NA, input$mfTime)
      dateFormat       <- input$mfDateFormat
      timeFormat       <- input$mfTimeFormat

      if (debug) cat("lat class: ", class(lat), "\n")
      if (debug) cat("lon class: ", class(lon), "\n")
      if (debug) cat("id class: ", class(id), "\n")

      if (debug) cat("lat class: ", lat, "\n")
      if (debug) cat("lon class: ", lon, "\n")
      if (debug) cat("id : ", id, "\n")
      if (debug) cat("day : ", date, "\n")
      if (debug) cat("time : ", time, "\n")
      if (debug) cat("dateformat : ", dateFormat, "\n")
      if (debug) cat("timeformat : ", timeFormat, "\n")

      if (debug) cat("Trying to remap", "\n")
      if (debug) cat(names(data()$data), "\n")
      if (debug) cat("Lookup ======= ", "\n")
      if (debug) cat(c(lon=lon, lat=lat, id=id, date=date, time=time), "\n")
      
      dat2 <- tryCatch(rhrMapFields(data()$data,
                                    fields=list(lon=lon, lat=lat, id=id, date=date, time=time), 
                                    proj4string=NULL, dateFormat=dateFormat,
                                    timeFormat=timeFormat, defaultId="Animal1"), error=function(e) NULL)
      

      if (debug) cat("str(dat2)\n")
      if (debug) cat(str(dat2$dat))

      if (debug) cat(input$configInEpsg, "\n")
      if (debug) cat(input$configOutEpsg, "\n")

      ## Epsg can be used later
      if (!is.null(dat2)) {
        if (!is.null(input$configOutEpsg)) {
          if (!is.na(input$configOutEpsg)) {
            if (!is.na(input$configInEpsg)) {
              if (rhrValidEpsg(input$configOutEpsg)) {
                ## should I also reproject
                dat2$dat <- SpatialPointsDataFrame(dat2$dat[, c("lon", "lat")],
                                                   data=dat2$dat[, c("id", "timestamp")],
                                                   proj4string=CRS(paste0("+init=epsg:", input$configInEpsg)))
                dat2$dat <- spTransform(dat2$dat, CRS(paste0("+init=epsg:", input$configOutEpsg)))
                dat2$dat <- as.data.frame(dat2$dat)
                names(dat2$dat) <- c("lon", "lat", "id", "timestamp")
                dat2$dat <- dat2$dat[, c("id", "lon", "lat", "timestamp")]
                ## successfully reporjected
                createAlert(session, "rhrReproject", "rhrReproject1",
                            "Reproject",
                            message="Data successfully reprojected", 
                            type="success", 
                            dismiss=TRUE,
                            append=FALSE)
              } else {
                createAlert(session, "rhrReproject", "rhrReproject1",
                            "Reproject",
                            message="Output EPSG not valid, won't reproject data", 
                            type="error", 
                            dismiss=TRUE,
                            append=FALSE)
                
              }
            }
          }
        }
      }
      
      return(dat2)
    } else {
      if (debug) cat("dat2 is null\n")
      if (debug) cat("============ \n")
      return(NULL)
    }
  })

  missingAndDuplicated <- reactive({
    if (!is.null(data())) {
      ff <- data.frame(
        id=names(unlist(data2()$res$nobs)),
        nobs=unlist(data2()$res$nobs), 
        missing=unlist(data2()$res$nIncompleteCases),
        nDuplicated=unlist(data2()$res$nDuplicated),
        finalN=data2()$res$nobsFinal)
      names(ff) <- c("Id", "Number observations", "Number missing", "Number Duplicated", "Final number")
      ff
    }
  })

  output$mfUI <- renderUI({
    if (!is.null(data2())) {
        if (debug) cat(str(data2()),  "\n")
        ## ugly workaround to get date displayed properly
        dat2Temp <- head(data2()$dat, 25)
        dat2Temp$timestamp <- as.character(dat2Temp$timestamp)

          
        list(
          h2("Missing and duplicated cases"),
          renderDataTable(missingAndDuplicated()), 
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


  ## We can only proceed if this is true
  succesfullyFinishedS2 <- reactive({
    if (!is.null(data2())) {
      TRUE
    } else {
      FALSE
    }
  })

  ## Do we have time
  hasTime <- reactive({
    if (is.null(data2()$dat)) {
      FALSE
    } else {
      if (!all(is.na(data2()$dat$timestamp))) {
        TRUE
      } else {
        FALSE
      }
    }
  })

  observe({
    if (!hasTime()) {
        createAlert(session, "generalNoTimeTTSI", "generalNoTimeTTSI1",
                    "Please provide date and time",
                    message="This method requires date and time of relocations, but it was not provided", 
                    type="error", 
                    dismiss=FALSE,
                    append=FALSE)

        createAlert(session, "generalNoTimeBBMM", "generalNoTimeBBMM1",
                    "Please provide date and time",
                    message="This method requires date and time of relocations, but it was not provided", 
                    type="error", 
                    dismiss=FALSE,
                    append=FALSE)
    } else {
        closeAlert(session, "generalNoTimeTTSI1")
        closeAlert(session, "generalNoTimeBBMM1")
      
    }
  })


  ## ============================================================================== ##  
  ## Subset data

  output$subsetUI <- renderUI({
    if (succesfullyFinishedS2()) {
      uis <- list(
        sliderInput("subsetXSlider", "X-Range", min(data2()$dat$lon), max(data2()$dat$lon), value=range(data2()$dat$lon)), 
        sliderInput("subsetYSlider", "Y-Range", min(data2()$dat$lat), max(data2()$dat$lat), value=range(data2()$dat$lat))
        )

      if (!all(is.na(data2()$dat$timestamp))) {
        uis <- c(uis, list(dateRangeInput("subsetDatePicker", "Date range:",
                                          start = format(min(data2()$dat$timestamp), "%Y-%m-%d"),
                                          end = format(max(data2()$dat$timestamp), "%Y-%m-%d"))))
      }

      ## select ids
      pcho <- unique(data2()$dat$id)
      if (debug) cat(pcho, "\n")

      uis <- c(uis, list(
        selectInput("subsetSelectedIds", "Select animals",
                    choices=pcho,
                    selected=pcho,
                    multiple=TRUE,
                    selectize=FALSE),
        hr() 
        ## actionButton("subsetReset", "Reset")
        ## helpText("Reset button to come here")
        ))
      return(uis)
    } else {
      return(NULL)
    }
  })

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

      cat("[b] Recreating data3() with\n")
      cat(subsetSelectedIds(), "\n")
      cat(checked, "\n")
      cat("[e] ===========================\n")

      dat <- data2()$dat
      if (!all(is.na(data2()$dat$timestamp))) {
        dat <- dat[dat$id %in% checked & 
                   dat$lat >= subsetYSliderValues()[1] &
                   dat$lat <= subsetYSliderValues()[2] &
                   dat$lon >= subsetXSliderValues()[1] &
                   dat$lon <= subsetXSliderValues()[2] &
                   dat$timestamp >= ymd(subsetDatePicker()[1]) &
                   dat$timestamp <= ymd(subsetDatePicker()[2]), ]
      } else {
        dat <- dat[dat$id %in% checked &
                   dat$lat >= subsetYSliderValues()[1] &
                   dat$lat <= subsetYSliderValues()[2] &
                   dat$lon >= subsetXSliderValues()[1] &
                   dat$lon <= subsetXSliderValues()[2], ]
      }

      if (nrow(dat) == 0) {
        NULL
      } else {
        dat
      }


    }
  })

  output$subsetPlot <- renderPlot({
    if (!is.null(data3())) {
      plot(data2()$dat[, c("lon", "lat")], col=adjustcolor("black", 0.1), pch=19, asp=1)
      points(data3()[ , c("lon", "lat")], col="red")
      abline(v=subsetXSliderValues(), col="blue", lty=2)
      abline(h=subsetYSliderValues(), col="blue", lty=2)
    } else {
      plot(0,0, type="n")
    }
  })

  ## Reset button: not yet implemented
  ## observe({
  ##   if (!is.null(data3())){
  ##     if (input$subsetReset == 0) {
  ##       cat("[e] ========over 1 \n")
  ##       return(NULL)
  ##     }
  ##     cat("[e] ========over 2 \n")
  ##       updateSliderInput(session, "subsetXSlider", "X-Range", value=range(data2()$lon))
  ##       updateSliderInput(session, "subsetYSlider", "Y-Range", value=range(data2()$lat))

  ##       if (FALSE){
  ##         if (!all(is.na(data2()$timestamp))) {
  ##           updateDateRangeInput(session, "subsetDatePicker", "Date range:",
  ##                                start = format(min(data2()$timestamp), "%Y-%m-%d"),
  ##                                end = format(max(data2()$timestamp), "%Y-%m-%d"))
  ##         }

  ##         ## select ids
  ##         pcho <- unique(data2()$id)

  ##         updateSelectInput(session, "subsetSelectedIds", "Select animals",
  ##                           choices=pcho,
  ##                           selected=pcho,
  ##                           multiple=TRUE,
  ##                           selectize=FALSE)
  ##       }
  ##   }
  ## })

  subsetTable <- reactive({
    if (!is.null(data3())) {
      dataa <- as.data.frame(table(data2()$dat$id))
      datbb <- as.data.frame(table(data3()$id))
      dat <- merge(dataa, datbb, by="Var1", all.x=TRUE)
      dat$Freq.y <- ifelse(is.na(dat$Freq.y), 0, dat$Freq.y)

      names(dat) <- c("Id", "Total number of Points", "Currently selected")
      dat
    } else {
      dataa <- as.data.frame(table(data2()$dat$id))
      datbb <- as.data.frame(table(data4()$id))
      dat <- merge(dataa, datbb, by="Var1", all.x=TRUE)
      dat$Freq.y <- ifelse(is.na(dat$Freq.y), 0, dat$Freq.y)

      names(dat) <- c("Id", "Total number of Points", "Currently selected")
      dat
    }
  })

  output$subsetTable <- renderDataTable({
    subsetTable()
  })


  ## ============================================================================== ##  
  ## Configure & Analysis

  data4 <- reactive({

    ## Just in case some skips the subset step
    if (!is.null(data3())) {
      data4 <- data3()
    } else {
      if (!is.null(data2())) {
        data4 <- data2()$dat
      } else {
        data4 <- NULL
      }
    }
  })

  ## ------------------------------------------------------------------------------ ##  
  ## Input validation

  ## Site Fidelity
  observe({
    x <- as.numeric(input$configSiteFidelityN)
    if(is.na(x)) {
      updateNumericInput(session, "configSiteFidelityN", value=config$pointLevel$sf$n)
    }
  })

  ## TTSI
  observe({
    ttsiinit <- as.numeric(input$configTTSIInit)
    if(is.na(ttsiinit)) {
      updateNumericInput(session, "configTTSIInit", value=config$pointLevel$ttsi$interval)
    }

    ttsintimes <- as.numeric(input$configTTSINTimes)
    if(is.na(ttsintimes)) {
      updateNumericInput(session, "configTTSINTimes", value=config$pointLevel$ttsi$ntimes)
    }
  })


  ## Check for MCP
  # observe({
#    input$btnModalMCPSave
#
#    isolate({
#      x <- as.numeric(input$modalMCPInputLevel)
#      
#      ## This will change the value of input$inText, based on x
#      if (!is.na(x) & x > 1 & x <= 100) {
#        y <- x
#      } else {
#        y <- 95
#      }
#      updateTextInput(session, "modalMCPInputLevel", value = y)
#    })
#  })
#
#
#

  ## ------------------------------------------------------------------------------ ##  
  ## Generate output grid

  output$configOuputGridBufferPlot <- renderPlot({
    if (!is.null(data4())) {
      xrange <- bufferXSliderValues() * c(-1, 1) + range(data4()$lon)
      yrange <- bufferYSliderValues() * c(-1, 1) + range(data4()$lat)

      plot(data4()[, c("lon", "lat")], type="n", asp=1, ylim=yrange, xlim=xrange)
      abline(v=xrange, col="grey50", lty=1)
      abline(h=yrange, col="grey50", lty=1)
      polygon(c(xrange, rev(xrange)), rep(yrange, each=2), col=adjustcolor("red", 0.5), border="grey50")
      points(data4()[, c("lon", "lat")], col=adjustcolor("black", 0.1), pch=19)
    } else {
      plot(0,0, type="n")
    }
  })

  output$bufferUI <- renderUI({
    if (!is.null(data4())) {
      xrange <- diff(range(data4()$lon)) * 2
      yrange <- diff(range(data4()$lat)) * 2
      uis <- list(
        sliderInput("bufferXSlider", "X-Buffer", 0, xrange, xrange * 0.5), 
        sliderInput("bufferYSlider", "Y-Buffer", 0, yrange, yrange * 0.5)
        )
    } else {
      return(NULL)
    }
  })

  ## Reactive slider values
  bufferXSliderValues <- reactive({
    input$bufferXSlider
  }) 

  bufferYSliderValues <- reactive({
    input$bufferYSlider
  }) 

  ## ------------------------------------------------------------------------------ ##  
  ## Grid

  output$gridResUi <- renderUI({
    if (!is.null(data4())) {
        rgs <- apply(data4()[, c("lon", "lat")], 2, function(x) diff(range(x)))
        rgs <- c(rgs / 10, rgs / 500)
        sliderInput("gridResSlider", "Resolution", min(rgs), max(rgs), mean(rgs))
      }
  })

  trast <- reactive({
    if (!is.null(data4())) {

      ext <- rhrExtFromPoints(data4()[, c("lon", "lat")],
                              buffer=c(bufferXSliderValues(), bufferYSliderValues()),
                              extendRange=NULL) 
      if (input$configOutputGridGrid == "pixel") {
        return(rhrRasterFromExt(ext, nrow=input$gridNColSlider, ncol=input$gridNRowSlider, res=NULL))
      } else {
        return(rhrRasterFromExt(ext, nrow=NULL, ncol=NULL, res=input$gridResSlider))
      }
    } else {
      return(NULL)
    }

  })

  output$printGrid <- renderPrint({
    cat(
      " Number of rows:    ", nrow(trast()), "\n",
      "Number of columns: ", ncol(trast()), "\n",
      "Resolution:        ", paste0(res(trast()), collapse=", "))
  })

  output$gridPlot <- renderPlot({
    if (!is.null(data4())) {
      plot(rhrUD(rhrKDE(data4()[if (nrow(data4()) < 100) sample(nrow(data4(), 100)) else 1:nrow(data4()), c("lon", "lat")],
                        trast=trast())))
    }
  })

  ## ------------------------------------------------------------------------------ ##  
  ## locoh

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

  ## ============================================================================= ##
  ## Analzye

  observe({
    if (!is.null(data4())) {
      updateButton(session, "rhrAnalyze", disable=FALSE)
      if (any(c("rhrTTSI", "rhrBBMM") %in% c(input$runSteps, input$runSteps2))) {
        if ("rhrTTSI" %in% c(input$runSteps, input$runSteps2) & !hasTime()) {
          createAlert(session, "rhrRunNoTimeTTSI", "rhrRunNoTimeTTSI1",
                      "Time to statistical independence",
                      message="Date and time are required", 
                      type="error", 
                      dismiss=FALSE,
                      append=FALSE)
          updateButton(session, "rhrAnalyze", disable=TRUE)
        } else {
          closeAlert(session, "rhrRunNoTimeTTSI1")
        }
        if ("rhrBBMM" %in% c(input$runSteps, input$runSteps2) & !hasTime()) {
          createAlert(session, "rhrRunNoTimeBBMM", "rhrRunNoTimeBBMM1",
                      "Brownian Bridges Movement Model",
                      message="Date and time are required", 
                      type="error", 
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

  observe({
    if (input$rhrAnalyze == 0) {
      return(NULL)
    }

    input$rhrAnalyze
    
    isolate({
      if (!is.null(data4())) {
        closeAlert(session, "rhrAnalyzeInfo1")

        runId <- paste0("rhr-run-", format(now(), "%Y%m%d%H%M%S"))
        outDir <- normalizePath(file.path(normalizePath(tempdir(), winslash="/"), runId), winslash="/")
        dir.create(outDir)
        addResourcePath("out", outDir)

        ## Create args
        args <- list(
          rhrSiteFidelity=list(
            n=input$configSiteFidelityN,
            alpha=input$configSiteFidelityAlpha), 
          rhrKDE=list(
            levels=rhrCorrectLevels(input$configGlobalLevel),
            trast=trast(), 
            h=input$configKDEbandwidth,
            userh=input$configKDEbandwidthUser), 
          rhrBBMM=list(
            levels=rhrCorrectLevels(input$configGlobalLevel),
            trast=trast(), 
            sigma2=input$configBBMMSigma2, 
            rangesigma1=input$configBBMMRangeSigma1), 
          rhrMCP=list(
            levels=rhrCorrectLevels(input$configGlobalLevel)
            ), 
          rhrTTSI=list(
            init=input$configTTSIInit,
            consec=input$configTTSISampling,
            ntimes=input$configTTSINTimes,
            alpha=input$configTTSIAlpha
            ),
          rhrLoCoH=list(
            levels=rhrCorrectLevels(input$configGlobalLevel),
            n=do.call(c, list(
              switch(input$configLOCOHtypeK, "inclm" = input$configLOCOHtypeKmanN),
              switch(input$configLOCOHtypeA, "inclm" = input$configLOCOHtypeAmanN),
              switch(input$configLOCOHtypeR, "inclm" = input$configLOCOHtypeRmanN))), 
            autoN=do.call(c, list(
              switch(input$configLOCOHtypeK, "incla" = TRUE, "inclm" = FALSE),
              switch(input$configLOCOHtypeA, "incla" = TRUE, "inclm" = FALSE),
              switch(input$configLOCOHtypeR, "incla" = TRUE, "inclm" = FALSE))),
            type=do.call(c, list(
              switch(input$configLOCOHtypeK, "incla" = "k", "not" = NULL),
              switch(input$configLOCOHtypeA, "incla" = "a", "not" = NULL),
              switch(input$configLOCOHtypeR, "incla" = "r", "not" = NULL)))
            ), 
          rhrAsymptote=list(
            estimators=input$configAsymptoteEstimators,
            minNP=input$configAsymptoteMinNP,
            si=input$configAsymptoteSI,
            nrep=input$configAsymptoteNRep,
            tolTotArea=input$configAsymptoteTolToA,
            nTimes=input$configAsymptoteNTimes,
            sampling=input$configAsymptoteSampling
            ),
           rhrUniNorm=list(
             levels=rhrCorrectLevels(input$configGlobalLevel),
             trast=trast()
             ),
           rhrBiNorm=list(
             levels=rhrCorrectLevels(input$configGlobalLevel),
             trast=trast()
             )
          )

        if (debug) cat(str(args))
        
        createAlert(session, "rhrAnalyzeProgress", "rhrAnalyzeProgress1",
                    "Starting Analysis",
                    message=paste0("[", Sys.time(), "] Preparing the analysis"), 
                    type="info", 
                    dismiss=FALSE,
                    append=FALSE)

        withProgress(session, {

          setProgress(message="Preparing calculations", detail="creating output files .....")

          if (debug) {
            files <- "/home/johannes/Documents/20_phd/201_projects/rhr2/pkg/rhr/inst/guiTemp"
          } else {
            files <- system.file("guiTemp", package="rhr")
          }
            
        createAlert(session, "rhrAnalyzeProgress", "rhrAnalyzeProgress2",
                    "Starting Calculations",
                    message=paste0("[", Sys.time(), "] Starting with calculations, this may take some time"), 
                    type="info", 
                    dismiss=FALSE,
                    append=TRUE)

          ## ------------------------------------------------------------------------------ ##  
          ## Run the whole analysis
          if (debug) cat("===========\n", c(input$runSteps, input$runSteps2), "\n")
          runtime <- system.time(res <- rhrHrAnalysis(data4(),
                                                      what=c(input$runSteps, input$runSteps2), 
                                                      args=args,
                                                      outDir=outDir,
                                                      inUnit=input$configOutputInUnits, 
                                                      outUnit=input$configOutputOutUnits, 
                                                      inGUI=TRUE), gcFirst=TRUE)
          ## ------------------------------------------------------------------------------ ##  
          ## Brew html

        createAlert(session, "rhrAnalyzeProgress", "rhrAnalyzeProgress3",
                    "Report",
                    message=paste0("[", Sys.time(), "] Generating report"), 
                    type="info", 
                    dismiss=FALSE,
                    append=TRUE)

          brewEnv <- list2env(list(
            config=config(), 
            runtime=runtime,
            debug=TRUE, 
            res=res,
            baseDir=outDir,
            steps=input$selectStep, 
            dat=data4(),
            subsetTable=subsetTable(),
            methodLookup=methodLookup,
            epsgs=list(
              input=input$configInEpsg, 
              output=input$configOutEpsg)
            ))

          knitEnv <- list2env(list(
            config=config(), 
            dat=data4(),
            data2=data2()$dat,
            data3=data3(),
            subsetTable=subsetTable(),
            relocTable=missingAndDuplicated()
            ))

          setProgress(message="Creating html file")
          src <- capture.output(brew(file=normalizePath(file.path(files, "body.brew"), winslash="/"), output=stdout(),
                                     envir=brewEnv))
          

          if (debug) cat(str(input$selectStep))


          foo <- knit(text=src, output=normalizePath(file.path(outDir, "rhrReport.Rmd"), winslash="/"), quiet=TRUE,
                      envir=knitEnv)
          
          setProgress(message="Just opening files", detail="new tab :) .....")
          markdownToHTML(
            output=normalizePath(file.path(outDir, "rhrReport.html"), winslash="/"), 
            file=normalizePath(file.path(outDir, "rhrReport.Rmd"), winslash="/"), 
            stylesheet=normalizePath(file.path(files, "style.css"), winslash="/"),
            template=normalizePath(file.path(files, "index.html"), winslash="/"))

          ### pdf
          if (config()$general$content$doPdf) {
            setProgress(message="Generating PDF", detail="may take a some time")
            brew(file=normalizePath(file.path(files, "report_brew.tex"), winslash="/"),
                 output=normalizePath(file.path(outDir, "rhrReport.Rnw"), winslash="/"),
                 envir=brewEnv)

            knit(input=normalizePath(file.path(outDir, "rhrReport.Rnw"), winslash="/"),
                 output=normalizePath(file.path(outDir, "rhrReport.tex"), winslash="/"), 
                 quiet=TRUE,
                 envir=knitEnv)
            
            ow <- setwd(outDir)
            createPDF <- tryCatch(tools::texi2pdf("rhrReport.tex", clean=TRUE), error=function(e) e)
            setwd(ow)
          }

          ## ------------------------------------------------------------------------------ ##  
          ## Clean up
          file.remove(normalizePath(file.path(outDir, "rhrReport.Rnw"), winslash="/"))
          file.remove(normalizePath(file.path(outDir, "rhrReport.Rmd"), winslash="/"))
          file.remove(normalizePath(file.path(outDir, "rhrReport.tex"), winslash="/"))
          unlink(normalizePath(file.path(outDir, "figure"), winslash="/"), recursive=TRUE)


          ## ------------------------------------------------------------------------------ ##  
          ## Zip - Removed since it is not working on Windows
          ## createAlert(session, "rhrAnalyzeProgress", "rhrAnalyzeProgress4",
          ##             "Zip-file",
          ##             message=paste0("[", Sys.time(), "] Zipping all results"), 
          ##             type="info", 
          ##             dismiss=FALSE,
          ##             append=TRUE)

          ## if (config()$general$content$doZip) {
          ##   ow <- setwd(outDir)
          ##   zip(paste0(runId, ".zip"), files=list.files(full.names=TRUE, include.dirs=TRUE))
          ##   if (config()$general$content$doCp) {
          ##     file.copy(paste0(runId, ".zip"), config()$general$content$wd)
          ##   }
          ##   setwd(ow)
          ## }

          if (config()$general$content$doCp) {
            dir.create(normalizePath(file.path(config()$general$content$wd, runId), winslash="/"))
            filesNames <- list.files(outDir, full.names=TRUE, recursive=FALSE, ignore.case=TRUE)
            sapply(filesNames, function(x)
                   file.copy(from=x, to=normalizePath(file.path(config()$general$content$wd, runId), winslash="/")))

            for (f in c("data", "plots", "vector", "raster")) {
              dir.create(normalizePath(file.path(config()$general$content$wd, runId, f), winslash="/"))
              filesNames <- list.files(normalizePath(file.path(outDir, "results", f), winslash="/"), full.names=TRUE, recursive=TRUE,
                                       ignore.case=TRUE)
              sapply(filesNames, function(x)
                     file.copy(from=x, to=normalizePath(file.path(config()$general$content$wd, runId, f), winslash="/")))
            }
            
          }

          createAlert(session, "rhrAnalyzeProgress", "rhrAnalyzeProgress5",
                      "Finish",
                      message=paste0("[", Sys.time(), "] Finished analysis"), 
                      type="info", 
                      dismiss=FALSE,
                      append=TRUE)
          
        })
        browseURL(normalizePath(file.path(outDir, "rhrReport.html"), winslash="/"))
      } else {
        createAlert(session, "rhrAnalyzeInfo", "rhrAnalyzeInfo1",
                    "Not ready yet",
                    message="Please load and remap the data before you try to run an analysis", 
                    type="error", 
                    dismiss=TRUE,
                    append=FALSE)
      }
    })
  })

})
