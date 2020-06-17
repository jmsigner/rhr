library(brew)
library(lubridate)
library(knitr)
library(markdown)
library(rgdal)
library(rhr)
library(shinyBS)
library(shiny)
library(xtable)

## clean everything 
rm(list=ls())
debug <- FALSE
hraas <- FALSE
dozip <- FALSE
outdir_base <- tempdir()

if (hraas) {
  dozip <- TRUE
}

options(shiny.maxRequestSize=30*1024^2)
addResourcePath("out", outdir_base)
rhrEPSGs <- readRDS("epsgs.RDS")

if (FALSE) {
  rhrEPSGs <- as.numeric(rgdal::make_EPSG()$code)
  rhrEPSGs <- rhrEPSGs[!is.na(rhrEPSGs)]
   saveRDS(rhrEPSGs, "inst/gui/epsgs.RDS")
}



shinyServer(function(input, output, session) {
  data <- reactive({
    res <- paste0('Unable to load data, did you select a file?')
    dat <- NULL
    exitStatus <- 1
    
    
    if (!is.null(input$readFileFile)) {
      dat <- tryCatch(
        read.csv(file = input$readFileFile$datapath, 
                 header = input$readFileHasHeader, 
                 sep = input$readFileFieldSep, 
                 dec = input$readFileSepDec)
      )
      
      if (!is(dat, "error")) {
        res <- "Data successfully read"
        exitStatus <- 0
      } else {
        res <- paste0("Something went wrong:", dat$message)
        exitStatus <- 1
      }
      createAlert(session, "alertLoadData", "a1",
                  "Reading Input Data",
                  content=res, 
                  dismiss=TRUE, append=FALSE)
    } 
    list(data=dat, message=res, exitStatus=exitStatus)
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
    if (debug) cat("\n Entered successfullyFinishedS1 \n")
    if (debug) cat(str(data()$data))
    if (!is.null(data()$data) & data()$exitStatus == 0) {
      TRUE
    } else {
      FALSE
    }
  })
  
  ## All Settings
  configInter <- reactive({
    list(
      general=list(
        name="General Settings",
        content=list(
          doCp= if (hraas) FALSE else input$configOutputCpWd,
          wd=normalizePath(getwd(), mustWork=FALSE, winslash="/"), 
          fileName=input$readFileFile$name,
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
                  content = "Data must be read before remapping", 
                  style="info", 
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
      updateSelectInput(session, "mfDateFormat",
                        choices = c("ymd", "dmy", "mdy", "ymd_h", "ymd_hm",
                          "ymd_hms", "dmy_h", "dmy_hm", "dmy_hms", "mdy_h",
                          "mdy_hm", "mdy_hms"))
      updateSelectInput(session, "mfTimeFormat", choices = c("hm", "hms"))
    }
  })

  ## ------------------------------------------------------------------------------ ##  
  ## check for EPSG codes


  output$reproject <- renderUI({
    if (rhrValidEpsg(input$configInEpsg)) {
      return(list(
        helpText(paste0("Optionally you can reproject your data (Input EPSG is: ",
                        input$configInEpsg, ")")), 
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
    if (debug) cat("\n Entered data2 \n\n")
    if (succesfullyFinishedS1()) {
      id <- ifelse(input$mfId == "NA", NA, input$mfId)
      lon <- ifelse(input$mfX == "NA", NA, input$mfX)
      lat <- ifelse(input$mfY == "NA", NA, input$mfY)
      date <- ifelse(input$mfDate == "NA", NA, input$mfDate)
      time <- ifelse(input$mfTime == "NA", NA, input$mfTime)
      dateFormat <- input$mfDateFormat
      timeFormat <- input$mfTimeFormat

      if (!is.na(lon) & !is.na(lat)) {
        
        inCRS <-  if (input$configInEpsg %in% rhrEPSGs) {
          createAlert(session, "alert_data_in_setcrs", title = "CRS set", content = paste0("CRS set to ", input$configInEpsg), append = FALSE)
          input$configInEpsg
        } else {
          createAlert(session, "alert_data_in_setcrs", title = "Invalid/No EPSG", content = "Invalid EPSG code, or no CRS set", append = FALSE)
          NULL
        }
        outCRS <- if (input$configOutEpsg %in% rhrEPSGs) {
          if (!is.null(inCRS)) {
            createAlert(session, "alert_data_in_transformcrs", title = "CRS transformed", 
                        content = paste0("CRS transformed to ", input$configOutEpsg), append = FALSE)
          } else {
            createAlert(session, "alert_data_in_transformcrs", title = "CRS not transformed", 
                        content = "Out CRS is ok, but in CRS not set.", append = FALSE)
            
          }
          input$configOutEpsg
        } else {
          createAlert(session, "alert_data_in_transformcrs", title = "Invalid EPSG", content = "CRS not transformed", append = FALSE)
          NULL
        }

        if (debug) cat("dat2.2 \n")
        
        inCRS <- if(!is.null(inCRS)) CRS(paste0("+init=epsg:", inCRS)) else NULL
        outCRS <- if(!is.null(outCRS)) CRS(paste0("+init=epsg:", outCRS)) else NULL
        
        if (debug) cat("dat2.3 \n")
        ## Epsg can be used later, only thing that is missing are 
        dat2 <- tryCatch(rhrMapFields(data()$data,
                             fields=list(lon=lon, lat=lat, id=id, date=date, time=time), 
                             projString=inCRS, 
                             projStringOut = outCRS, dateFormat=dateFormat,
                             timeFormat=timeFormat), error = function(e) e)
        
        if (is(dat2, "error")) {
          return(dat2)
        } else {
          
          r <- if (dat2$hasTS) {
            rhrTracks(dat2$dat, ts = dat2$dat$timestamp, id = dat2$dat$id)
          } else {
            rhrTracks(dat2$dat, id = dat2$dat$id)
          }
          return(r)
        }
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  })
  
  output$mapTable <- renderUI({
    if (!is.null(data2())) {
      list(
        h2("Preview mapped data"),
        p("The frist 20 relocations the data are shown below"), 
        renderTable(head(as.data.frame(rhrPoints(data2())), 20))
      )
    } 
  })
  
  output$mapDataPlot <- renderPlot(
    if(is(data2(), "RhrTracks")) {
      plot(data2())
    } else {
      NULL
    }
  )
  
  output$mapDataMSG <- renderUI(
    if(is(data2(), "error")) {
      verbatimTextOutput(data2()$message)
    } 
  )
  
  succesfullyFinishedS2 <- reactive({
    if (!is.null(data2())) {
      if (!is(data2(), "error")) {
        TRUE
      } else {
        FALSE
      }
    } else {
      FALSE
    }
  })
  
  succesfullyFinishedS3 <- reactive({
    if (!is.null(data3())) {
      TRUE
    } else {
      FALSE
    }
  })

  succesfullyFinishedS4 <- reactive({
    if (!is.null(data4())) {
      TRUE
    } else {
      FALSE
    }
  })


  ## ============================================================================== ##  
  ## Subset data
  
  output$subsetUI <- renderUI({
    if (succesfullyFinishedS2()) {
      bbx <- rhrBBX(data2(), f = 0.02)
      uis <- list(
        h2("Subset settings"),
        sliderInput("subsetXSlider", "X-Range", bbx[1, 1], bbx[1, 2], value=bbx[1, ]), 
        sliderInput("subsetYSlider", "Y-Range", bbx[2, 1], bbx[2, 2], value=bbx[2, ])
      )
      if (debug) cat("dat3.2 \n")
      if (is(data2(), "RhrTracksST")) {
        uis <- c(uis, list(dateRangeInput("subsetDatePicker", "Date range:",
                                          start = format(rhrTracksStart(data2()), "%Y-%m-%d"),
                                          end = format(rhrTracksEnd(data2()), "%Y-%m-%d"))))
      }
      ### select ids
      if (debug) cat("dat3.3 \n")
      pcho <- names(data2())
      uis <- c(uis, list(
        selectInput("subsetSelectedIds", "Select animals",
                    choices=pcho,
                    selected=pcho,
                    multiple=TRUE,
                    selectize=TRUE)
      ))
      if (debug) cat("dat3.4 \n")
      uis
    } else {
      NULL
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
  
  
  data_checked <- reactive({
    if(!is.null(data2())) {
      checked <- if (length(subsetSelectedIds()) == 0L) {
        unique(names(data2()))
      } else {
        subsetSelectedIds()
      }
      checked
    }
  })
  
  
  data3 <- reactive({
    if(!is.null(data2()) & !is.null(subsetXSliderValues()) & !is.null(subsetYSliderValues()) & !is.null(subsetDatePicker())) {
      
      if (debug) cat("3.1: space \n")
      # space
      bbx <- rgeos::gEnvelope(sp::SpatialPoints(cbind(subsetXSliderValues(), subsetYSliderValues())))
      dat <- rhr::rhrWithin(data2(), bbx)
      
      # id
      if (debug) cat("3.2: id \n")
      if (length(data_checked()) == 0L) {
        checked <- names(data2())
      } else {
        checked <- data_checked()
      }
      dat <- rhr:::rhrAnimalById(dat, data_checked())
      
      if (debug) cat("3.3: time \n")
      if (is(data2(), "RhrTracksST")) {
        int <- lubridate::`%--%`(subsetDatePicker()[1], subsetDatePicker()[2])
        dat <- rhr::rhrWithinTime(dat, int)
        
      }
      dat
      
    } 
  })
  
  output$subsetPlot <- renderPlot({
    if (!is.null(data3())) {
      bbx <- rhrBBX(data2(), f = 0.01)
      plot(rhrPoints(data2()), col=adjustcolor("black", 0.1), pch=19, xlim = bbx[1, ], ylim = bbx[2, ])
      
      x <- rhrPoints(data3())
      points(x, col=rainbow(length(unique(x$id))))
      
      axis(1)
      axis(2)
      abline(v=subsetXSliderValues(), col="blue", lty=2)
      abline(h=subsetYSliderValues(), col="blue", lty=2)
      
      
      
    } else {
      plot(0,0, type="n")
    }
  })
  

  subsetTable <- reactive({
    if (!is.null(data3())) {
      dataa <- data.frame(id = names(data2()), 
                          n = rhrN(data2())) 
      datbb <- data.frame(id = names(data3()), 
                          n = rhrN(data3()))
      dat <- merge(dataa, datbb, by="id", all.x=TRUE)
      dat$n.y <- ifelse(is.na(dat$n.y), 0, dat$n.y)

      names(dat) <- c("Id", "Total number of Points", "Currently selected")
      dat
    } else {
      dataa <- data.frame(id = names(data2()), 
                          n = rhrN(data2())) 
      datbb <- data.frame(id = names(data4()), 
                          n = rhrN(data3()))
      dat <- merge(dataa, datbb, by="id", all.x=TRUE)
      dat$n.y <- ifelse(is.na(dat$n.y), 0, dat$n.y)

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
    if (succesfullyFinishedS3()) {
      data3()
    } else if (succesfullyFinishedS2()) {
      data2()
    } else {
      NULL
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
    ttsiinit <- as.numeric(input$configTTSIInterval)
    if(is.na(ttsiinit)) {
      updateNumericInput(session, "configTTSIInterval", value=config$pointLevel$ttsi$interval)
    }
 
    ttsintimes <- as.numeric(input$configTTSINTimes)
    if(is.na(ttsintimes)) {
      updateNumericInput(session, "configTTSINTimes", value=config$pointLevel$ttsi$ntimes)
    }
  })


  ## ------------------------------------------------------------------------------ ##  
  ## Generate output grid

  output$configOuputGridBufferPlot <- renderPlot({
    if (!is.null(data4())) {
      bbx <- rhrBBX(data4())
      xrange <- bufferXSliderValues() * c(-1, 1) + bbx[1, ]
      yrange <- bufferYSliderValues() * c(-1, 1) + bbx[2, ] 

      plot(0, 0, type="n", asp=1, ylim=yrange, xlim=xrange)
      abline(v=xrange, col="grey50", lty=1)
      abline(h=yrange, col="grey50", lty=1)
      polygon(c(xrange, rev(xrange)), rep(yrange, each=2), col=adjustcolor("red", 0.5), border="grey50")
      points(rhrPoints(data4()), col=adjustcolor("black", 0.1), pch=19)
    } else {
      plot(0,0, type="n")
    }
  })
  
  output$bufferUI <- renderUI({
    if (!is.null(data4())) {
      bbx <- rhrBBX(data4())
      xrange <- diff(bbx[1, ]) 
      yrange <- diff(bbx[2, ]) 
      uis <- list(
        sliderInput("bufferXSlider", "X-Buffer", 0, xrange, xrange * 0.5), 
        sliderInput("bufferYSlider", "Y-Buffer", 0, yrange, yrange * 0.5)
      )
    } else {
      return(NULL)
    }
  })

 # Reactive slider values
  bufferXSliderValues <- reactive({
    input$bufferXSlider
  }) 
  
  bufferYSliderValues <- reactive({
    input$bufferYSlider
  }) 

 # ------------------------------------------------------------------------------ ##  
 # Grid

 output$gridResUi <- renderUI({
   if (!is.null(data4())) {
    rgs <- apply(rhrBBX(data4()), 1, diff)
    rgs <- min(rgs) / 2000
    sliderInput("gridResSlider", "Resolution", rgs, rgs * 1000, round(mean(rgs)))
   }
 })

 trast <- reactive({
   if (!is.null(data4())) {

     ext <- rhrExtFromPoints(rhrPoints(data4()),
                             buffer=c(bufferXSliderValues(), bufferYSliderValues()),
                             extendRange=NULL) 
     if (input$configOutputGridGrid == "pixel") {
       return(rhrRasterFromExt(ext, nrow=input$gridNColSlider, ncol=input$gridNRowSlider, res=NULL))
     } else {
       return(rhrRasterFromExt(ext, nrow=NULL, ncol=NULL, res=ceiling(input$gridResSlider)))
     }
   } else {
     return(NULL)
   }

 })

 output$printGrid <- renderPrint({
   cat(
     " Number of rows:   ", nrow(trast()), "\n",
     "Number of columns: ", ncol(trast()), "\n",
     "Resolution:        ", paste0(raster::res(trast()),  collapse=", "), "\n",
     "Number of cells:   ", raster::ncell(trast())
   )
 })

 ## -------------------------------------------------------------------------- ##
 ## levels

 observe({
   xx <- tryCatch(all(is.numeric(rhr:::rhrCheckLevels(
     strsplit(input$configGlobalLevel, ",")[[1]]))), 
                  error = function(e) return(FALSE))
   
   if (!xx) {
     updateTextInput(session, "configGlobalLevel", value = "95")
   }
 })


 # ------------------------------------------------------------------------------ ##  
# locoh

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

  ## ------------------------------------------------------------------------------ ##  
  ## KDE

  output$configKDEbandwidthUserInput <- renderUI({
    if ("user" %in% input$configKDEbandwidth) {
      list(
        numericInput("configKDEbandwithUser", "Bandwidth (manual)", value=100)
      )
    } else {
      NULL
    }
  }) 

 ## ============================================================================= ##
 ## Analzye

 ## Check we have date+time for the estimators that need it
  observe({
    if (is.null(data4())) {
      updateButton(session, "rhrAnalyze", disable=FALSE)
    } 
  })

  observe({
    if (!is.null(data4())) {
      updateButton(session, "rhrAnalyze", disable=FALSE)
       if (any(c("rhrTTSI", "rhrBBMM") %in% c(input$runSteps, input$runSteps2))) {
         if ("rhrTTSI" %in% c(input$runSteps, input$runSteps2) & !is(data4(), "RhrTracksST")) {
           createAlert(session, "rhrRunNoTimeTTSI", "rhrRunNoTimeTTSI1",
                       "Time to statistical independence",
                       content = "Date and time are required", 
                       dismiss=FALSE,
                       append=FALSE)
           updateButton(session, "rhrAnalyze", disable=TRUE)
         } else {
           closeAlert(session, "rhrRunNoTimeTTSI1")
         }
         if ("rhrBBMM" %in% c(input$runSteps, input$runSteps2) & !is(data4(), "RhrTracksST")) {
           createAlert(session, "rhrRunNoTimeBBMM", "rhrRunNoTimeBBMM1",
                       "Brownian Bridges Movement Model",
                       content = "Date and time are required", 
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
  
  report <- reactiveValues(runid = NULL)
  

  observe({
    if (input$rhrAnalyze == 0) {
      return(NULL)
    }

    input$rhrAnalyze
    
    isolate({
      if (!is.null(data4())) {
        closeAlert(session, "rhrAnalyzeInfo1")

        runId <- paste0("rhr_run_", format(now(), "%Y%m%d%H%M%S"))
        outDir <- normalizePath(file.path(normalizePath(outdir_base, mustWork=FALSE, winslash="/"), runId), mustWork=FALSE, winslash="/")
        dir.create(outDir)

        ## Create args
        args <- list(
          rhrSiteFidelity=list(
            n=input$configSiteFidelityN,
            alpha=input$configSiteFidelityAlpha), 
          rhrKDE=list(
            levels=rhrCorrectLevels(input$configGlobalLevel),
            trast=trast(), 
            h=input$configKDEbandwidth,
            userh=input$configKDEbandwithUser), 
          rhrBBMM=list(
            levels=rhrCorrectLevels(input$configGlobalLevel),
            trast=trast(), 
            sigma2=input$configBBMMSigma2, 
            rangesigma1=input$configBBMMRangeSigma1), 
          rhrMCP=list(
            levels=rhrCorrectLevels(input$configGlobalLevel)
          ), 
          rhrTTSI=list(
            interval=input$configTTSIInterval,
            consec=input$configTTSISampling,
            ntimes=input$configTTSINTimes,
            alpha=input$configTTSIAlpha
          ),
          rhrLoCoH=list(
            levels=rhrCorrectLevels(input$configGlobalLevel),
            n=do.call(base::c, list(
              switch(input$configLOCOHtypeK, "inclm" = input$configLOCOHtypeKmanN, "incla" = 10),
              switch(input$configLOCOHtypeA, "inclm" = input$configLOCOHtypeAmanN, "incla" = 10),
              switch(input$configLOCOHtypeR, "inclm" = input$configLOCOHtypeRmanN, "incla" = 10))), 
            autoN=do.call(base::c, list(
              switch(input$configLOCOHtypeK, "incla" = TRUE, "inclm" = FALSE),
              switch(input$configLOCOHtypeA, "incla" = TRUE, "inclm" = FALSE),
              switch(input$configLOCOHtypeR, "incla" = TRUE, "inclm" = FALSE))),
            type=do.call(base::c, list(
              switch(input$configLOCOHtypeK, "incla" = "k", "inclm" = "k", "not" = NULL),
              switch(input$configLOCOHtypeA, "incla" = "a", "inclm" = "a", "not" = NULL),
              switch(input$configLOCOHtypeR, "incla" = "r", "inclm" = "r", "not" = NULL)))
          ), 
          rhrAsymptote=list(
            include=input$configAsymptoteInclude, 
            minNP=input$configAsymptoteMinNP,
            si=input$configAsymptoteSI,
            nrep=input$configAsymptoteNRep,
            tolTotArea=input$configAsymptoteTolToA,
            nTimes=input$configAsymptoteNTimes,
            sampling=input$configAsymptoteSampling
          ),
          rhrUniCirc=list(
            levels=rhrCorrectLevels(input$configGlobalLevel),
            trast=trast()
          ),
          rhrBiCirc=list(
            levels=rhrCorrectLevels(input$configGlobalLevel),
            trast=trast()
          ), 
          rhrUniNorm=list(
            levels=rhrCorrectLevels(input$configGlobalLevel),
            trast=trast()
          ),
          rhrBiNorm=list(
            levels=rhrCorrectLevels(input$configGlobalLevel),
            trast=trast()
          ), 
         rhrCoreArea = list(
           include = input$configCAInclude
         ) 
        )
        
        closeAlert(session, "rhrAnalyzeProgress1") 
        closeAlert(session, "rhrAnalyzeProgress2") 
        closeAlert(session, "rhrAnalyzeProgress3") 
        closeAlert(session, "rhrAnalyzeProgress4") 
        closeAlert(session, "rhrAnalyzeProgress5") 

        createAlert(session, "rhrAnalyzeProgress", "rhrAnalyzeProgress1",
                    "Starting Analysis",
                    content = paste0("[", Sys.time(), "] Preparing the analysis"), 
                    style="info", 
                    dismiss=FALSE,
                    append=FALSE)

        withProgress(message="Starting", {
          setProgress(message="Preparing calculations", detail="creating output files .....")
          files <- system.file("guiTemp", package="rhr")
          createAlert(session, "rhrAnalyzeProgress", "rhrAnalyzeProgress2",
                      "Starting Calculations",
                      content = paste0("[", Sys.time(), "] Starting with calculations, this may take some time"), 
                      dismiss=FALSE,
                      append=TRUE)
          ## ------------------------------------------------------------------------------ ##  
          ## Run the whole analysis
          starttime <- Sys.time()
          runtime <- system.time(res <- rhrHrAnalysis(data4(),
                                                      what=input$runSteps, 
                                                      args=args,
                                                      outDir=outDir,
                                                      inUnit=input$configOutputInUnits, 
                                                      outUnit=input$configOutputOutUnits, 
                                                      inGUI=TRUE, zip = dozip, 
                                                      report = TRUE, 
                                                      repArgs = list(
                                                        doCp = if(configInter()$general$content$doCp) TRUE else NULL,
                                                        outDir = outDir,
                                                        wd = configInter()$general$content$wd,
                                                        hraas = hraas)
                                                      ), gcFirst=TRUE)
          
          ## ------------------------------------------------------------------------------ ##  
          ## Brew html

          createAlert(session, "rhrAnalyzeProgress", "rhrAnalyzeProgress3",
                      "Report",
                      content = paste0("[", Sys.time(), "] Generating report"), 
                      style="info", 
                      dismiss=FALSE,
                      append=TRUE)
            
          
          # copy everything
          if (configInter()$general$content$doCp) {
            dir.create(normalizePath(file.path(configInter()$general$content$wd, runId), mustWork=FALSE, winslash="/"))
            filesNames <- list.files(outDir, full.names=TRUE, recursive=FALSE, ignore.case=TRUE)
            sapply(filesNames, function(x)
              file.copy(from=x, to=normalizePath(file.path(configInter()$general$content$wd, runId), mustWork=FALSE, winslash="/")))
            #
            for (f in c("data", "plots", "vector", "raster")) {
              dir.create(normalizePath(file.path(configInter()$general$content$wd, runId, f), mustWork=FALSE, winslash="/"))
              filesNames <- list.files(normalizePath(file.path(outDir, "results", f), mustWork=FALSE, winslash="/"), full.names=TRUE, recursive=TRUE,
                                       ignore.case=TRUE)
              sapply(filesNames, function(x)
                file.copy(from=x, to=normalizePath(file.path(configInter()$general$content$wd, runId, f), mustWork=FALSE, winslash="/")))
            }
            
          }
          
          createAlert(session, "rhrAnalyzeProgress", "rhrAnalyzeProgress5",
                      "Finish",
                      content =paste0("[", Sys.time(), "] Finished analysis"), 
                      style="info", 
                      dismiss=FALSE,
                      append=TRUE)
          
        })
        
        
        if (dozip) {
          report$runId <- runId
        } else {
          browseURL(normalizePath(file.path(outDir, "rhrReport.html"), mustWork=FALSE, winslash="/"))
        }
      } else {
        createAlert(session, "rhrAnalyzeInfo", "rhrAnalyzeInfo1",
                    "Not ready yet",
                    content ="Please load and remap the data before you try to run an analysis", 
                    dismiss=TRUE,
                    append=FALSE)
      }
    })
  })
  
  
  output$dzip <- downloadHandler(
    filename = paste0(report$runId, ".zip"),
    content = function(file) file.copy(file.path(outdir_base, paste0(report$runId, ".zip")), file, overwrite = TRUE)
  )
  
  output$drep <- downloadHandler(
    filename = paste0(report$runId, ".html"),
    content = function(file) file.copy(file.path(outdir_base, report$runId, "rhrReport.html"), file, overwrite = TRUE)
  )
  
  
  output$result <- renderUI({
    if (!is.null(report$runId)) {
      list( 
        downloadButton("dzip", "Download Results"),
        downloadButton("drep", "Download Report")
      )
    }
  })
})
