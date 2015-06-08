output$configureGeneralUI <- renderUI(
  fluidPage(
    navlistPanel("General",
                 tabPanel("Output",
                          h2("Set output options"), 
                          hr(), 
                          checkboxInput("configOutputMkPdf", "Generate a pdf file"),
                          helpText("This requires a working LaTeX installation on your machine"), 
                          hr(), 
                          checkboxInput("configOutputCpWd", "Copy all results to the current working directory",
                                        value=FALSE),
                          helpText("All files will be copied into a new directory in current working directory"), 
                          hr()
                          ## checkboxInput("configOutputZip", "Compress all results into a zip file", value=TRUE)
                 ), 
                 tabPanel("Units",
                          selectInput("configOutputInUnits", "Input units are:",
                                      choices=c("I don't know" = "ido", "meters" = "m", "kilometers" = "km"), selectize=FALSE),
                          selectInput("configOutputOutUnits", "Desired output units are:",
                                      choices=c("square meters" = "sqm", "hectars" = "ha", "square km" = "sqkm"), selectize=FALSE)
                 ), 
                 tabPanel("Output Grid",
                          h2("Specify a grid"), 
                          helpText("Some estimators (e.g. Kernel Density Estimation) require a grid for the results. Here you can specify this grid. The same grid will be used for all animals in the analysis."), 
                          h2("Buffer"),
                          h3("Specify a buffer"),
                          uiOutput("bufferUI"), 
                          helpText("The buffer is in output units"), 
                          h3("Currently used buffer"), 
                          plotOutput("configOuputGridBufferPlot"), 
                          h2("Specify resolution and number of rows/columns"),
                          helpText("A grid can be created based on either number of rows and number of columns, or based on a provided resolution. Note, at the moment only quadratic pixels are supported, this means that the actual number of rows/columns, maybe slightly different to the one provided."),
                          selectInput("configOutputGridGrid", "Create output grid based on:", choices=c("number of rows and columns" = "pixel", "resolution" = "res"), 
                                      selectize=FALSE),
                          conditionalPanel(
                            condition="input.configOutputGridGrid == 'pixel'",
                            sliderInput("gridNColSlider", "Number of columns", 10, 500, 100), 
                            sliderInput("gridNRowSlider", "Number of rows", 10, 500, 100)
                          ), 
                          conditionalPanel(
                            condition="input.configOutputGridGrid == 'res'",
                            uiOutput("gridResUi")
                          ), 
                          h3("Currently used grid"),
                          verbatimTextOutput("printGrid")
                 ), 
                 tabPanel("Levels",
                          h2("Specify levels"), 
                          helpText("Some estimators (e.g. Kernel Density Estimation) require a grid for the results. Here you can specify this grid. The same grid will be used for all animals in the analysis."), 
                          helpText("Levels can be specified either as single number or comma seperated lists (e.g., '10,50,95'). Range from 1 to 100 is permitted, all other values will default back to 95"),
                          textInput("configGlobalLevel", "Levels:", "50,95")
                 ), 
                 well=FALSE)))


# Grid --------------------------------------------------------------------

output$configOuputGridBufferPlot <- renderPlot({
  if (!is.null(data4())) {
    bbx <- bbox(data4()$dat)
    xrange <- bufferXSliderValues() * c(-1, 1) + bbx[1, ]
    yrange <- bufferYSliderValues() * c(-1, 1) + bbx[2, ] 
    
    plot(data4()$dat, type="n", asp=1, ylim=yrange, xlim=xrange)
    abline(v=xrange, col="grey50", lty=1)
    abline(h=yrange, col="grey50", lty=1)
    polygon(c(xrange, rev(xrange)), rep(yrange, each=2), col=adjustcolor("red", 0.5), border="grey50")
    points(data4()$dat, col=adjustcolor("black", 0.1), pch=19)
  } else {
    plot(0,0, type="n")
  }
})

output$bufferUI <- renderUI({
  if (!is.null(data4())) {
    bbx <- bbox(data4()$dat)
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
    rgs <- apply(bbox(data4()$dat), 1, diff)
    rgs <- c(rgs / 10, rgs / 500)
    sliderInput("gridResSlider", "Resolution", 1, 1000, round(mean(rgs)))
  }
})

trast <- reactive({
  if (!is.null(data4())) {
    
    bbx <- bbox(data4()$dat)
    
    xrange <- if (is.null(bufferXSliderValues())) {
      diff(bbx[1, ])
    } else {
      bufferXSliderValues()
    }
  
    yrange <- if (is.null(bufferYSliderValues())) {
      diff(bbx[2, ])
    } else {
      bufferYSliderValues()
    }
    
    res <- if (is.null(input$gridResSlider)) {
      ceiling(apply(bbx, 1, diff) / 100)
    } else {
      input$gridResSlider
    }
    
    ext <- rhrExtFromPoints(data4()$dat,
                            buffer=c(xrange, yrange),
                            extendRange=NULL) 
    
    if (is.null(input$configOutputGridGrid)) {
      return(rhrRasterFromExt(ext, nrow=NULL, ncol=NULL, res=ceiling(res)))
    } else {
      if (input$configOutputGridGrid == "pixel") {
        return(rhrRasterFromExt(ext, nrow=input$gridNColSlider, ncol=input$gridNRowSlider, res=NULL))
      } else {
        return(rhrRasterFromExt(ext, nrow=NULL, ncol=NULL, res=ceiling(res)))
      }
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


# Levels ------------------------------------------------------------------

observe({
  xx <- tryCatch(all(is.numeric(rhr:::rhrCheckLevels(
    strsplit(input$configGlobalLevel, ",")[[1]]))), 
    error = function(e) return(FALSE))
  
  if (!xx) {
    updateTextInput(session, "configGlobalLevel", value = "95")
  }
})

