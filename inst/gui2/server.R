
## For the packaged version: make an optinal data argument and
## outputDir argument. They will be set static and be identical for each client.
## This shouldn't be a problem, if the app is run locally

## clean everything 
rm(list=ls())
debug <- TRUE
if (debug) {
  .datFromR <- NULL
  outDir <- "/tmp"
  setwd("~/201_projects/rhr2/pkg/rhr/inst/gui")
}

## Max upload size
options(shiny.maxRequestSize=30*1024^2)
#####

library(brew)
library(lubridate)
library(knitr)
library(markdown)
library(rgdal)
library(rhr)
library(shinyBS)
library(shiny)
library(xtable)

shinyServer(function(input, output, session) {
  
  ## ============================================================================== ##  
  ## Set up
  for (f in list.files(c("uiParts", "serverParts"), "*.R", full.names = TRUE)) {
    cat(f, "\n")
    source(f, local= TRUE)
  }
  source("global.R", local = TRUE)
  
  ##  runId <- paste0("rhr-run-", format(now(), "%Y%m%d%H%M%S"))
  ##  outDir <- normalizePath(file.path(normalizePath(tempdir(), winslash="/"), runId), winslash="/")
  ##  dir.create(outDir)
  ##  addResourcePath("out", outDir)


  ## ============================================================================== ##  
  ## Read data: serverParts/readData.R

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
    if (!is.null(data()$data) & data()$exitStatus == 0) {
      TRUE
    } else {
      FALSE
    }
  })
  

#  ## ------------------------------------------------------------------------------ ##  
#  ## check for EPSG codes
#
#
#  output$reproject <- renderUI({
#    if (rhrValidEpsg(input$configInEpsg)) {
#      return(list(
#        helpText(paste0("Optionally you can reproject your data (Input EPSG is: ",
#                        input$configInEpsg, ")")), 
#        numericInput("configOutEpsg", "Output EPSG", NA), 
#        textOutput("rhrReproject")))
#    } else {
#      return(list(
#        helpText("Invalid or no input EPSG provided")
#      ))
#    }
#  })
#
#  ##
#  ## Remapping of the fields happens here
#
  ## ============================================================================== ##  
  ## Subset data: uiParts/subsetData.R
  
  
  #  ## Reset button: not yet implemented
  #  ## observe({
  #  ##   if (!is.null(data3())){
  #  ##     if (input$subsetReset == 0) {
  #  ##       cat("[e] ========over 1 \n")
  #  ##       return(NULL)
  #  ##     }
  #  ##     cat("[e] ========over 2 \n")
  #  ##       updateSliderInput(session, "subsetXSlider", "X-Range", value=range(data2()$lon))
  #  ##       updateSliderInput(session, "subsetYSlider", "Y-Range", value=range(data2()$lat))
  
  #  ##       if (FALSE){
  #  ##         if (!all(is.na(data2()$timestamp))) {
  #  ##           updateDateRangeInput(session, "subsetDatePicker", "Date range:",
  #  ##                                start = format(min(data2()$timestamp), "%Y-%m-%d"),
  #  ##                                end = format(max(data2()$timestamp), "%Y-%m-%d"))
  #  ##         }
  #
  #  ##         ## select ids
  #  ##         pcho <- unique(data2()$id)
  #
  #  ##         updateSelectInput(session, "subsetSelectedIds", "Select animals",
  #  ##                           choices=pcho,
  #  ##                           selected=pcho,
  #  ##                           multiple=TRUE,
  #  ##                           selectize=FALSE)
  #  ##       }
  #  ##   }
  #  ## })
  
  
  ## ============================================================================= ##
  ## Analzye



})
