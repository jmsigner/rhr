output$readData <- renderUI(sidebarLayout(
  sidebarPanel(  
    h2("Relocations"), 
    selectInput("readFileFieldSep", "Field separator",
                choices = c("Comma (,)" = "comma", "Semicolon (;)" = "semi", "Tab (\t)" = "tab")), 
    selectInput("readFileSepDec", "Decimal Separator",
                choices = c("Point (.)" = "point", "Comma (,)" = "comma")), 
    numericInput("readFileSkipLines", "Skip Lines", value=0, min=0, max=NA),
    checkboxInput("readFileHasHeader", "First line is a header", value=TRUE), 
    fileInput("readFileFile", "Relocations", multiple = FALSE,
              accept = NULL), 
    hr(),  
    numericInput("configInEpsg", "Input EPSG", NA), 
    hr()
    ## h2("Habitat data"), 
    ## fileInput("inputhafiles", "Habitat Files", multiple = TRUE,
    ##          accept = NULL)
  ),
  mainPanel(
    uiOutput("readFileTable")
  )
)
)

data <- reactive({
  if (is.null(.datFromR)) {
    res <- paste0('Unable to load data, did you select a file?')
    dat <- NULL
    exitStatus <- 1
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
      if (!is(dat, "error")) {
        res <- "Data successfully read"
        exitStatus <- 0
      } else {
        res <- paste0("Something went wrong:", dat$message)
        exitStatus <- 1
      }
    } 
    list(data=dat, message=res, exitStatus=exitStatus)
  } else {
    list(data=.datFromR, message="Data read from R", exitStatus=0)
  }
})
