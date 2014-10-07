library(shinyBS)
library(shiny)

shinyUI(
  navbarPage("Reproducible Home Range Analysis with R",
             ## ============================================================================== ##  
             ## Read Files
             tabPanel("Load Data",
                      sidebarLayout(
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
                          bsAlert("alertLoadData"),
                          uiOutput("readFileTable")
                          ))),

             ## ============================================================================== ##  
             ## Remap fields
             tabPanel("Map and Project Data", 
                      sidebarLayout(
                        sidebarPanel(
                          h2("Select Fields"), 
                          selectInput("mfId", "Id", choices=NULL), 
                          selectInput("mfX", "Longitude (X)", choices=NULL), 
                          selectInput("mfY", "Latitude (Y)", choices=NULL), 
                          selectInput("mfDate", "Date", choices=NULL), 
                          selectInput("mfTime", "Time", choices=NULL), 
                          hr(), 
                          selectInput("mfDateFormat", "Date format", choices=NULL), 
                          selectInput("mfTimeFormat", "Time format", choices=NULL)

                          ),
                        mainPanel(
                          h2("Reproject Data"),
                          uiOutput("reproject"), 
                          bsAlert("rhrReproject"),
                          hr(),
                          bsAlert("alertMapFields"),
                          uiOutput("mfUI"))
                        )),

             ## ============================================================================== ##  
             ## Subset Data
             tabPanel("Subset Data",
                      sidebarLayout(
                        sidebarPanel(
                          uiOutput("subsetUI")),
                        mainPanel(
                          plotOutput("subsetPlot"),
                          dataTableOutput("subsetTable")
                          ))), 

             ## ============================================================================== ##  
             ## Configure
             navbarMenu("Configure",
                        tabPanel("General", 
                                 fluidPage(
                                   navlistPanel("General",
                                                tabPanel("Output",
                                                         h2("Set output options"), 
                                                         hr(), 
                                                         checkboxInput("configOutputMkPdf", "Generate a pdf file"),
                                                         helpText("This requires a working LaTeX installation on your machine"), 
                                                         hr(), 
                                                         checkboxInput("configOutputCpWd", "Copy all results to the current working directory",
                                                                       value=TRUE),
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
                                                         h2("Currently selected grid"),
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
                                                         helpText("Kernel density estimate (href) of 100 randomly selected points (among all ids) to preview the chosen grid."), 
                                                         plotOutput("gridPlot"), 
                                                         verbatimTextOutput("printGrid")
                                                         ), 
                                                tabPanel("Levels",
                                                         h2("Specify levels"), 
                                                         helpText("Some estimators (e.g. Kernel Density Estimation) require a grid for the results. Here you can specify this grid. The same grid will be used for all animals in the analysis."), 
                                                         helpText("Levels"),
                                                         textInput("configGlobalLevel", "Levels:", "50,95")
                                                         ), 
                                                well=FALSE)
                                   )), 
                        tabPanel("Exploratory analysis", 
                                 fluidPage(
                                   navlistPanel("Configure exploratory analysis",
                                                tabPanel("Site Fidelity",
                                                         h2("Site Fidelity"), 
                                                         numericInput("configSiteFidelityN", "Number of bootstrap replicates", value=config$pointLevel$sf$n, min=1), 
                                                         sliderInput("configSiteFidelityAlpha", "Alpha", min=0, max=1, step=0.01, value=config$pointLevel$sf$alpha)), 
                                                tabPanel("Time to statistical independence",
                                                         h2("Time to statistical independence"),
                                                         bsAlert("generalNoTimeTTSI"), 
                                                         numericInput("configTTSIInit", "Initial time difference", value=config$pointLevel$ttsi$interval),
                                                         helpText("Initial time difference in seconds"), 
                                                         selectInput("configTTSISampling", "Sampling regime", choices=config$pointLevel$ttsi$sampling), 
                                                         numericInput("configTTSINTimes", "Number of time above critical value", value=config$pointLevel$ttsi$ntimes)
                                                      ##   sliderInput("configTTSINAlpha", "Alpha", value=config$pointLevel$ttsi$alpha, min=0, max=1, step=0.01)
                                                         ), 
                                                well=FALSE)
                                   )), 
                        tabPanel("Home Ranges", 
                                 fluidPage(
                                   navlistPanel("Configure Home Ranges",
                                                tabPanel("Unimodal Bivariate Normal",
                                                         h2("Estimate unimodal bivariate normal home ranges")
                                                         ), 
                                                tabPanel("Bimodal Bivariate Normal",
                                                         h2("Estimate bimodal bivariate normal home ranges")
                                                         ), 
                                                tabPanel("Minimum Convex Polygon",
                                                         h2("Configure Minimum Convex Polygon")
                                                         ),
                                                tabPanel("Kernel Density Estimation",
                                                         h2("Kernel Density Estimation"),
                                                         selectInput("configKDEbandwidth", "Bandwidth", choices=config$homeRange$kde$bandwidthOptions,
                                                                     multiple=TRUE, selectize=FALSE),
                                                         helpText("Use CTRL to select several options"), 
                                                         uiOutput("configKDEbandwidthUserInput")
                                                         ),
                                                tabPanel("Local Convex Polygon",
                                                         h2("Local Convex Polygon"),
                                                         hr(),
                                                         helpText("'n' is determined automatically, see documentation for more information"), 

                                                         selectInput("configLOCOHtypeK", "Type k",
                                                                     choices=c("Include (automatic)" = "incla",
                                                                       "Include (manual)" = "inclm", 
                                                                       "Do not include" = "not"),
                                                                     selected="incla", selectize=FALSE),
                                                         uiOutput("configLOCOHtypeKField"),
                                                         selectInput("configLOCOHtypeA", "Type a",
                                                                     choices=c(
                                                                       "Include (automatic)" = "incla",
                                                                       "Include (manual)" = "inclm", 
                                                                       "Do not include" = "not"),
                                                                     selected="not", selectize=FALSE), 
                                                         uiOutput("configLOCOHtypeAField"),

                                                         selectInput("configLOCOHtypeR", "Type r",
                                                                     choices=c("Include (automatic)" = "incla",
                                                                       "Include (manual)" = "inclm", 
                                                                       "Do not include" = "not"),
                                                                     selectize=FALSE, selected="not"), 
                                                         uiOutput("configLOCOHtypeRField"),
                                                         hr()
                                                         ), 

                                                tabPanel("Home range asymptote",
                                                         h2("Home range asymptote"),
                                                         numericInput("configAsymptoteMinNP", "Minimum number of points",
                                                                      value=config$homeRange$asymptote$minNP), 
                                                         numericInput("configAsymptoteSI", "Sampling interval",
                                                                      value=config$homeRange$asymptote$SI), 
                                                         numericInput("configAsymptoteNRep", "Number of replications",
                                                                      value=config$homeRange$asymptote$NRep), 
                                                         sliderInput("configAsymptoteTotA", "Tolerance to total area",
                                                                     min=0, max=1, step=0.01,
                                                                      value=config$homeRange$asymptote$TotA), 
                                                        ## sliderInput("configAsymptoteAlpha", "Alpha for confidence interval",
                                                        ##             min=0, max=1, step=0.01,
                                                        ##             value=config$homeRange$asymptote$TotA), 
                                                         numericInput("configAsymptoteNTimes", "Number of time breakup met",
                                                                      value=config$homeRange$asymptote$NTimes), 
                                                         selectInput("configAsymptoteSampling", "Sampling regime",
                                                                     choices=config$homeRange$asymptote$sampling, 
                                                                     multiple=FALSE, selectize=FALSE),
                                                         selectInput("configAsymptoteEstmators", "Estimators",
                                                                     choices=config$homeRange$asymptote$estimators,
                                                                     multiple=TRUE, selectize=FALSE)
                                                         ), 
                                                ## tabPanel("Synoptic Home Ranges",
                                                ##         h1("Not yet implemented")), 
                                                tabPanel("Brownian Bridges",
                                                         h2("Estimate home ranges with Brownian Bridges"),
                                                         bsAlert("generalNoTimeBBMM"), 
                                                         numericInput("configBBMMSigma2", "Location Error (Sigma 2)", value=10),
                                                         sliderInput("configBBMMRangeSigma1", "Search range for sigma 1", value=c(1, 1e4),
                                                                     min=1, max=1e4)
                                                         ), 
                                                well=FALSE)
                                   )), 
                        tabPanel("Core Area", 
                                 fluidPage(
                                   navlistPanel("Configure Core Area",
                                                tabPanel("Method of Powell and Seaman",
                                                         well=FALSE), 
                                                h2("Method of Powell and Seaman"),
                                                helpText("No additional configuration is required, core area is calculated from kernel density estimation")
                                   ))) 
                        ## tabPanel("Path Level Analysis", 
                                 ## fluidPage(
                                   ## navlistPanel("Configure Path Level Analysis",
                                                ## tabPanel("Method 1",
                                                         ## h1("MCP")), 
                                                ## tabPanel("Method 2",
                                                         ## h1("MCP")), 
                                                ## well=FALSE)
                                   ## )) 
                        ),
             tabPanel("Run Analysis",
                      fluidRow(
                        column(width=5, 
### FLuid 2 sides, one with steps, one with warnings, etc
                               h2("Select analytical steps"),
                               helpText("..."), 
                               h3("Home range analysis"), 
                               checkboxGroupInput("runSteps", "Select Steps",
                                                  choices=c(
                                                    "Site Fidelity" = "rhrSiteFidelity", 
                                                    "Time to statistical independence" = "rhrTTSI", 
                                                    "Minimum Convex Polygon" = "rhrMCP",
                                                    "Kernel Density Estimation" = "rhrKDE", 
                                                    "Local Convex Hull" = "rhrLoCoH",
                                                    "Home Range Asymptote" = "rhrAsymptote",
                                                    "Brownian Bridges" = "rhrBBMM",
                                                    "Unimodal Bivariate Normal" = "rhrUniNorm",
                                                    "Bimodal Bivariate Normal" = "rhrBiNorm")),
                               h3("Core Area"), 
                               checkboxGroupInput("runSteps2", "Select Steps",
                                                  choices=c(
                                                    "Core Area" = "rhrCoreArea" )), 
                               bsActionButton("rhrAnalyze", label="Analyze", disabled=TRUE, size="large", style="primary")
                               ),
                        column(width=5,
                               bsAlert("rhrAnalyzeInfo"), 
                               bsAlert("rhrRunNoTimeTTSI"), 
                               bsAlert("rhrRunNoTimeBBMM"),
                               bsAlert("rhrAnalyzeProgress")
                               )
                        )
                      ),
             inverse=TRUE,
             footer=list(hr(), p("2014 - Wildlife Sciences - Georg-August-University Goettingen, citation"))))

