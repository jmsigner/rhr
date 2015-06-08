library(shinyBS)
library(shiny)

shinyUI(
  navbarPage("Reproducible Home Range Analysis with R",
             ## ============================================================================== ##  
             ## Read Files
             tabPanel("Load Data", uiOutput("readData")),
             
             ## ============================================================================== ##  
             ## Remap fields
             tabPanel("Map and Project Data", #uiOutput("mapDataUI")), 
                      sidebarLayout(
                        sidebarPanel(
                          h2("Select Fields"), 
                          selectInput("mfId", "Id", choices = NA),
                          selectInput("mfX", "Longitude (X)", choices = NA),
                          selectInput("mfY", "Latitude (Y)", choices = NA),
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
                          uiOutput("data2")
                        )
                      )),
             
             ## ============================================================================== ##  
             ## Subset Data
             tabPanel("Subset Data", uiOutput("subsetDataUI")), 
             
             ## ============================================================================== ##  
             ## Configure
             navbarMenu("Configure", 
                        tabPanel("General", uiOutput("configureGeneralUI")),
                        tabPanel("Exploratory analysis", uiOutput("configureExploratory")),
                        tabPanel("Home Ranges", uiOutput("configureHR")),
                        tabPanel("Core Area", uiOutput("configureCA"))
                        ## tablPanel("Path Level Analysis)
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
                               #bsButton("rhrAnalyze", label="Analyze", disabled=TRUE, size="large", style="primary")
                               actionButton("rhrAnalyze", "Analyze")
                        ),
                        column(width=5,
                               bsAlert("rhrAnalyzeInfo"), 
                               bsAlert("rhrRunNoTimeTTSI"), 
                               bsAlert("rhrRunNoTimeBBMM"),
                               bsAlert("rhrAnalyzeProgress")
                        ))
             
             ),
             inverse=TRUE)
)

