library(shiny)
library(DT)
library(shinyBS)
library(dygraphs)
library(shinydashboard)

shinyUI(
    
    fluidPage(
        tags$head(tags$style(
            HTML('
                            #sidebar01 {
                                        background-color: #FFFFFF;
                                        border-color: #4682b4;
                            }
                            #sidebar02 {
                                        background-color: #FFFFFF;
                                        border-color: #4682b4;
                                        }
                            #PrepasidebarreResPanel {
                                        background-color: #FFFFFF;
                                        border-color: #4682b4;
                                     }    

                            body, label, input, button, select { 
                            font-family: "Arial";}')
        )),
        
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "shiny.css")
        ),
        shinyjs::useShinyjs(),
        navbarPage("Anomaly Detection", id = "MNB",
            tabPanel("Load",
                fluidPage(
                    sidebarPanel(id="sidebar01",width = 3,
                    fluidRow(
                       
                        bsCollapse(id = "collapseLoad", open = "Load Data",multiple = T,
                        bsCollapsePanel (title = "Load Data", style = "primary", 
                            
                            fileInput("LocalFile", NULL),
                 
                            checkboxInput("FileHasHeader", "Header", TRUE),
                            radioButtons(
                                "FileSeparator", "Separator",
                                choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                                selected = ","
                            ),
                            radioButtons(
                                "FileQuotation", "Quote",
                                choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                                selected = ""
                            )
                            )))),
                mainPanel(
                    fluidRow( 
                            
                            wellPanel(id="PrepasidebarreResPanel",h2("Preview"),style = "overflow-y:scroll;height:80%;",
   
                               DT::dataTableOutput('UserData'),
                               conditionalPanel(
                                   condition = "output.UserDataLoaded",
                                   fluidRow(
                                       column(6,
                                              h3("Time Scale"),
                                              fluidRow(
                                                  column(6,
                                                         div("Column name", style = "padding: 5px;font-size: 110%;")
                                                  ),
                                                  column(6,
                                                         uiOutput("DateTimeVarName")
                                                  )
                                              ),
                                              fluidRow(
                                                  column(6,
                                                         div("Datetime format", style = "padding: 5px;font-size: 110%;")
                                                  ),
                                                  column(6,
                                                         selectInput("DateTimeCh", label=NULL, choices = c("Manualy", "YYYY-MM-DD" = "%Y-%m-%d",
                                                                                                           "YYYY-MM-DD HH:MM:SS" = "%Y-%m-%d %H:%M:%S",
                                                                                                           "YYYY/MM/DD" = "%Y/%m/%d", 
                                                                                                           "YYYY/MM/DD HH:MM:SS" = "%Y/%m/%d %H:%M:%S")),
                                                         uiOutput("DateTimeF")
                                                  )
                                              )
                                       ),
                                       column(6,
                                              h3("Target variable"),
                                              fluidRow(
                                                  # column(6,
                                                  #        #tags$div("Column name", style = "padding: 5px;font-size: 110%;")
                                                  # ),
                                                  column(6,# offset = 6,
                                                         uiOutput("TargetVarName")
                                                  )
                                              )
                                       )
                                   )
                               )
                        )
                    ),
                fluidRow(
                    conditionalPanel(
                        condition = "output.UserDataLoaded",
                        column(2, offset = 10,
                               shinyjs::disabled(actionButton("Next1Btn", "Next", width = '100%'))
                        )
                    )
                ))
            )),
            tabPanel("Prepare",
                fluidPage(
                    
                        sidebarPanel(id="sidebar02",width = 3,
                                     fluidRow(
                                         bsCollapse(id = "collapseTransform", open = "Tranformation",multiple = T,
                                                    
                                                    bsCollapsePanel("Tranformation",
                                                                    fluidRow(
                                                                        column(6,
                                                                               div(strong("Missing values"), style = "padding: 5px;font-size: 110%;")
                                                                        ),
                                                                        column(6,
                                                                               selectInput("MissValTreatment", NULL, choices = c('Zero'='zero', 'Linear'='linear', 'Mean'='mean'))
                                                                        )
                                                                    ),
                                                                    hr(),
                                                                    fluidRow(
                                                                        column(6,
                                                                               div(strong("Outliers Detection"), style = "padding: 5px;font-size: 110%;")
                                                                        ),
                                                                        column(6,
                                                                               selectInput("OutliersTreatment", NULL, choices = c('None' = 'none', 'Normal'='normal', 'Diff'='diff'))
                                                                        )
                                                                    ),
                                                                    conditionalPanel(
                                                                        condition = "input.OutliersTreatment != 'none'",
                                                                        fluidRow(
                                                                            column(6,
                                                                                   div("Count", style = "padding: 5px;font-size: 110%;")
                                                                            ),
                                                                            column(6,
                                                                                   sliderInput("OutliersSigmaCount", NULL, value = 5, min = 3, max = 7)
                                                                            )
                                                                        )
                                                                    ),
                                                                    hr(),
                                                                    fluidRow(
                                                                        column(6,
                                                                               div(strong("Noise Reduction"), style = "padding: 5px;font-size: 110%;")
                                                                        ),
                                                                        column(6,
                                                                               selectInput("NoiseTreatment", NULL, choices = c('None' = 'none', 'Gaussian'='gaussian', 'SMA'='sma','EMA'='ema'))
                                                                        )
                                                                    ),
                                                                    conditionalPanel(
                                                                        condition = "input.NoiseTreatment != 'none'",
                                                                        fluidRow(
                                                                            column(6,
                                                                                   div("Kernel size", style = "padding: 5px;font-size: 110%;")
                                                                            ),
                                                                            column(6,
                                                                                   selectInput("NoiseWindowType", NULL, choices = c('Auto'='auto', 'Manual'='manual'))
                                                                            )
                                                                        )
                                                                    ),
                                                                    conditionalPanel(
                                                                        condition = "input.NoiseTreatment != 'none' && input.NoiseWindowType != 'auto'",
                                                                        fluidRow(
                                                                            column(12,
                                                                                   sliderInput("NoiseWindowSize", NULL, value = 5, min = 3, max = 25, width = '100%')
                                                                            )
                                                                        )
                                                                    ),
                                                                    style = "primary"),
                                                    bsCollapsePanel("Aggregation",
                                                                    fluidRow(
                                                                        column(6,
                                                                               div(strong("Function"), style = "padding: 5px;font-size: 110%;")
                                                                        ),
                                                                        column(6,
                                                                               selectInput("AggFunction", NULL, choices = c('None'='none', 'Sum'='sum', 'Min'='min', 'Avg'='mean', 'Median'='median', 'Max'='max'))
                                                                        )
                                                                    ),
                                                                    conditionalPanel(
                                                                        condition = "input.AggFunction != 'none'",
                                                                        fluidRow(
                                                                            column(6,
                                                                                   div(strong("Unit"), style = "padding: 5px;font-size: 110%;")
                                                                            ),
                                                                            column(6,
                                                                                   uiOutput("AggUnitPlaceholder")
                                                                            )
                                                                        ),
                                                                        fluidRow(
                                                                            column(12,
                                                                                   uiOutput("AggCountPlaceholder")
                                                                            )
                                                                        )
                                                                    ),
                                                                    
                                                                    style = "primary")
                                         ),
                                         fluidRow(
                                             column(6, offset=3,
                                                    actionButton("ApplyTsfBtn", "Apply", width = '100%')
                                             )
                                         )
                                     )
                       

                        

             
                        
                        
                    ),
                    
                    mainPanel(
                    fluidRow(
                        
                        wellPanel(id="PrepasidebarreResPanel", style = "overflow-y:scroll;height:80%;", 
                                  h2("Original"),#TODO: figure out height options
                
                                  dygraphOutput("OriginalSeries"),
                                  br(),
                                  tags$div(id='tranform-res-placeholder')
                        )
                    ),
                    fluidRow(
                        column(2, offset=10,
                            actionButton("Next2Btn", "Next", width = '100%')
                        )
                    )
                ))
            ),
            tabPanel(
                "Model"
            )
        )
    )
)
