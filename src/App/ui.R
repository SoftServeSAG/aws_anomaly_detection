library(shiny)
library(DT)
library(shinyBS)
library(dygraphs)

shinyUI(
    fluidPage(
        shinyjs::useShinyjs(),
        navbarPage("Anomaly Detection", id = "MNB",
            tabPanel("Load",
                fluidPage(
                    fluidRow(
                        column(3,
                               h3("Load data"),
                               br(),
                               fileInput("LocalFile", NULL),
                               hr(),
                               h3("CSV options"),
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
                        ),
                        column(9,
                               h2("Preview"),
                               DT::dataTableOutput('UserData'),
                               conditionalPanel(
                                   condition = "output.UserDataLoaded",
                                   fluidRow(
                                       column(3,
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
                                                         textInput("DateTimeFmt", NULL, placeholder = "HH:MM:SS"),
                                                         bsPopover("DateTimeFmt", "Datetime format", "Specify datetime format for time scale", placement="top")
                                                  )
                                              )
                                       ),
                                       column(3,
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
                    )
                ),
                fluidRow(
                    conditionalPanel(
                        condition = "output.UserDataLoaded",
                        column(1, offset = 11,
                               shinyjs::disabled(actionButton("Next1Btn", "Next", width = '100%'))
                        )
                    )
                )
            ),
            tabPanel(
                "Prepare",
                fluidPage(
                    fluidRow(
                        column(3,
                            h3("Transformation"),
                            hr(),
                            fluidRow(
                                column(6,
                                       div(strong("Missing values"), style = "padding: 5px;font-size: 110%;")
                                ),
                                column(6,
                                       selectizeInput("MissValTreatment", NULL, choices = c('Zero'='zero', 'Linear'='linear', 'Mean'='mean'),
                                                      options = list(
                                                          placeholder = "None",
                                                          onInitialize = I('function() { this.setValue(""); }')
                                                      ))
                                )
                            ),
                            hr(),
                            fluidRow(
                                column(6,
                                       div(strong("Outliers Detection"), style = "padding: 5px;font-size: 110%;")
                                ),
                                column(6,
                                       selectizeInput("OutliersTreatment", NULL, choices = c('Normal'='normal', 'Diff'='diff'),
                                                      options = list(
                                                          placeholder = "None",
                                                          onInitialize = I('function() { this.setValue(""); }')
                                                      ))
                                )
                            ),
                            hr(),
                            fluidRow(
                                column(6,
                                       div(strong("Noise Reduction"), style = "padding: 5px;font-size: 110%;")
                                ),
                                column(6,
                                       selectizeInput("NoiseTreatment", NULL, choices = c('Gaussian'='gaussian', 'SMA'='sma','EMA'='ema', 'DEMA'='dema','WMA'='wma'),
                                                      options = list(
                                                          placeholder = "None",
                                                          onInitialize = I('function() { this.setValue(""); }')
                                                      ))
                                )
                            ),
                            hr(), hr(),
                            h3("Aggregation"),
                            hr(),
                            fluidRow( # TODO: unfinished
                                column(6,
                                       div(strong("Function"), style = "padding: 5px;font-size: 110%;")
                                ),
                                column(6,
                                       selectizeInput("AggFunction", NULL, choices = c('Sum'='sum', 'Min'='min', 'Avg'='mean', 'Median'='median', 'Max'='max'),
                                                      options = list(
                                                          placeholder = "None",
                                                          onInitialize = I('function() { this.setValue(""); }')
                                                      ))
                                )
                            ),
                            fluidRow(
                                column(4, offset=4,
                                       actionButton("ApplyTsfBtn", "Apply", width = '100%')
                                )
                            )
                        ),
                        column(9,
                            wellPanel(id="PrepareResPanel", style = "overflow-y:scroll;height:80%;", #TODO: figure out height options
                                      h3("Original"),
                                      dygraphOutput("OriginalSeries"),
                                      br(),
                                      tags$div(id='tranform-res-placeholder')
                            )
                        )
                    ),
                    fluidRow(
                        column(1, offset=11,
                               actionButton("Next2Btn", "Next", width = '100%')
                        )
                    )
                )
            ),
            tabPanel(
                "Model"
            )
        )
    )
)
