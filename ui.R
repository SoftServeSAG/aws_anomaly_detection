library(shiny)
library(DT)
library(shinyBS)
library(dygraphs)

shinyUI(
    fluidPage(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "shiny.css"),
            tags$link(rel = "stylesheet", type = "text/css", href = "ui.css")
        ),
        shinyjs::useShinyjs(),
        navbarPage("Anomaly Detection", id = "MNB",
            tabPanel("Load",
                fluidPage(
                    sidebarLayout(
                        sidebarPanel(id = "sidebar01",
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
                            ),
                            width = 3
                        ),
                        mainPanel(
                            h3("Preview"),
                            dygraphOutput("OriginalSeries"),
                            hr(),
                            uiOutput("DataSplitControl"),
                            uiOutput("NextBtn1Control"),
                            width = 9
                        )
                    )
                )
            ),
            tabPanel("Prepare",
                fluidPage(
                    sidebarPanel(id = 'sidebar02',
                        fluidRow(
                            bsCollapse(open = "Transformation", multiple = F,
                                bsCollapsePanel("Transformation",
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
                                    )
                                ),
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
                                    )
                                )
                            ),
                            fluidRow(
                                column(6, offset=3,
                                       actionButton("ApplyTsfBtn", "Apply", width = '100%')
                                )
                            )
                        ),
                        width = 3
                    ),
                    mainPanel(
                        wellPanel(id="PrepareResPanel", style = "overflow-y:scroll;height:80%;", #TODO: figure out height options
                                   h3("Original"),
                                   dygraphOutput("TrainSeriesClean"),
                                   br(),
                                   tags$div(id='tranform-res-placeholder')
                        ),
                        fluidRow(
                            column(1, offset=11,
                                   actionButton("Next2Btn", "Next", width = '100%')
                            )
                        ),
                        width = 9
                    )
                )
            ),
            tabPanel(
                "Model"
            )
        )
    )
)
