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
                        sidebarPanel(class = "sad-app-container",
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
                            width = 9
                        )
                    )
                )
            ),
            tabPanel("Prepare",
                fluidPage(
                    sidebarLayout(
                        sidebarPanel(class = "sad-app-container",
                            fluidRow(
                                bsCollapse(id = "TsfCollapse", open = c("Transformation","Aggregation"), multiple = T,
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
                            wellPanel(class = "sad-app-container",
                                h3("Original"),
                                dygraphOutput("TrainSeriesClean"),
                                br(),
                                tags$div(id='tranform-res-placeholder')
                            ),
                            width = 9
                        )
                    )
                )
            ),
            tabPanel("Model",
                fluidPage(
                    sidebarLayout(
                        sidebarPanel(class = "sad-app-container",
                            fluidRow(
                                bsCollapse(id = "MSetupCollapse", open = "Model Setup", multiple = F,
                                    bsCollapsePanel("Model Setup",
                                        fluidRow(
                                            column(6,
                                                div(strong("Model type"), style = "padding: 5px;font-size: 110%;")
                                            ),
                                            column(6,
                                                selectizeInput("ModelType", NULL, choices = c('Dyn. Thresholds'='dt', 'Prophet'='prophet'),
                                                    options = list(
                                                        placeholder = "Select",
                                                        onInitialize = I('function() { this.setValue(""); }')
                                                    )
                                                )
                                            )
                                        ),
                                        conditionalPanel(
                                            condition = "input.ModelType != ''",
                                            fluidRow(
                                                column(6,
                                                    div(strong("Train mode"), style = "padding: 5px;font-size: 110%;")
                                                ),
                                                column(6,
                                                    selectizeInput("TrainMode", NULL, choices = c('Simple'='simple', 'Expert'='expert'),
                                                        options = list(
                                                            placeholder = "Select",
                                                            onInitialize = I('function() { this.setValue(""); }')
                                                        )
                                                    )
                                                )
                                            )
                                        ),
                                        conditionalPanel(
                                            condition = "input.ModelType == 'dt'",
                                            conditionalPanel(
                                                condition = "input.TrainMode == 'simple'",
                                                fluidRow(
                                                    column(6,
                                                        div("Sensitivity", style = "padding: 5px;font-size: 110%;")
                                                    ),
                                                    column(6,
                                                        selectInput("DT_Simple_Sens", NULL, choices = c("Low"="Low","Medium"="Medium","High"="High"), selected = "Medium")
                                                    )
                                                )
                                            ),
                                            conditionalPanel(
                                                condition = "input.TrainMode == 'expert'",
                                                fluidRow(
                                                    column(12,
                                                        fluidRow(
                                                            column(12,
                                                                sliderInput("DT_Expert_TAL", "Thresholds Aggregation Level", min = 0.05, max = 0.9, value = 0.6)
                                                            )
                                                        ),
                                                        fluidRow(
                                                            column(12,
                                                                sliderInput("DT_Expert_LTB", "Local Trend Basis", min = 0.05, max = 0.9, value = 0.3)
                                                            )
                                                        ),
                                                        fluidRow(
                                                            column(12,
                                                                sliderInput("DT_Expert_NS", "Neighbour Similarity", min = 0, max = 0.99, value = 0.1)
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        ),
                                        conditionalPanel(
                                            condition = "input.ModelType == 'prophet'",
                                            conditionalPanel(
                                                condition = "input.TrainMode == 'expert'",
                                                fluidRow(
                                                    column(12,
                                                        fluidRow(
                                                            column(6,
                                                                div("Yearly seasonality", style = "padding: 5px;font-size: 110%;")
                                                            ),
                                                            column(6,
                                                                selectInput("PROPH_Expert_YS", NULL, choices = c("Auto"= "auto", "True"= T, "False"=F))
                                                            )
                                                        ),
                                                        fluidRow(
                                                            column(6,
                                                                div("Weekly seasonality", style = "padding: 5px;font-size: 110%;")
                                                            ),
                                                            column(6,
                                                                selectInput("PROPH_Expert_WS", NULL, choices = c("Auto"= "auto", "True"= T, "False"=F))
                                                            )
                                                        ),
                                                        fluidRow(
                                                            column(6,
                                                                div("Daily seasonality", style = "padding: 5px;font-size: 110%;")
                                                            ),
                                                            column(6,
                                                                selectInput("PROPH_Expert_DS", NULL, choices = c("Auto"= "auto", "True"= T, "False"=F))
                                                            )
                                                        ),
                                                        fluidRow(
                                                            column(12,
                                                                sliderInput("PROPH_Expert_UI", "Uncertainty intervals", min = 0.6, max = 0.99, value = 0.9)
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        ),
                                        hr(),
                                        fluidRow(
                                            column(6, offset = 3,
                                                shinyjs::disabled(actionButton("ModelTrainBtn", "Train", width = "100%"))
                                            )
                                        )
                                    ),
                                    bsCollapsePanel("Model Tuning",
                                        conditionalPanel(
                                            condition = "input.ModelType == 'dt'",
                                            fluidRow(
                                                column(12,
                                                    fluidRow(
                                                        column(12,
                                                            h4("Low-bound anomalies"),
                                                            sliderInput("DT_LBAnomCorr", "Correction", min = -1, max = 1, value = 0, step = 0.1),
                                                            sliderInput("DT_LBAnomScale", "Scale", min = 0.25, max = 4, value = 1)
                                                        )
                                                    ),
                                                    fluidRow(
                                                        column(12,
                                                            h4("High-bound anomalies"),
                                                            sliderInput("DT_HBAnomCorr", "Correction", min = -1, max = 1, value = 0, step = 0.1),
                                                            sliderInput("DT_HBAnomScale", "Scale", min = 0.25, max = 4, value = 1)
                                                        )
                                                    ),
                                                    hr(),
                                                    fluidRow(
                                                        column(8,
                                                            sliderInput("DT_AutoTunePct", NULL, min = 0.001, max = 0.05, value = 0.01),
                                                            bsTooltip("DT_AutoTunePct", "Percent of anomaly cases for auto-tuning", placement = "top")
                                                        ),
                                                        column(4, style = "padding: 15px;",
                                                            actionButton("AutoTuneBtn", "Auto-tune", width = "100%"),
                                                            bsTooltip("AutoTuneBtn", "Set slider values automatically", placement = "top")
                                                        )
                                                    )
                                                )
                                            )
                                        ),
                                        conditionalPanel(
                                            condition = "input.ModelType == 'prophet'",
                                            fluidRow(
                                                column(12,
                                                    fluidRow(
                                                        column(12,
                                                            h4("Low-bound anomalies"),
                                                            sliderInput("PROPH_LBAnomScale", "Scale", min = -1, max = 1, value = 0, step = 0.01)
                                                        ),
                                                        column(12,
                                                            h4("High-bound anomalies"),
                                                            sliderInput("PROPH_HBAnomScale", "Scale", min = -1, max = 1, value = 0, step = 0.01)
                                                        )
                                                    )
                                                )
                                            )
                                        ),
                                        conditionalPanel(
                                            condition = "input.ModelType == ''",
                                            fluidRow(
                                                column(12,
                                                    div("Please, select model type")
                                                )
                                            )
                                        )
                                    )
                                )
                            ),
                            width = 3
                        ),
                        mainPanel(
                            wellPanel(class = "sad-app-container",
                                h3("Train data"),
                                dygraphOutput("TrainSeriesDisplay"),
                                hr(),
                                uiOutput("AnomaliesStats"),
                                hr(),
                                fluidRow(
                                    column(6,
                                        DT::dataTableOutput("AnomaliesSummary")
                                    ),
                                    column(6,
                                        plotlyOutput("SelectedAnomaly")
                                    )
                                )
                            ),
                            width = 9
                        )
                    )
                )
            ),
            tabPanel("Apply",
                fluidPage(
                    sidebarLayout(
                        sidebarPanel(class = "sad-app-container",
                            conditionalPanel(
                                condition = "input.ModelType == 'dt'",
                                fluidRow(
                                    column(12,
                                        fluidRow(
                                            column(12,
                                                h4("Low-bound anomalies"),
                                                sliderInput("DT_LBAnomCorr_Test", "Correction", min = -1, max = 1, value = 0, step = 0.1),
                                                sliderInput("DT_LBAnomScale_Test", "Scale", min = 0.25, max = 4, value = 1)
                                            )
                                        ),
                                        fluidRow(
                                            column(12,
                                                h4("High-bound anomalies"),
                                                sliderInput("DT_HBAnomCorr_Test", "Correction", min = -1, max = 1, value = 0, step = 0.1),
                                                sliderInput("DT_HBAnomScale_Test", "Scale", min = 0.25, max = 4, value = 1)
                                            )
                                        )
                                    )
                                )
                            ),
                            conditionalPanel(
                                condition = "input.ModelType == 'prophet'",
                                fluidRow(
                                    column(12,
                                        fluidRow(
                                            column(12,
                                                h4("Low-bound anomalies"),
                                                sliderInput("PROPH_LBAnomScale_Test", "Scale", min = -1, max = 1, value = 0, step = 0.01)
                                            ),
                                            column(12,
                                                h4("High-bound anomalies"),
                                                sliderInput("PROPH_HBAnomScale_Test", "Scale", min = -1, max = 1, value = 0, step = 0.01)
                                            )
                                        )
                                    )
                                )
                            ),
                            conditionalPanel(
                                condition = "input.ModelType == ''",
                                fluidRow(
                                    column(12,
                                        div("Model not selected")
                                    )
                                )
                            ),
                            width = 3
                        ),
                        mainPanel(
                            wellPanel(class = "sad-app-container",
                                h3("Test data"),
                                dygraphOutput("TestSeriesDisplay"),
                                hr(),
                                uiOutput("AnomaliesStatsTest"),
                                hr(),
                                fluidRow(
                                    column(6,
                                        DT::dataTableOutput("AnomaliesSummaryTest")
                                    ),
                                    column(6,
                                        plotlyOutput("SelectedAnomalyTest")
                                    )
                                )
                            ),
                            width = 9
                        )
                    )
                )
            )
        )
    )
)
