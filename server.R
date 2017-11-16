get.aggregation.data <- function(input, aggData, data) {
    return(
        list(
            data.agg = data$seriesPrepared,
            ts_type = ifelse(input$AggFunction == "none", names(aggData)[1], input$AggUnit),
            ts_val = ifelse(input$AggFunction == "none", aggData[[1]]['start'], input$AggCount),
            ts_func = ifelse(input$AggFunction == "none", "mean", input$AggFunction)
        )
    )
}

shinyServer(function(input, output, session) {
    
    # GENERAL DATA PLACEHOLDERS
    data <- reactiveValues(
        orig = NULL,
        series = NULL,
        aggData = NULL,
        model = NULL)
    
    # TRAIN DATA PLACEHOLDERS
    train <- reactiveValues(
        series = NULL,
        seriesNoMis = NULL,
        seriesNoOut = NULL,
        seriesNoNs = NULL,
        seriesPrepared = NULL,
        seriesAfterModel = NULL
    )
    
    # TEST DATA PLACEHOLDERS
    test <- reactiveValues(
        series = NULL,
        seriesNoMis = NULL,
        seriesNoOut = NULL,
        seriesNoNs = NULL,
        seriesPrepared = NULL,
        seriesAfterModel = NULL
    )
    
    # FIRST PAGE CONTENT
    # show modal dialog to select columns roles after file is loaded
    observeEvent(input$LocalFile, {
        # read and store uploaded data
        data$orig <- read.csv(
            input$LocalFile$datapath,
            header = input$FileHasHeader,
            sep = input$FileSeparator,
            quote = input$FileQuotation,
            stringsAsFactors = F
        )
        # role selection dialog
        showModal(
            modalDialog(
                DT::dataTableOutput("UserData"),
                hr(),
                fluidRow(
                    column(6,
                        fluidRow(
                            column(6,
                                div("Time variable:", style = "padding: 5px;font-size: 110%;")
                            ),
                            column(6,
                                uiOutput("DateTimeVarSelector")
                            )
                        ),
                        fluidRow(
                            column(6,
                                tags$div("Target variable:", style = "padding: 5px;font-size: 110%;")
                            ),
                            column(6,
                                uiOutput("TargetVarSelector")
                            )
                        )
                    ),
                    column(6,
                        fluidRow(
                            column(6,
                                div("Datetime format:", style = "padding: 5px;font-size: 110%;")
                            ),
                            column(6,
                                textInput("DateTimeFmt", NULL, placeholder = "HH:MM:SS"),
                                bsPopover("DateTimeFmt", "Datetime format", "Specify datetime format for time scale", placement="top")
                            )
                        )
                    )
                ),
                title="Role assignment", 
                size = "l", 
                footer = shinyjs::disabled(actionButton(inputId = "RoleSelectBtn", "Select"))
            )
        )
    })
    
    output$UserData <- DT::renderDataTable({
        req(data$orig)
        return(data$orig)
    },
    selection = 'none',
    options = list(dom = 'tsp', pageLength = 5, scrollX = T))
    
    output$DateTimeVarSelector <- renderUI({
        selectizeInput("DateTimeVar", NULL, choices = names(data$orig),
                       options = list(
                           placeholder = "Select",
                           onInitialize = I('function() { this.setValue(""); }')
                       )
        )
    })
    
    output$TargetVarSelector <- renderUI({
        selectizeInput("TargetVar", NULL,
                       choices = names(data$orig[sapply(data$orig, is.numeric)]),
                       options = list(
                           placeholder = "Select",
                           onInitialize = I('function() { this.setValue(""); }')
                       )
        )
    })
    
    output$DataSplitControl <- renderUI({
        if(!is.null(data$series)) {
            sliderInput("TrainTestSplit", "Data Split", min = 1, max = 100, step = 1, value = 67)
        }
    })
    
    output$NextBtn1Control <- renderUI({
        if(!is.null(data$series)) {
            fluidRow(
                column(1, offset = 11,
                    actionButton("Next1Btn", "Next", width = "100%")
                )
            )
        }
    })
    
    observeEvent(c(input$DateTimeVar, input$TargetVar, input$DateTimeFmt), {
        if (input$DateTimeVar != "" && input$TargetVar != "" && input$DateTimeFmt != "") {
            shinyjs::enable("RoleSelectBtn")
        }
    })
    
    observeEvent(input$RoleSelectBtn, {
        withProgress(message = "", value = 0, style = "old", {
            data$series <- data$orig[, c(input$DateTimeVar, input$TargetVar)]
            data$series[, input$DateTimeVar] <-
                strptime(data$series[, input$DateTimeVar], format = input$DateTimeFmt) %>% as.POSIXct()
            data$series <- xts(data$series[, 2], order.by = data$series[, 1])
            data$aggData <- timeSliders(data$series[, 1])
        })
        removeModal()
    })
    
    output$OriginalSeries <- renderDygraph({
        if(is.null(data$series) || is.null(input$TrainTestSplit)) {
            return(NULL)
        } else {
            tts = input$TrainTestSplit / 100
            return(plot_time_series(data$series, window_size = 0.01, train_test_split = tts))
        }
    })
    
    observeEvent(input$Next1Btn, {
        split.coef = input$TrainTestSplit / 100
        train$series <- data$series[1:(floor(length(data$series) * split.coef))]
        test$series <- data$series[(floor(length(data$series) * split.coef)+1):length(data$series)]
        updateTabsetPanel(session, "MNB", selected = "Prepare")
    })
    
    # SECOND (PREPARE) PAGE CONTENT
    # plot series, train part
    output$TrainSeriesClean <- renderDygraph({
        if(is.null(train$series)) {
            return(NULL)
        } else {
            return(plot_time_series(train$series))
        }
    })
    
    # aggregation dynamic UI parts
    output$AggUnitPlaceholder <- renderUI({
        selectInput("AggUnit", NULL, choices = setNames(names(data$aggData), str_to_title(names(data$aggData))) )
    })
    
    output$AggCountPlaceholder <- renderUI({
        if (!is.null(input$AggUnit)) {
            sliderInput("AggCount", "Aggregation Count",
                min = data$aggData[[input$AggUnit]]['start'],
                max = data$aggData[[input$AggUnit]]['end'],
                value = data$aggData[[input$AggUnit]]['start'],
                width = "100%"
            )
        }
    })
    
    # calculate results and add dynamic container for display
    observeEvent(input$ApplyTsfBtn, {
        withProgress(message = "", value = 0, style = "old", {
            train$seriesNoMis <- remove_na_from_data(train$series, type = input$MissValTreatment)
            train$seriesNoOut <-
                remove_outliers_from_data(train$seriesNoMis, type = input$OutliersTreatment, number = input$OutliersSigmaCount)
            win_ns = if(input$NoiseWindowType != 'auto') input$NoiseWindowSize else input$NoiseWindowType
            train$seriesNoNs <- denoise_data(train$seriesNoOut, type = input$NoiseTreatment, window_noise = win_ns)
            agg_type = if(input$AggFunction != 'none') paste(input$AggCount, input$AggUnit, sep = " ") else input$AggFunction
            train$seriesPrepared <- aggregation_data(train$seriesNoNs, type = agg_type, func_aggregate = input$AggFunction)
            train$seriesAfterModel <- train$seriesPrepared
            
            removeUI(selector = "#processed-results")
            insertUI(
                selector = "#tranform-res-placeholder",
                ui = div(id = "processed-results",
                         h3("Processed"),
                         hr(),
                         radioButtons("TransformResSelector", NULL, inline = T,
                                      choices = c(
                                          "Missing" = "missing",
                                          "Outliers" = "outliers",
                                          "Noise" = "noise",
                                          "Aggregation" = "aggregation"
                                      )
                         ),
                         dygraphOutput("ProcessedSeries")
                )
            )
        })
    })
    
    output$ProcessedSeries <- renderDygraph({
        if(!is.null(input$TransformResSelector)) {
            render.data <- NULL
            if(input$TransformResSelector == 'missing') {
                return(plot_time_series(train$seriesNoMis))
            } else if (input$TransformResSelector == 'outliers') {
                return(plot_time_series(train$seriesNoOut))
            } else if(input$TransformResSelector == 'noise') {
                return(plot_time_series(train$seriesNoNs))
            } else {
                return(plot_time_series(train$seriesPrepared))
            }
        }
    })
    
    # THIRD (TRAIN) PAGE CONTENT
    output$TrainSeriesDisplay <- renderDygraph({
        if(is.null(train$seriesAfterModel)) {
            return(NULL)
        }
        return(plot_time_series(train$seriesAfterModel))
    })
    
    output$ModelParams <- renderUI({
        if(input$ModelType == "dt") {
            if(input$TrainMode == "simple") {
                fluidRow(
                    column(6,
                        div("Sensitivity", style = "padding: 5px;font-size: 110%;")
                    ),
                    column(6,
                        selectInput("DT_Simple_Sens", NULL, choices = c("Low"="Low","Medium"="Medium","High"="High"), selected = "Medium")
                    )
                )
            } 
            else if(input$TrainMode == "expert") {
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
                                sliderInput("DT_Expert_NS", "Neighbour Similarity", min = 0.01, max = 0.99, value = 0.1)
                            )
                        )
                    )
                )
            }
        }
        else if(input$ModelType == "prophet") {
            if(input$TrainMode == "expert") {
                fluidRow(
                    column(12,
                        fluidRow(
                            column(6,
                                div("Yearly seasonality", style = "padding: 5px;font-size: 110%;")
                            ),
                            column(6,
                                selectInput("PROPH_Expert_YS", NULL, choices = c("Auto"= "auto", "True"= "true", "False"="false"))
                            )
                        ),
                        fluidRow(
                            column(6,
                                div("Weekly seasonality", style = "padding: 5px;font-size: 110%;")
                            ),
                            column(6,
                                selectInput("PROPH_Expert_WS", NULL, choices = c("Auto"= "auto", "True"= "true", "False"="false"))
                            )
                        ),
                        fluidRow(
                            column(6,
                                div("Daily seasonality", style = "padding: 5px;font-size: 110%;")
                            ),
                            column(6,
                                selectInput("PROPH_Expert_DS", NULL, choices = c("Auto"= "auto", "True"= "true", "False"="false"))
                            )
                        ),
                        fluidRow(
                            column(12,
                                sliderInput("PROPH_Expert_UI", "Uncertainty intervals", min = 0.6, max = 0.99, value = 0.9)
                            )
                        )
                    )
                )
            }
        }
    })
    
    output$ModelTuning <- renderUI({
        if(input$ModelType == "dt") {
            fluidRow(
                column(12,
                    fluidRow(
                        column(12,
                            h4("Low-bound anomalies"),
                            sliderInput("DT_LBAnomCorr", "Correction", min = -1, max = 1, value = 0),
                            sliderInput("DT_LBAnomScale", "Scale", min = 0.25, max = 4, value = 1)
                        )
                    ),
                    fluidRow(
                        column(12,
                            h4("High-bound anomalies"),
                            sliderInput("DT_HBAnomCorr", "Correction", min = -1, max = 1, value = 0),
                            sliderInput("DT_HBAnomScale", "Scale", min = 0.25, max = 4, value = 1)
                        )
                    ),
                    hr(),
                    fluidRow(
                        column(8,
                            sliderInput("DT_AutoTunePct", NULL, min = 0.001, max = 0.05, value = 0.01)
                        ),
                        column(4, style = "padding: 15px;",
                            actionButton("AutoTuneBtn", "Auto-tune", width = "100%"),
                            bsTooltip("AutoTuneBtn", "Set slider values automatically", placement = "top")
                        )
                    )
                )
            )
        }
        else if(input$ModelType == "prophet") {
            fluidRow(
                column(12,
                    fluidRow(
                        column(12,
                            h4("Low-bound anomalies"),
                            sliderInput("PROPH_LBAnomScale", "Scale", min = -1, max = 1, value = 0)
                        )
                    ),
                    fluidRow(
                        column(12,
                            h4("High-bound anomalies"),
                            sliderInput("PROPH_HBAnomScale", "Scale", min = -1, max = 1, value = 0)
                        )
                    )
                )
            )
        }
        else {
            fluidRow(
                column(12,
                    div("Please select model type")
                )
            )
        }
    })
    
    observeEvent(c(input$ModelType, input$TrainMode), {
        if(input$ModelType != "" && input$TrainMode != "") {
            shinyjs::enable("ModelTrainBtn")
        }
    })
    
    observeEvent(input$ModelTrainBtn, {
        withProgress(message = "", value = 0, style = "old", {
            ts.agg <- get.aggregation.data(input, data$aggData, train)
            if(input$ModelType == "dt") {
                if(input$TrainMode == "simple") {
                    train.params <- list(
                        mode = input$TrainMode,
                        sensitivity = input$DT_Simple_Sens
                    )
                } else {
                    train.params <- c(
                        mode = input$TrainMode,
                        params = list(
                            agg_th = input$DT_Expert_TAL,
                            local_trend = input$DT_Expert_LTB,
                            similar = input$DT_Expert_NS
                        )
                    )
                }
                data$model <- dynamicThreshold.train(ts.agg = ts.agg, train.params = train.params, type_th = "Both")
                train$seriesAfterModel <- dynamicThreshold.apply(ts.agg = ts.agg, model = data$model, type_th = "Both",
                                                                 correction = c("Low" = c(coef = input$DT_LBAnomCorr, scale = input$DT_LBAnomScale),
                                                                                "High" = c(coef = input$DT_HBAnomCorr, scale = input$DT_HBAnomScale)))
            } else {
                
            }
            
        })
    })
})
