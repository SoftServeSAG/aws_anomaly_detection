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
        seriesAfterModel = NULL,
        anomaliesReport = NULL
    )
    
    # TEST DATA PLACEHOLDERS
    test <- reactiveValues(
        series = NULL,
        seriesNoMis = NULL,
        seriesNoOut = NULL,
        seriesNoNs = NULL,
        seriesPrepared = NULL,
        seriesAfterModel = NULL,
        anomaliesReport = NULL
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
    
    observeEvent(input$TrainTestSplit, {
        req(data$series)
        split.coef = input$TrainTestSplit / 100
        train$series <- data$series[1:(floor(length(data$series) * split.coef))]
        test$series <- data$series[(floor(length(data$series) * split.coef)+1):length(data$series)]
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
    
    # SECOND (PREPARE) PAGE CONTENT
    # plot series, train part
    output$TrainSeriesClean <- renderDygraph({
        req(train$series)
        return(plot_time_series(train$series))
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
            # win_ns = ifelse(input$NoiseWindowType != 'auto', input$NoiseWindowSize, input$NoiseWindowType)
            # agg_type = ifelse(input$AggFunction != 'none', paste(input$AggCount, input$AggUnit, sep = " "), input$AggFunction)
            # for(itm in c(train, test)) {
            #     print(itm)
            #     itm$seriesNoMis <- remove_na_from_data(itm$series, type = input$MissValTreatment)
            #     itm$seriesNoOut <- remove_outliers_from_data(itm$seriesNoMis, type = input$OutliersTreatment, number = input$OutliersSigmaCount)
            #     itm$seriesNoNs <- denoise_data(itm$seriesNoOut, type = input$NoiseTreatment, window_noise = win_ns)
            #     itm$seriesPrepared <- aggregation_data(itm$seriesNoNs, type = agg_type, func_aggregate = input$AggFunction)
            #     itm$seriesAfterModel <- itm$seriesPrepared
            # }
            process.data(train, input)
            process.data(test, input)
            
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
    model_params <- reactive({
        ts.agg <- list(
            data.agg = NULL,
            ts_type = ifelse(input$AggFunction == "none", names(data$aggData)[1], input$AggUnit),
            ts_val = ifelse(input$AggFunction == "none", data$aggData[[1]]['start'], input$AggCount),
            ts_func = ifelse(input$AggFunction == "none", "mean", input$AggFunction)
        )
        
        if(input$ModelType == "dt") {
            if(input$TrainMode == "simple") {
                train.params <- list(
                    mode = input$TrainMode,
                    sensitivity = input$DT_Simple_Sens
                )
            } else {
                train.params <- list(
                    mode = input$TrainMode,
                    params = list(
                        agg_th = input$DT_Expert_TAL,
                        local_trend = input$DT_Expert_LTB,
                        similar = input$DT_Expert_NS
                    )
                )
            }
        } else {
            train.params = NULL
        }
        return(list(ts.agg = ts.agg, train.params = train.params))
    })
    
    params_train <- reactive({
        req(train$seriesPrepared)
        res = model_params()
        res$ts.agg$data.agg = train$seriesPrepared
        return(res)
    })
    
    params_test <- reactive({
        req(test$seriesPrepared)
        res = model_params()
        res$ts.agg$data.agg = test$seriesPrepared
        return(res)
    })
    
    anom_percent_train <- reactive({
        req(train$anomaliesReport)
        full.len = nrow(train$anomaliesReport$ad_res)
        ll = sum(train$anomaliesReport$ad_res$significance == "Low")
        ml = sum(train$anomaliesReport$ad_res$significance == "Medium")
        hl = sum(train$anomaliesReport$ad_res$significance == "High")
        return(
            list(
                low = ll/full.len * 100,
                medium = ml/full.len * 100,
                high = hl/full.len * 100
            )
        )
    })
    
    output$TrainSeriesDisplay <- renderDygraph({
        req(train$seriesAfterModel)
        return(plot_time_series(train$seriesAfterModel))
    })
    
    observeEvent(c(input$ModelType, input$TrainMode), {
        if(input$ModelType != "" && input$TrainMode != "") {
            shinyjs::enable("ModelTrainBtn")
        }
    })
    
    # reset modelling state, when we change model type
    observeEvent(input$ModelType, {
        req(data$model)
        # drop model
        data$model <- NULL
        # reset train
        train$seriesAfterModel <- train$seriesPrepared
        train$anomaliesReport <- NULL
        # reset test
        test$seriesAfterModel <- test$seriesPrepared
        test$anomaliesReport <- NULL
    })
    
    observeEvent(input$ModelTrainBtn, {
        withProgress(message = "", value = 0, style = "old", {
            if(input$ModelType == "dt") {
                data$model <- dynamicThreshold.train(ts.agg = params_train()$ts.agg, train.params = params_train()$train.params, type_th = "Both")
                train$seriesAfterModel <- dynamicThreshold.apply(ts.agg = params_train()$ts.agg, model = data$model, type_th = "Both", 
                                                                 correction = list("Low" = c(coef = input$DT_LBAnomCorr, scale = input$DT_LBAnomScale), 
                                                                                   "High" = c(coef = input$DT_HBAnomCorr, scale = input$DT_HBAnomScale)))
                train$anomaliesReport <- anomalies.stat(train$seriesAfterModel, train$series, params_train()$ts.agg$ts_type, params_train()$ts.agg$ts_val)
                # apply for the test set
                test$seriesAfterModel <- dynamicThreshold.apply(ts.agg = params_test()$ts.agg, model = data$model, type_th = "Both", 
                                                                 correction = list("Low" = c(coef = input$DT_LBAnomCorr, scale = input$DT_LBAnomScale), 
                                                                                   "High" = c(coef = input$DT_HBAnomCorr, scale = input$DT_HBAnomScale)))
                test$anomaliesReport <- anomalies.stat(test$seriesAfterModel, test$series, params_test()$ts.agg$ts_type, params_test()$ts.agg$ts_val)
                
            } else { # prophet
                data$model <- model_prophet_train_test(train$seriesAfterModel, test$seriesAfterModel, 
                                                yearly = input$PROPH_Expert_YS, weekly = input$PROPH_Expert_WS, daily = input$PROPH_Expert_DS, 
                                                interval_width = input$PROPH_Expert_UI, method = 'train')
                train$seriesAfterModel <- model_prophet_new_interval(data$model$data_train, method = "both")
                train$anomaliesReport <- anomalies.stat(train$seriesAfterModel, train$series, model_params()$ts.agg$ts_type, model_params()$ts.agg$ts_val)
                # apply for the test set
                test$seriesAfterModel <- model_prophet_new_interval(data$model$data_test, method = "both")
                test$anomaliesReport <- anomalies.stat(test$seriesAfterModel, test$series, params_test()$ts.agg$ts_type, params_test()$ts.agg$ts_val)
            }
            updateCollapse(session, "MSetupCollapse", open = "Model Tuning")
        })
    })
    
    observeEvent(c(input$DT_LBAnomCorr, input$DT_LBAnomScale, input$DT_HBAnomCorr, input$DT_HBAnomScale), {
        req(data$model)
        withProgress(message = "", value = 0, style = "old", {
            train$seriesAfterModel <- dynamicThreshold.apply(ts.agg = params_train()$ts.agg, model = data$model, type_th = "Both", 
                                                             correction = list("Low" = c(coef = input$DT_LBAnomCorr, scale = input$DT_LBAnomScale), 
                                                                               "High" = c(coef = input$DT_HBAnomCorr, scale = input$DT_HBAnomScale)))
            train$anomaliesReport <- anomalies.stat(train$seriesAfterModel, train$series, params_train()$ts.agg$ts_type, params_train()$ts.agg$ts_val)
        })
    })
    
    observeEvent(input$AutoTuneBtn, {
        req(data$model)
        withProgress(message = "", value = 0, style = "old", {
            coefs = dynamicThreshold.autoturn(ts.agg = params_train()$ts.agg, model = data$model, type_th = 'Both', p_anomalies = input$DT_AutoTunePct)
            updateSliderInput(session, "DT_LBAnomCorr", value = coefs$Low[1])
            updateSliderInput(session, "DT_LBAnomScale", value = coefs$Low[2])
            updateSliderInput(session, "DT_HBAnomCorr", value = coefs$High[1])
            updateSliderInput(session, "DT_HBAnomScale", value = coefs$High[2])
        })
    })
    
    observeEvent(c(input$PROPH_LBAnomScale, input$PROPH_HBAnomScale), {
        req(data$model)
        withProgress(message = "", value = 0, style = "old", {
            train$seriesAfterModel <- model_prophet_new_interval(data$model$data_train, percent_up = input$PROPH_HBAnomScale, 
                                                                 percent_low = input$PROPH_LBAnomScale, method = "both")
            train$anomaliesReport <- anomalies.stat(train$seriesAfterModel, train$series, model_params()$ts.agg$ts_type, model_params()$ts.agg$ts_val)
        })
    })
    
    output$AnomaliesStats <- renderUI({
        req(train$anomaliesReport)
        fluidRow(
            column(12,
                h3("Anomalies Report"),
                fluidRow(
                    valueBox(anom_percent_train()$high, "% high"),
                    valueBox(anom_percent_train()$medium, "% medium"),
                    valueBox(anom_percent_train()$low, "% low")
                )
            )
        )
    })
    
    output$AnomaliesSummary <- DT::renderDataTable({
        req(train$anomaliesReport)
        return(train$anomaliesReport$ad_res)
    },
    selection = 'single',
    options = list(dom = 'tsp', pageLength = 5, scrollX = T))
    
    output$SelectedAnomaly <- renderPlotly({
        req(train$anomaliesReport)
        req(input$AnomaliesSummary_rows_selected)
        return(anomalies.detail(
            train$anomaliesReport$ad_res[input$AnomaliesSummary_rows_selected,],
            train$series,
            params_train()$ts.agg$ts_func
        )$row_data.anomaly.plot)
    })
    
    # LAST (TEST) PAGE CONTENT
    anom_percent_test <- reactive({
        req(test$anomaliesReport)
        full.len = nrow(test$anomaliesReport$ad_res)
        ll = sum(test$anomaliesReport$ad_res$significance == "Low")
        ml = sum(test$anomaliesReport$ad_res$significance == "Medium")
        hl = sum(test$anomaliesReport$ad_res$significance == "High")
        return(
            list(
                low = ll/full.len * 100,
                medium = ml/full.len * 100,
                high = hl/full.len * 100
            )
        )
    })
    
    output$TestSeriesDisplay <- renderDygraph({
        req(test$seriesAfterModel)
        return(plot_time_series(test$seriesAfterModel))
    })
    
    observeEvent(c(input$DT_LBAnomCorr_Test, input$DT_LBAnomScale_Test, input$DT_HBAnomCorr_Test, input$DT_HBAnomScale_Test), {
        req(data$model)
        withProgress(message = "", value = 0, style = "old", {
            test$seriesAfterModel <- dynamicThreshold.apply(ts.agg = params_test()$ts.agg, model = data$model, type_th = "Both", 
                                                             correction = list("Low" = c(coef = input$DT_LBAnomCorr_Test, scale = input$DT_LBAnomScale_Test), 
                                                                               "High" = c(coef = input$DT_HBAnomCorr_Test, scale = input$DT_HBAnomScale_Test)))
            test$anomaliesReport <- anomalies.stat(test$seriesAfterModel, test$series, params_test()$ts.agg$ts_type, params_test()$ts.agg$ts_val)
        })
    })
    
    observeEvent(c(input$PROPH_LBAnomScale_Test, input$PROPH_HBAnomScale_Test), {
        req(data$model)
        withProgress(message = "", value = 0, style = "old", {
            test$seriesAfterModel <- model_prophet_new_interval(data$model$data_test, percent_up = input$PROPH_HBAnomScale_Test, 
                                                                 percent_low = input$PROPH_LBAnomScale_Test, method = "both")
            test$anomaliesReport <- anomalies.stat(test$seriesAfterModel, test$series, model_params()$ts.agg$ts_type, model_params()$ts.agg$ts_val)
        })
    })
    
    output$AnomaliesStatsTest <- renderUI({
        req(test$anomaliesReport)
        fluidRow(
            column(12,
                h3("Anomalies Report"),
                fluidRow(
                    valueBox(anom_percent_test()$high, "% high"),
                    valueBox(anom_percent_test()$medium, "% medium"),
                    valueBox(anom_percent_test()$low, "% low")
                )
            )
        )
    })
    
    output$AnomaliesSummaryTest <- DT::renderDataTable({
        req(test$anomaliesReport)
        return(test$anomaliesReport$ad_res)
    },
    selection = 'single',
    options = list(dom = 'tsp', pageLength = 5, scrollX = T))
    
    output$SelectedAnomalyTest <- renderPlotly({
        req(test$anomaliesReport)
        req(input$AnomaliesSummaryTest_rows_selected)
        return(anomalies.detail(
            test$anomaliesReport$ad_res[input$AnomaliesSummaryTest_rows_selected,],
            test$series,
            params_test()$ts.agg$ts_func
        )$row_data.anomaly.plot)
    })
})

process.data <- function(itm, input) {
    win_ns = ifelse(input$NoiseWindowType != 'auto', input$NoiseWindowSize, input$NoiseWindowType)
    agg_type = ifelse(input$AggFunction != 'none', paste(input$AggCount, input$AggUnit, sep = " "), input$AggFunction)
    itm$seriesNoMis <- remove_na_from_data(itm$series, type = input$MissValTreatment)
    itm$seriesNoOut <- remove_outliers_from_data(itm$seriesNoMis, type = input$OutliersTreatment, number = input$OutliersSigmaCount)
    itm$seriesNoNs <- denoise_data(itm$seriesNoOut, type = input$NoiseTreatment, window_noise = win_ns)
    itm$seriesPrepared <- aggregation_data(itm$seriesNoNs, type = agg_type, func_aggregate = input$AggFunction)
    itm$seriesAfterModel <- itm$seriesPrepared
}
