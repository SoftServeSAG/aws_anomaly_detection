shinyServer(function(input, output, session) {
    
    flags <- reactiveValues(
        train = FALSE,
        zeros = FALSE
    )
    
    # GENERAL DATA PLACEHOLDERS
    data <- reactiveValues(
        orig = NULL,
        series = NULL,
        loadErrMsg = NULL,
        aggData = NULL,
        model = NULL
    )
    
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
        res <- try(
            data$orig <- (read_delim(
                file = input$LocalFile$datapath,
                delim = input$FileSeparator,
                quote = input$FileQuotation,
                col_names = input$FileHasHeader,
                trim_ws = T
            )), T
        )
        # role selection dialog
        if(ncol(data$orig) > 1) {
            data$orig = as.data.frame(data$orig)
            showModal(
                modalDialog(
                    tags$head(
                        # this changes the size of the popovers
                        tags$style(".popover{width:350px;height:400px;max-width: 300%;}")
                    ),
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
                                    selectInput("DateTimeCh", label=NULL, choices = c("User-defined", "YYYY-MM-DD" = "%Y-%m-%d",
                                                                                      "YYYY-MM-DD HH:MM:SS" = "%Y-%m-%d %H:%M:%S",
                                                                                      "YYYY/MM/DD" = "%Y/%m/%d", 
                                                                                      "YYYY/MM/DD HH:MM:SS" = "%Y/%m/%d %H:%M:%S")),
                                    uiOutput("DateTimeF"),
                                    bsPopover("DateTimeFmt", "Datetime format", paste("Specify datetime format for time scale",
                                                                                      '<table width="90%" border="0"> <tr> <td><strong>Symbol</strong></td>', 
                                                                                      '<td><strong>Meaning</strong></td> <td><strong>Example</strong></td> </tr>', 
                                                                                      '<tr> <td><strong>%d</strong></td> <td>day as a number (0-31) </td> <td>01-31</td> </tr>', 
                                                                                      '<tr> <td><strong>%a<br /> %A</strong></td> <td>abbreviated weekday <br /> unabbreviated weekday </td>', 
                                                                                      '<td>Mon<br /> Monday</td> </tr> <tr> <td><strong>%m</strong></td> <td>month (00-12) </td> <td>00-12</td> </tr>', 
                                                                                      '<tr> <td><strong>%b<br /> %B</strong></td> <td>abbreviated month<br /> unabbreviated month </td> <td>Jan<br /> January</td>', 
                                                                                      '</tr> <tr> <td><strong>%y<br /> %Y</strong></td> <td>2-digit year <br /> 4-digit year </td> <td>07<br /> 2007</td> </tr> </table'), 
                                              placement="top")
                                )
                            )
                        )
                    ),
                    title="Role assignment", 
                    size = "l",
                    footer = list(
                        column(8, htmlOutput("LoadErrorMsgBox", inline = T)),
                        column(4, 
                            shinyjs::disabled(actionButton(inputId = "RoleSelectBtn", "Select")), 
                            actionButton(inputId = "CancelLoadBtn", "Cancel")
                        )
                    )
                )
            )
        } else {
            showModal(
                modalDialog(
                    title = "Load Error",
                    size = "s",
                    "Data load error. Please, check your file",
                    footer = list(actionButton(inputId = "CloseMessageBtn", "Close"))
                )
            )
        }
    })
    output$DateTimeF <- renderUI({
        if (input$DateTimeCh == "User-defined") {
            
            return(list(textInput("DateTimeFmt", NULL, placeholder = "HH:MM:SS"),
                        bsPopover("DateTimeFmt", "Datetime format", paste("Specify datetime format for time scale",
                                                                          '<table width="100%" border="1"> <tr> <td><strong>Symbol</strong></td>', 
                                                                          '<td><strong>Meaning</strong></td>  </tr>', 
                                                                          '<tr> <td><strong>%d</strong></td> <td>day as a number (0-31) </td> </tr>', 
                                                                          '<tr> <td><strong>%m</strong></td> <td>month (00-12) </td></tr>', 
                                                                          '<tr> <td><strong>%y<br /> %Y</strong></td> <td>2-digit year <br /> 4-digit year </td> </tr>',
                                                                          '<tr> <td><strong>%H</strong></td> <td>2-digit hours </td> </tr>',
                                                                          '<tr> <td><strong>%M</strong></td> <td>2-digit minutes </td> </tr>',
                                                                          '<tr> <td><strong>%S</strong></td> <td>2-digit seconds </td> </tr>',
                                                                          '</table><br>',
                                                                          '<strong>Example:</strong><br>',
                                                                          '<table width="100%" border="1"> <tr> <td><strong>DateTime</strong></td>', 
                                                                          '<td><strong>Format</strong></td>  </tr>', 
                                                                          '<tr> <td>2017-12-01&nbsp12:20:02</td> <td>%Y-%m-%d&nbsp$H:%M:%S</td> </tr>',
                                                                          '<tr> <td>10/30/2017&nbsp12:20:02</td> <td>%m/%d/%Y&nbsp$H:%M:%S</td> </tr>',
                                                                          '</table>'
                                                                          ), 
                                  placement="top")))
        }
    })
    
    output$UserData <- DT::renderDataTable({
        req(data$orig)
        data$orig
    },
    selection = 'none',
    options = list(dom = 'tsp', pageLength = 5, scrollX = T))
    
    output$DateTimeVarSelector <- renderUI({
        selectizeInput("DateTimeVar", NULL, choices = names(data$orig),
                       options = list(
                           placeholder = "Select",
                           onInitialize = I('function() { this.setValue(""); }')
                       ))
    
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
    
    output$LoadErrorMsgBox <- renderText({
        req(data$loadErrMsg)
        data$loadErrMsg
    })
    
    output$DataSplitControl <- renderUI({
        if(!is.null(data$series)) {
            sliderInput("TrainTestSplit", "Data Split", min = 1, max = 100, step = 1, value = 67)
        }
    })
    
    observeEvent(c(input$DateTimeVar, input$TargetVar, input$DateTimeFmt, input$DateTimeCh), {
        if(!is.null(input$DateTimeVar) &&  !is.null(input$TargetVar)) {
            if(input$DateTimeVar != "" && input$TargetVar != "" && (input$DateTimeFmt != "" || input$DateTimeCh != "User-defined")) {
                shinyjs::enable("RoleSelectBtn")
            } else {
                shinyjs::disable("RoleSelectBtn")
            }
        }
    })
    
    observeEvent(input$DateTimeCh, {
        data$loadErrMsg <- NULL
    })
    
    observeEvent(input$RoleSelectBtn, {
        # reset previous state
        data$series <- NULL
        data$aggData <- NULL
        data$model <- NULL
        train$series = NULL
        train$seriesNoMis = NULL
        train$seriesNoOut = NULL
        train$seriesNoNs = NULL
        train$seriesPrepared = NULL
        train$seriesAfterModel = NULL
        train$anomaliesReport = NULL
        test$series = NULL
        test$seriesNoMis = NULL
        test$seriesNoOut = NULL
        test$seriesNoNs = NULL
        test$seriesPrepared = NULL
        test$seriesAfterModel = NULL
        test$anomaliesReport = NULL
        
        withProgress(message = "", value = 0, style = "old", {
            data$series <- data$orig[, c(input$DateTimeVar, input$TargetVar)]
            if (input$DateTimeCh=="User-defined")
                data$series[,input$DateTimeVar] <- strptime(data$series[,input$DateTimeVar], format=input$DateTimeFmt) %>% as.POSIXct() 
            else
                data$series[,input$DateTimeVar] <- strptime(data$series[,input$DateTimeVar], format=input$DateTimeCh) %>% as.POSIXct() 
            
            # data validity checks
            if(sum(is.na(data$series[,input$DateTimeVar])) > 0) {
                if(sum(is.na(data$series[,input$DateTimeVar])) == nrow(data$series)) {
                    data$loadErrMsg <- paste("<font color=\"#FF0000\"><b>", 'Wrong DateTime format', "</b></font>")
                } else {
                    data$loadErrMsg <- paste("<font color=\"#FF0000\"><b>", 'Wrong Data format', "</b></font>")
                }
                data$series <- NULL
                return()
            } else {
                data$loadErrMsg <- NULL
            }
            
            data$series <- xts(data$series[, 2], order.by = data$series[, 1])
            data$aggData <- timeSliders(data$series[, 1])
            removeModal()
        })
        
        removeUI(selector = "#processed-results")
    })
    
    observeEvent(input$CancelLoadBtn, {
        data$loadErrMsg <- NULL
        removeModal()
        shinyjs::reset("LocalFile")
    })
    
    observeEvent(input$CloseMessageBtn, {
        removeModal()
        shinyjs::reset("LocalFile")
    })
    
    output$OriginalSeries <- renderDygraph({
        if(is.null(data$series) || is.null(input$TrainTestSplit)) {
            return(NULL)
        } else {
            tts = input$TrainTestSplit / 100
            res = train_test_split_time_series(data$series, tts)
            train$series <- res$train
            test$series <- res$test
            return(res$plot)
        }
    })
    
    observeEvent(input$TrainTestSplit, {
        if (input$TrainTestSplit > 90) {
            updateSliderInput(session, inputId = "TrainTestSplit",value = 90)
        }
        if (input$TrainTestSplit < 30) {
            updateSliderInput(session, inputId = "TrainTestSplit",value = 30)
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
                low = round(ll/full.len * 100, 2),
                medium = round(ml/full.len * 100,2),
                high = round(hl/full.len * 100,2)
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
        # reset controls
        # dt
        updateSliderInput(session, "DT_LBAnomCorr", value = 0)
        updateSliderInput(session, "DT_LBAnomScale", value = 1)
        updateSliderInput(session, "DT_HBAnomCorr", value = 0)
        updateSliderInput(session, "DT_HBAnomScale", value = 1)
        updateSliderInput(session, "DT_AutoTunePct", value = 0.01)
        # prophet
        updateSliderInput(session, "PROPH_LBAnomScale", value = 0)
        updateSliderInput(session, "PROPH_HBAnomScale", value = 0)
        
        # reset state train/test
        req(data$model)
        data$model <- NULL
        train$seriesAfterModel <- train$seriesPrepared
        train$anomaliesReport <- NULL
        test$seriesAfterModel <- test$seriesPrepared
        test$anomaliesReport <- NULL
    })
    
    observeEvent(input$ModelTrainBtn, {
        withProgress(message = "", value = 0, style = "old", {
            flags$train=TRUE
            flags$zeros=TRUE
            if(input$ModelType == "dt") {
                updateSliderInput(session, "DT_LBAnomCorr", value = 0)
                updateSliderInput(session, "DT_LBAnomScale", value = 1)
                updateSliderInput(session, "DT_HBAnomCorr", value = 0)
                updateSliderInput(session, "DT_HBAnomScale", value = 1)
                
                data$model <- dynamicThreshold.train(ts.agg = params_train()$ts.agg,  train.params = params_train()$train.params, type_th = "Both")
                train$seriesAfterModel <- dynamicThreshold.apply(ts.agg = params_train()$ts.agg, model = data$model, type_th = "Both", 
                                                                 correction = list("Low" = c(coef = 0, scale = 1), 
                                                                                   "High" = c(coef = 0, scale = 1)))
                train$anomaliesReport <- anomalies.stat(train$seriesAfterModel, train$series, params_train()$ts.agg$ts_type, params_train()$ts.agg$ts_val)
                # apply for the test set
                test$seriesAfterModel <- dynamicThreshold.apply(ts.agg = params_test()$ts.agg, model = data$model, type_th = "Both", 
                                                                 correction = list("Low" = c(coef = 0, scale = 1), 
                                                                                   "High" = c(coef = 0, scale = 1)))
                test$anomaliesReport <- anomalies.stat(test$seriesAfterModel, test$series, params_test()$ts.agg$ts_type, params_test()$ts.agg$ts_val)
                
            } else { # prophet
                updateSliderInput(session, "PROPH_LBAnomScale", value = 0)
                updateSliderInput(session, "PROPH_HBAnomScale", value = 0)
                
                data$model <- model_prophet_train_test(train$seriesPrepared, test$seriesPrepared, 
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
        if (flags$train==FALSE ||  
            (input$DT_LBAnomCorr!=0 || input$DT_LBAnomScale !=1 || input$DT_HBAnomCorr!=0 || input$DT_HBAnomScale !=1) ){
            # sync test ui
            flags$train=FALSE 
            flags$zeros=FALSE 
            updateSliderInput(session, "DT_LBAnomCorr_Test", value = input$DT_LBAnomCorr)
            updateSliderInput(session, "DT_LBAnomScale_Test", value = input$DT_LBAnomScale)
            updateSliderInput(session, "DT_HBAnomCorr_Test", value = input$DT_HBAnomCorr)
            updateSliderInput(session, "DT_HBAnomScale_Test", value = input$DT_HBAnomScale)
            # we need model to go any further
            req(data$model)
            withProgress(message = "", value = 0, style = "old", {
                train$seriesAfterModel <- dynamicThreshold.apply(ts.agg = params_train()$ts.agg, model = data$model, type_th = "Both", 
                                                                 correction = list("Low" = c(coef = input$DT_LBAnomCorr, scale = input$DT_LBAnomScale), 
                                                                                   "High" = c(coef = input$DT_HBAnomCorr, scale = input$DT_HBAnomScale)))
                train$anomaliesReport <- anomalies.stat(train$seriesAfterModel, train$series, params_train()$ts.agg$ts_type, params_train()$ts.agg$ts_val)
            })
            
        }else{
            updateSliderInput(session, "DT_LBAnomCorr_Test", value = 0)
            updateSliderInput(session, "DT_LBAnomScale_Test", value = 1)
            updateSliderInput(session, "DT_HBAnomCorr_Test", value = 0)
            updateSliderInput(session, "DT_HBAnomScale_Test", value = 1)
            flags$train=FALSE 
            
        }
        
        
    })
    
    observeEvent(input$AutoTuneBtn, {
        flags$train=FALSE
        flags$zeros=FALSE
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
        if (flags$train==FALSE || (input$PROPH_LBAnomScale!=0 || input$PROPH_HBAnomScale!=0)){
            flags$train=FALSE 
            flags$zeros=FALSE
            updateSliderInput(session, "PROPH_LBAnomScale_Test", value = input$PROPH_LBAnomScale)
            updateSliderInput(session, "PROPH_HBAnomScale_Test", value = input$PROPH_HBAnomScale)
            # we need model to go any further
            req(data$model)
            withProgress(message = "", value = 0, style = "old", {
                train$seriesAfterModel <- model_prophet_new_interval(data$model$data_train, percent_up = input$PROPH_HBAnomScale, 
                                                                     percent_low = input$PROPH_LBAnomScale, method = "both")
                train$anomaliesReport <- anomalies.stat(train$seriesAfterModel, train$series, model_params()$ts.agg$ts_type, model_params()$ts.agg$ts_val)
            })}else{
                updateSliderInput(session, "PROPH_LBAnomScale_Test", value = 0)
                updateSliderInput(session, "PROPH_HBAnomScale_Test", value = 0)
                flags$train=FALSE
            }
       
        # sync test ui
        
    })
    
    output$AnomaliesStats <- renderUI({
        req(train$anomaliesReport)
        fluidRow(
            column(12,
                h3("Anomalies Report"),
                fluidRow(
                    valueBox(paste(anom_percent_train()$high, "%"), "HIGH", color = "orange"),
                    valueBox(paste(anom_percent_train()$medium, "%"), "MEDIUM", color = "light-blue"),
                    valueBox(paste(anom_percent_train()$low, "%"), "LOW", color = "teal")
                )
            )
        )
    })
    
    output$AnomaliesSummary <- DT::renderDataTable({
        req(train$anomaliesReport)
        if (!is.null(train$anomaliesReport$ad_res)){
            ddf=train$anomaliesReport$ad_res
            ddf$start=as.character(ddf$start)
            ddf=ddf[,c(1,5,3,4)]
            names(ddf)[1]='datetime'
            return(ddf) 
        }
        
    },
    selection = 'single',
    options = list(dom = 'tsp', pageLength = 5, scrollX = T))
    
    output$SelectedAnomaly <- renderDygraph({
        req(train$anomaliesReport)
        req(input$AnomaliesSummary_rows_selected)
        anomaly = train$anomaliesReport$ad_res[input$AnomaliesSummary_rows_selected,]
        t1 = anomaly$start-(anomaly$end-anomaly$start)
        t2 = anomaly$end+(anomaly$end-anomaly$start)
        return(plot_time_series(train$series[paste(t1,'/',t2, sep = '')], window_size = c(anomaly$start, anomaly$end)))
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
                low = round(ll/full.len * 100,2),
                medium = round(ml/full.len * 100,2),
                high = round(hl/full.len * 100,2)
            )
        )
    })
    
    output$TestSeriesDisplay <- renderDygraph({
        req(test$seriesAfterModel)
        return(plot_time_series(test$seriesAfterModel))
    })
    
    observeEvent(c(input$DT_LBAnomCorr_Test, input$DT_LBAnomScale_Test, input$DT_HBAnomCorr_Test, input$DT_HBAnomScale_Test), {
        if (flags$zeros==FALSE ||
            input$DT_LBAnomCorr_Test!=0 || input$DT_LBAnomScale_Test!=1 || input$DT_HBAnomCorr_Test!=0 || input$DT_HBAnomScale_Test!=1){
            flags$train=FALSE 
            flags$zeros = FALSE
            req(data$model)
            withProgress(message = "", value = 0, style = "old", {
                test$seriesAfterModel <- dynamicThreshold.apply(ts.agg = params_test()$ts.agg, model = data$model, type_th = "Both", 
                                                                correction = list("Low" = c(coef = input$DT_LBAnomCorr_Test, scale = input$DT_LBAnomScale_Test), 
                                                                                  "High" = c(coef = input$DT_HBAnomCorr_Test, scale = input$DT_HBAnomScale_Test)))
                test$anomaliesReport <- anomalies.stat(test$seriesAfterModel, test$series, params_test()$ts.agg$ts_type, params_test()$ts.agg$ts_val)
            })
        }else{
            flags$train=FALSE
            flags$zeros = FALSE
        }
       
    })
    
    observeEvent(c(input$PROPH_LBAnomScale_Test, input$PROPH_HBAnomScale_Test), {
        if (flags$zeros==FALSE && input$PROPH_LBAnomScale_Test!=0 || input$PROPH_HBAnomScale_Test!=0){
            flags$train=FALSE 
            flags$zeros = FALSE
            req(data$model)
            withProgress(message = "", value = 0, style = "old", {
                test$seriesAfterModel <- model_prophet_new_interval(data$model$data_test, percent_up = input$PROPH_HBAnomScale_Test, 
                                                                    percent_low = input$PROPH_LBAnomScale_Test, method = "both")
                test$anomaliesReport <- anomalies.stat(test$seriesAfterModel, test$series, model_params()$ts.agg$ts_type, model_params()$ts.agg$ts_val)
            })
        }else{
            flags$train=FALSE
            flags$zeros = FALSE
        }
        
    })
    
    output$AnomaliesStatsTest <- renderUI({
        req(test$anomaliesReport)
        fluidRow(
            column(12,
                h3("Anomalies Report"),
                fluidRow(
                    valueBox(paste(anom_percent_test()$high, "%"), "HIGH", color = "orange"),
                    valueBox(paste(anom_percent_test()$medium, "%"), "MEDIUM", color = "light-blue"),
                    valueBox(paste(anom_percent_test()$low, "%"), "LOW", color = "teal")
                )
            )
        )
    })
    
    output$AnomaliesSummaryTest <- DT::renderDataTable({
        req(test$anomaliesReport)
        if (!is.null(test$anomaliesReport$ad_res)){
            ddf=test$anomaliesReport$ad_res
            ddf$start=as.character(ddf$start)
            ddf=ddf[,c(1,5,3,4)]
            names(ddf)[1]='datetime'
            return(ddf)
        }
        
    },
    selection = 'single',
    options = list(dom = 'tsp', pageLength = 5, scrollX = T))
    
    output$SelectedAnomalyTest <- renderDygraph({
        req(test$anomaliesReport)
        req(input$AnomaliesSummaryTest_rows_selected)
        anomaly = test$anomaliesReport$ad_res[input$AnomaliesSummaryTest_rows_selected,]
        t1 = anomaly$start-(anomaly$end-anomaly$start)
        t2 = anomaly$end+(anomaly$end-anomaly$start)
        return(plot_time_series(test$series[paste(t1,'/',t2, sep = '')], window_size = c(anomaly$start, anomaly$end)))
    })
})

process.data <- function(itm, input) {
    win_ns = ifelse(input$NoiseWindowType != 'auto', input$NoiseWindowSize, input$NoiseWindowType)
    agg_type = ifelse(input$AggFunction != 'none', paste(input$AggCount, input$AggUnit, sep = " "), input$AggFunction)
    itm$seriesNoMis <- remove_na_from_data(itm$series, type = input$NAValTreatment)
    itm$seriesNoOut <- remove_outliers_from_data(itm$seriesNoMis, type = input$OutliersTreatment, number = input$OutliersSigmaCount)
    itm$seriesNoNs <- denoise_data(itm$seriesNoOut, type = input$NoiseTreatment, window_noise = win_ns)
    itm$seriesPrepared <- aggregation_data(itm$seriesNoNs, type = agg_type, func_aggregate = input$AggFunction)
    print(head(itm$seriesPrepared, n = 25))
    print(strtoi(input$MisValImputeNum))
    itm$seriesPrepared <- remove_na_from_data_in_dates(itm$seriesPrepared, type = input$MisValTreatment, fill_value = strtoi(input$MisValImputeNum))
    print(head(itm$seriesPrepared, n = 25))
    itm$seriesAfterModel <- itm$seriesPrepared
}
