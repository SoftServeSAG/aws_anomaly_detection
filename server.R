library(shiny)
library(dygraphs)
library(stringr)
source("src/timeSliders.R")
source("src/remove_na_values.R")
source("src/remove_outliers.R")
source("src/denoise.R")
source("src/aggregation.R")
source("src/visualization.R")

shinyServer(function(input, output, session) {
    
    # GENERAL DATA PLACEHOLDERS
    data <- reactiveValues(
        orig = NULL,
        series = NULL,
        aggData = NULL)
    
    # TRAIN DATA PLACEHOLDERS
    train <- reactiveValues(
        series = NULL,
        seriesNoMis = NULL,
        seriesNoOut = NULL,
        seriesNoNs = NULL,
        seriesFinal = NULL
    )
    
    # TEST DATA PLACEHOLDERS
    test <- reactiveValues(
        series = NULL,
        seriesNoMis = NULL,
        seriesNoOut = NULL,
        seriesNoNs = NULL,
        seriesFinal = NULL
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
    output$TrainSeriesClean <- renderDygraph(plot_time_series(train$series))
    
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
            win_ns <- if(input$NoiseWindowType != 'auto') input$NoiseWindowSize else input$NoiseWindowType
            train$seriesNoNs <- denoise_data(train$seriesNoOut, type = input$NoiseTreatment, window_noise = win_ns)
            agg_type <- if(input$AggFunction != 'none') paste(input$AggCount, input$AggUnit, sep = " ") else input$AggFunction
            train$seriesFinal <- aggregation_data(train$seriesNoNs, type = agg_type, func_aggregate = input$AggFunction)
            
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
                return(plot_time_series(data$seriesNoMis))
            } else if (input$TransformResSelector == 'outliers') {
                return(plot_time_series(data$seriesNoOut))
            } else if(input$TransformResSelector == 'noise') {
                return(plot_time_series(data$seriesNoNs))
            } else {
                return(plot_time_series(data$seriesFinal))
            }
        }
    })
})
