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
    # data placeholders
    data <- reactiveValues(
                full = NULL,
                series = NULL,
                seriesNoMis = NULL,
                seriesNoOut = NULL,
                seriesNoNs = NULL,
                seriesFinal = NULL,
                aggData = NULL)
    
    # FIRST (LOAD) PAGE CONTENT
    # read uploaded file and render contents into a table
    output$UserData <- DT::renderDataTable({
        req(input$LocalFile)
        data$full <- read.csv(
            input$LocalFile$datapath,
            header = input$FileHasHeader,
            sep = input$FileSeparator,
            quote = input$FileQuotation,
            stringsAsFactors = F
        )
        return(data$full)
    },
    selection = 'none',
    options = list(dom = 'tsp'))
    
    # store 'data loaded' flag in outputs for dynamic UI
    output$UserDataLoaded <- reactive({
        !is.null(data$full)
    })
    outputOptions(output, "UserDataLoaded", suspendWhenHidden = FALSE)
    
    output$DateTimeVarName <- renderUI({
        selectizeInput("DateTimeVar", NULL, choices = names(data$full),
            options = list(
                placeholder = "Select",
                onInitialize = I('function() { this.setValue(""); }')
            )
        )
    })
    
    output$TargetVarName <- renderUI({
        selectizeInput("TargetVar", NULL,
            choices = names(data$full[sapply(data$full, is.numeric)]),
            options = list(
                placeholder = "Select",
                onInitialize = I('function() { this.setValue(""); }')
            )
        )
    })
    
    # process next button click (slice dataframe into series and move to the Prepare stage)
    observeEvent(input$Next1Btn, {
        withProgress(message = "", value = 0, style = "old", {
            data$series <- data$full[, c(input$DateTimeVar, input$TargetVar)]
            if (input$DateTimeCh=="Manualy")
                data$series[,input$DateTimeVar] <- strptime(data$series[,input$DateTimeVar], format=input$DateTimeFmt) %>% as.POSIXct() 
            else
                data$series[,input$DateTimeVar] <- strptime(data$series[,input$DateTimeVar], format=input$DateTimeCh) %>% as.POSIXct() 
            
            data$series <- xts(data$series[, 2], order.by = data$series[, 1])
            data$aggData <- timeSliders(data$series[, 1])
            updateTabsetPanel(session, "MNB", selected = "Prepare")
        })
    })
    
    observeEvent(c(input$DateTimeVar, input$TargetVar), {
        if (input$DateTimeVar != "" && input$TargetVar != "") {
            shinyjs::enable("Next1Btn")
        }
    })
    
    # SECOND (PREPARE) PAGE CONTENT
    # plot original series
    output$OriginalSeries <- renderDygraph(if (!is.null(data$series)) plot_time_series(data$series))
    
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
            data$seriesNoMis <- remove_na_from_data(data$series, type = input$MissValTreatment)
            data$seriesNoOut <-
                remove_outliers_from_data(data$seriesNoMis, type = input$OutliersTreatment, number = input$OutliersSigmaCount)
            win_ns <- if(input$NoiseWindowType != 'auto') input$NoiseWindowSize else input$NoiseWindowType
            data$seriesNoNs <- denoise_data(data$seriesNoOut, type = input$NoiseTreatment, window_noise = win_ns)
            agg_type <- if(input$AggFunction != 'none') paste(input$AggCount, input$AggUnit, sep = " ") else input$AggFunction
            data$seriesFinal <- aggregation_data(data$seriesNoNs, type = agg_type, func_aggregate = input$AggFunction)
            
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
    
    output$DateTimeF <- renderUI({
        if (input$DateTimeCh=="Manualy")
        {
            return(list(textInput("DateTimeFmt", NULL, placeholder = "HH:MM:SS"),
                        bsPopover("DateTimeFmt", "Datetime format", "Specify datetime format for time scale", placement="top")))
        }
        
    })
})
