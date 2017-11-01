library(shiny)
library(dygraphs)
source("../visualization.R")

shinyServer(
    function(input, output, session) {
        
        # data placeholders
        data <- reactiveValues(full = NULL, series = NULL)
        
        # FIRST (LOAD) PAGE CONTENT
        # read uploaded file and render contents into a table
        output$UserData <- DT::renderDataTable({
            req(input$LocalFile)
            data$full <- read.csv(input$LocalFile$datapath, 
                                  header = input$FileHasHeader, 
                                  sep = input$FileSeparator, 
                                  quote = input$FileQuotation,
                                  stringsAsFactors = F)
            return(data$full)
        }, 
        selection = 'none',
        options = list(dom='tsp'))
        
        # store 'data loaded' flag in outputs for dynamic UI
        output$UserDataLoaded <- reactive({!is.null(data$full)})
        outputOptions(output, "UserDataLoaded", suspendWhenHidden = FALSE)
        
        output$DateTimeVarName <- renderUI({
            selectizeInput("DateTimeVar", NULL, choices = names(data$full), 
                           options = list(
                               placeholder = "Select",
                               onInitialize = I('function() { this.setValue(""); }')
                           ))
        })
        
        output$TargetVarName <- renderUI({
            selectizeInput("TargetVar", NULL, choices = names(data$full[sapply(data$full, is.numeric)]),
                           options = list(
                               placeholder = "Select",
                               onInitialize = I('function() { this.setValue(""); }')
                           ))
        })
        
        # process next button click (slice dataframe into series and move to the Prepare stage)
        observeEvent(input$Next1Btn, {
            data$series <- data$full[, c(input$DateTimeVar, input$TargetVar)]
            data$series[,input$DateTimeVar] <- strptime(data$series[,input$DateTimeVar], format=input$DateTimeFmt) %>% as.POSIXct() 
            data$series <- xts(data$series[,2], order.by=data$series[,1])
            updateTabsetPanel(session, "MNB", selected = "Prepare")
        })
        
        observeEvent(input$DateTimeVar, {
            if(input$DateTimeVar != "" && input$TargetVar != "") {
                shinyjs::enable("Next1Btn")
            }
        })
        
        observeEvent(input$TargetVar, {
            if(input$DateTimeVar != "" && input$TargetVar != "") {
                shinyjs::enable("Next1Btn")
            }
        })
        
        # SECOND (PREPARE) PAGE CONTENT
        output$OriginalSeries <- renderDygraph(plot_time_series(data$series))
    }
)
