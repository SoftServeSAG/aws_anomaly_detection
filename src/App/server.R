library(shiny)
library(dygraphs)
source("../visualization.R")
source("../remove_na_values.R")
source("../remove_outliers.R")
source("../denoise.R")
source("../aggregation.R")

shinyServer(
    function(input, output, session) {
        
        # data placeholders
        data <- reactiveValues(full = NULL, series = NULL, tsfPlotSelect = NULL)
        
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
        
        observeEvent(c(input$DateTimeVar, input$TargetVar), {
            if(input$DateTimeVar != "" && input$TargetVar != "") {
                shinyjs::enable("Next1Btn")
            }
        })
        
        # SECOND (PREPARE) PAGE CONTENT
        # plot original series
        output$OriginalSeries <- renderDygraph(plot_time_series(data$series))
        
        # add dynamic container to hold transformation results
        observeEvent(input$ApplyTsfBtn, {
            if(input$MissValTreatment != "") {data$tsfPlotSelect[["Missing"]] = "missing"}
            if(input$OutliersTreatment != "") {data$tsfPlotSelect[["Outliers"]] = "outliers"}
            if(input$NoiseTreatment != "") {data$tsfPlotSelect[["Noise"]] = "noise"}
            
            removeUI(selector = "#processed-results")
            
            if(length(data$tsfPlotSelect) > 0) {
                insertUI(selector = "#tranform-res-placeholder",
                         ui = div(id = "processed-results",
                             h3("Processed"),
                             hr(),
                             radioButtons("TransformResSelector", NULL, inline = T, choices = data$tsfPlotSelect),
                             fluidRow(
                                 conditionalPanel(
                                     condition = "input.TransformResSelector == 'missing'",
                                     dygraphOutput("MissingProcessedSeries")
                                 )
                             ),
                             fluidRow(
                                 conditionalPanel(
                                     condition = "input.TransformResSelector == 'outliers'",
                                     dygraphOutput("OutliersProcessedSeries")
                                 )
                             ),
                             fluidRow(
                                 conditionalPanel(
                                     condition = "input.TransformResSelector == 'noise'",
                                     dygraphOutput("NoiseProcessedSeries")
                                 )
                             )
                         )
                )
            }
        })
        
        # TODO: Load animation needed
        # TODO: do we need to save intermediate results?
        output$MissingProcessedSeries <- renderDygraph(plot_time_series(remove_na_from_data(data$series, type = input$MissValTreatment)))
        output$OutliersProcessedSeries <- renderDygraph(plot_time_series(remove_outliers_from_data(data$series, type = input$OutliersTreatment)))
        output$NoiseProcessedSeries <- renderDygraph(plot_time_series(denoise_data(data$series, type = input$NoiseTreatment)))
    }
)
