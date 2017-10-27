library(shiny)

shinyServer(
    function(input, output) {
        
        # data placeholders
        data <- reactiveValues(full = NULL, series = NULL)
        
        # First (Load) page content
        # Load table with uploaded data
        output$UserData <- DT::renderDataTable({
            req(input$LocalFile)
            data$full <- read.csv(input$LocalFile$datapath, 
                                  header = input$FileHasHeader, 
                                  sep = input$FileSeparator, 
                                  quote = input$FileQuotation)
            return(data$full)
        }, 
        selection = 'none',
        options = list(dom='tsp'))
        
        # Load dynamic controls
        output$TimeScaleHr <- renderPrint({
            if(!is.null(data$full)) {
                h3("Time Scale")
            }
        })
        
        output$DateTimeVarName <- renderUI({
            if(!is.null(data$full)) {
                fluidRow(
                    column(6,
                        tags$div("Column name", style = "padding: 5px;font-size: 110%;")
                    ),
                    column(6,
                           selectInput("DateTimeVar", NULL, choices = names(data$full))
                    )
                )
            }
        })
        
        output$DateTimeFmt <- renderUI({
            if(!is.null(data$full)) {
                fluidRow(
                    column(6,
                           tags$div("Datetime format", style = "padding: 5px;font-size: 110%;")
                    ),
                    column(6,
                           textInput("DateTimeFmt", NULL, placeholder = "HH:MM:SS")
                    )
                )
            }
        })
        
        output$TargetVarHr <- renderPrint({
            if(!is.null(data$full)) {
                h3("Target variable")
            }
        })
        
        output$TargetVarName <- renderUI({
            if(!is.null(data$full)) {
                fluidRow(
                    column(6,
                           tags$div("Column name", style = "padding: 5px;font-size: 110%;")
                    ),
                    column(6,
                           selectInput("TargetVar", NULL, choices = names(data$full[sapply(data$full, is.numeric)]))
                    )
                )
            }
        })
        
        output$Next1Button <- renderUI({
            if(!is.null(data$full)) {
                actionButton("Next1Btn", "Next", width = '100%')
            }
        })
    }
)