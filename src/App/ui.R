library(shiny)
library(DT)

shinyUI(
    fluidPage(
        navbarPage(
            "Anomaly Detection",
            tabPanel(
                "Load",
                fluidPage(
                    fluidRow(
                        column(3,
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
                               )
                        ),
                        column(9,
                               h2("Preview"),
                               DT::dataTableOutput('UserData'),
                               fluidRow(
                                   column(3,
                                          uiOutput("TimeScaleHr"),
                                          uiOutput("DateTimeVarName"),
                                          uiOutput("DateTimeFmt")
                                   ),
                                   column(3,
                                          uiOutput("TargetVarHr"),
                                          uiOutput("TargetVarName")
                                   )
                               )
                        ),
                        fluidRow(
                            column(1, offset = 11,
                                   uiOutput("Next1Button")
                            )
                        )
                    )
                )
            ),
            tabPanel(
                "Prepare"
            ),
            tabPanel(
                "Model"
            ),
            tabPanel(
                "Evaluate"
            )
        )
    )
)