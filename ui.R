
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)

source("csvFileInput.R", local = TRUE)

shinyUI(fluidPage(

  # Application title
  titlePanel("ShinyConc Builder"),
  
  tags$style(type = 'text/css', 
             "footer img {width: 100%;max-width: 200px}"
  ),
  
  sidebarLayout(
    sidebarPanel(
      csvFileInput("corpusmeta", h3("Load CSV file")),
      downloadButton("downloadApp", "Create App")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      conditionalPanel(condition="! output.fileUploaded",
        h1("Welcome to ShinyConc"),
        p("Load a corpus (metadata) CSV file to begin. This CSV file should either contain a column with file names or one or two corpus columns, as well as any metadata columns.")),
      conditionalPanel(
        condition="output.fileUploaded",
        wellPanel(
          h3("Basic setup"),
          textInput("appName","App Name"),
#          checkboxInput("submitButton", "Use Submit Button", value=TRUE),
          radioButtons("corpustype", "Corpus type", choices=c(
            "Texts"="text",
            "Lines"="line",
            "Q/A pairs"="pair"),
            inline=TRUE),
          radioButtons("corpussource", "Read corpus from", choices=c(
            "Text file"="file",
            "CSV" = "csv",
            "Custom" = "custom"),
            inline=TRUE),
          uiOutput("sourceOpts")
        ),
        wellPanel(
          h3("Selector definitions"),
          fluidRow(
            column(4,
                   #selectInput("selectColumns", "Available columns", 
                   #           choices=NULL, selectize=FALSE)
                   DT::dataTableOutput("dcolumns")
                   ),
            column(8,
                   conditionalPanel(
                     condition = "typeof input.dcolumns_rows_selected  === 
                                  'undefined' || 
                                  input.dcolumns_rows_selected.length  <= 0",
                     p("Select columns to configure them")
                   ),
                   conditionalPanel(
                     condition = "typeof input.dcolumns_rows_selected  !== 
                                  'undefined' && 
                                  input.dcolumns_rows_selected.length > 0" ,
                     textOutput("colLabel"),
                     textInput("selLabel", "Display as"),
                     selectInput("selectorType", "Choose as:", choices = c(
                       "List"="select",
                       "Radio Buttons"="radioButtons",
                       "Checkboxes"="checkboxGroup"
                     )),
                     uiOutput("valuesSelector"),
                     checkboxInput("cascade", 
                                   "Filter Choices by previous selections", 
                                   value = FALSE),
                     fluidRow(
                       column(
                         6, radioButtons("defaulttype", "Select by default", 
                                         choices=c("All", "Choose:"), 
                                         selected = "All", inline=TRUE)),
                      column(
                        6, selectInput("default", "", choices = NULL, 
                                       selectize=TRUE, multiple=TRUE)
                   )
                   )
          )
        ))),
        wellPanel(
          h3("Selector Order"),
          p("Enter the names of the columns to show in the order they are supposed to appear. Separate Selectors on the same row with a comma (','), separate rows with a semicolon (';')."),
          textInput("selOrder", "Selector Order")
        ),
        wellPanel(
          h3("Tool Options"),
          selectInput("KWICcolselect", "Extra columns in KWIC View", 
                      choices=NULL, selected = NULL, multiple=TRUE),
          selectInput("Datacolselect", "Extra columns in Data View", 
                      choices=NULL, selected = NULL, multiple=TRUE),
        DT::dataTableOutput("table"))
    ))
),
hr(),
tags$footer(
#div(class = "footer",
    fluidRow(
      column(4, img(src="jlu-logo-600.png")),
      column(8, p("ShinyConc Builder (c) Christoph Wolk 2017. Presented by the University of Giessen.")))
    #      includeHTML("footer.html")
)
))
