
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)
library(yaml)
source("csvFileInput.R", local = TRUE)

shinyServer(function(input, output, session) {

  config=reactiveValues()
  config$SearchTool <- list()
  config$Corpus <- list()
  
  observeEvent(input$appName, config$AppTitle <- input$appName)
  observeEvent(input$submitButton, config$useSubmitButton <- input$submitButton)
  
  observeEvent(input$KWICcolselect, 
               config$SearchTool$KWIC$DisplayExtraColumns <- input$KWICcolselect)
  
  observeEvent(input$Datacolselect, 
               config$SearchTool$Data$DisplayColumns <- input$Datacolselect)
  
  observeEvent(input$corpustype, { 
    config$Corpus$Type <- input$corpustype
    if (isTruthy(input$corpustype) && identical(input$corpustype, "pair")) {
      if (! "ShinyConc.mode" %in% names(config$Selectors)) {
        config$Selectors$ShinyConc.mode <- list()
        config$Selectors$ShinyConc.mode$label <- "Source"
        config$Selectors$ShinyConc.mode$type <- "radioButtons"
        config$Selectors$ShinyConc.mode$values <- c("Questions"="Q", 
                                                    "Answers"="A",
                                                    "Either"="Q|A",
                                                    "Both"="Q&A")
        config$Selectors$ShinyConc.mode$cascade <- FALSE
        config$Selectors$ShinyConc.mode$default <- "A"
      }
      if (! "Q" %in% config$searchTool$Data$DisplayColumns) {
        config$searchTool$Data$DisplayColumns <- c(
          config$searchTool$Data$DisplayColumns, "Q", "A")
        if ("text" %in% config$searchTool$Data$DisplayColumns)
          config$searchTool$Data$DisplayColumns <- 
            config$searchTool$Data$DisplayColumns[
              config$searchTool$Data$DisplayColumns != "text"
            ]
        updateSelectInput(session, "Datacolselect", choices = c(
          dcolumns(), "Q", "A"), selected = config$searchTool$Data$DisplayColumns)
      }
    } else {
      if ("ShinyConc.mode" %in% names(config$Selectors)) {
        config$selectors$ShinyConc.mode <- NULL
      }
      config$searchTool$Data$DisplayColumns <- c(
        config$searchTool$Data$DisplayColumns, "text")
      if ("Q" %in% config$searchTool$Data$DisplayColumns ||
          "A" %in% config$searchTool$Data$DisplayColumns )
        config$searchTool$Data$DisplayColumns <- 
        config$searchTool$Data$DisplayColumns[
          ! config$searchTool$Data$DisplayColumns %in% c("Q", "A")]
      updateSelectInput(session, "Datacolselect", choices = c(
        dcolumns(), "text"), selected = config$searchTool$Data$DisplayColumns)
    }
  })
  # TODO: switching between corpus types will leave remnant columns 


  observeEvent(input$corpussource, config$Corpus$Source <- input$corpussource)
  
  #
  # load and process csv file
  #
  
  datafile <- callModule(csvFile, "corpusmeta",
                         stringsAsFactors = FALSE)
  
  output$table <- renderDataTable({
    datafile()
  })
  
  output$fileUploaded <- reactive({
    return(!is.null(datafile()))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)

  observeEvent(datafile(), {
    updateSelectInput(session, "dcolumns", choices = dcolumns())
    config$Selectors <- list()
    for (col in dcolumns()) {
      if (! is.null(config$Selectors[[col]])) next
      config$Selectors[[col]] <- list(
        label=col,
        type="select",
        values="verbatim",
        cascade=FALSE,
        default=TRUE
      )
    }
    updateSelectInput(session, "KWICcolselect", choices = dcolumns())
    updateSelectInput(session, "Datacolselect", choices = 
                        c(dcolumns(), switch(input$corpustype,
                                             "pair"=c("Q", "A"),
                                             "line"="text",
                                             "text"="text")),
                      selected= config$searchTool$Data$DisplayColumns)
  })
  
  
  dcolumns <- reactive({
    cols <- colnames(datafile())
    if (isTruthy(input$corpustype) && identical(input$corpustype, "pair")) {
      cols <- c(cols, "ShinyConc.mode")
    }
    cols
  })
  
  output$dcolumns <- DT::renderDataTable(data.frame(column=dcolumns()),
                                         rownames=NULL, colnames=NULL,
                                         filter="none", selection="single",
                                         options = list(dom = 't'))
  
  activeCol <- reactive({
    if (isTruthy(input$dcolumns_rows_selected) && 
        length(input$dcolumns_rows_selected) > 0)
      dcolumns()[input$dcolumns_rows_selected]
    else NULL
  })
  
  
  output$colLabel <- renderText(paste0("Column ", activeCol()))
  
  mapValues <- function(x) {
    if (identical(x, "verbatim"))
      "verbatim" 
    else "fixed"
  }

  observeEvent(activeCol(), {
    if (isTruthy(activeCol)) {
      updateTextInput(session, "selLabel", 
                      value=config$Selectors[[activeCol()]]$label)
      updateSelectInput(session, "selectorType", 
                        selected=config$Selectors[[activeCol()]]$type)
      #browser()
      #updateRadioButtons(session, "values", selected = mapValues(
      #  config$Selectors[[activeCol()]]$values))
      updateCheckboxInput(session, "cascade", 
                          value=config$Selectors[[activeCol()]]$cascade)
      updateRadioButtons(session, "defaulttype", selected = if(
        identical(config$Selectors[[activeCol()]]$default, TRUE )) 
        "All" else "Choose:")
      updateSelectInput(
        session, "default", 
        choices = unique(datafile()[,activeCol()]),
        selected=if (identical(config$Selectors[[activeCol()]]$default, TRUE))
          NULL else config$Selectors[[activeCol()]]$default)
    }
  })
  
  observeEvent(input$selLabel, {
    if (isTruthy(activeCol()))
    config$Selectors[[activeCol()]]$label <- input$selLabel
  })
  
  observeEvent(input$selectorType, {
    if (isTruthy(activeCol()))
      config$Selectors[[activeCol()]]$type <- input$selectorType
  })
  
  output$valuesSelector <- renderUI({
    req(activeCol())
    default <- if (identical(mapValues(config$Selectors[[activeCol()]]$values), 
                  "fixed")) "fixed" else "verbatim"
    radioButtons("values", "Choices", choices = c(
      "Read from data"="verbatim",
      "Fixed"="fixed"
    ), selected = default)
  })
  
  observeEvent(input$values, {
    if (isTruthy(activeCol()))
      if (isTruthy(input$values) && identical(input$values, "verbatim") && 
          identical(mapValues(config$Selectors[[activeCol()]]$values), 
                    "fixed")) {
        config$Selectors[[activeCol()]]$values <- input$values
      }
  ## TODO properly handle fixed values
  })
  
  observeEvent(input$cascade, {
    if (isTruthy(activeCol()))
      config$Selectors[[activeCol()]]$cascade <- input$cascade
  })
  
  observeEvent(input$default, {
    if (isTruthy(activeCol()) & identical(input$defaulttype, "Choose:"))
      config$Selectors[[activeCol()]]$default <- input$default
  })
  
  observeEvent(input$defaulttype, {
    if (isTruthy(activeCol()) & identical(input$defaulttype, "All"))
      config$Selectors[[activeCol()]]$default <- TRUE
  })
  
  processSelectorOrder <- function(x) {
    rows <- strsplit(x, split = ";\\s*")[[1]]
    out <- list()
    lapply(rows, function(row) {
      entries <- strsplit(row, ",\\s*")[[1]]
      width = 12 / length(entries)
      lapply(entries, function(x) list(Selector=x, size=width))
    })
  }
  
  output$downloadApp <- downloadHandler("shinyconc.zip", 
                                        content = function(file) {
    z <- tempfile()
    dir.create(z)
    file.copy("ShinyApp", z, recursive = TRUE)
    write.csv(datafile(), file=paste0(z,"/ShinyApp/corpus/meta.csv"), 
              row.names = FALSE)
    config$SelectorOrder <- processSelectorOrder(input$selOrder)
    cat(as.yaml(reactiveValuesToList(config)), file = 
          paste0(z,"/ShinyApp/shinyconc.yaml"))
    oldwd <- getwd()
    setwd(z)
    zip(file, list.files(z))
    setwd(oldwd)
  })
  
  output$sourceOpts <- renderUI(
    if (identical(input$corpussource, "file" ))
      selectInput("charset", label= "Character Encoding", choices=c("Auto",
        "UTF-8", "ISO-8859-1", "ASCII", "windows-1252"
      ), selected = "Auto")
  )
  
  observeEvent(input$charset, {
    config$corpusCharset <- input$charset
  })    
  
})
