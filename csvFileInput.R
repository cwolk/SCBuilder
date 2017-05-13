# Simple csv input module from the shiny module article

csvFileInput <- function(id, label = "CSV file") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fileInput(ns("file"), label),
    checkboxInput(ns("heading"), "Has heading", value = TRUE),
    selectInput(ns("quote"), "Quote", c(
      "None" = "",
      "Double quote" = "\"",
      "Single quote" = "'"
    ), selected = '"'),
    radioButtons(ns("type"), label = "Separator", choices=c("comma", "semicolon"),
                 selected="comma")
  )
}

# Module server function
csvFile <- function(input, output, session, stringsAsFactors) {
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })
  
  # The user's data, parsed into a data frame
  dataframe <- reactive({
    fun <- if (input$type == "comma") read.csv else read.csv2
    fun(userFile()$datapath,
             header = input$heading,
             quote = input$quote,
             stringsAsFactors = stringsAsFactors)
  })
  
  # # We can run observers in here if we want to
  # observe({
  #   msg <- sprintf("File %s was uploaded", userFile()$name)
  #   cat(msg, "\n")
  # })
  
  # Return the reactive that yields the data frame
  return(dataframe)
}
