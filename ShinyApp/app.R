if (! (require("ShinyConc", character.only = TRUE))) {
  if (! (require("devtools", character.only = TRUE)))
    install.packages("devtools")
  devtools::install_github("cwolk/ShinyConc")
  library("ShinyConc", character.only=TRUE)
}

loadOrInstall(c("shiny", "stringr", "DT", "dplyr", "shinyBS", "yaml"))
conf <- yaml.load_file("shinyconc.yaml")

if (identical(conf$Corpus$Source, "custom"))
    source("setup.R", local=TRUE) else {
       corpus <- loadCorpus("corpus", read.csv("corpus/meta.csv",
                                               stringsAsFactors=FALSE), 
                             conf$Corpus$Source, conf$Corpus$Type, conf)
       selectCorpus <- function(...) corpus
}
       
ui <- shinyUI(fluidPage(
   tags$head(tags$link(rel = "stylesheet", type="text/css", href="style.css"),
             tags$script(src = "button.js")),

   titlePanel(conf$AppTitle),

   sidebarLayout(
      sidebarPanel(
        corpusUIVerticalInput("mainCorpus", conf, selectCorpus, TRUE),
        width=4),
      mainPanel(
        restrictionNotificationUI("restrictionNotification"),
        tabsetPanel(
          tabPanel("Search",
            searchModuleOutput("searchTool"),
            searchContextOutput("searchContext", conf)),
          countModuleOutput("countTool", panel = "Count"),
          compareModuleOutput("compareTool", selectCorpus, conf, "Compare"),
          id="mainTabset")))))

server <- shinyServer(function(input, output,session) {

  appControl <- list()
  appControl$setTab <- function(tab) 
    updateTabsetPanel(session, "mainTabset", tab)
  appControl$setSearchMode <- function(smode)
    updateRadioButtons(session, "searchType", selected=smode)
  
  mainCorpus <- callModule(corpusUIInput, "mainCorpus", conf, selectCorpus,
                           TRUE)

  restrictionNotification <- callModule(restrictionNotificationModule,
                                        "restrictionNotification",
                                        mainCorpus)
  
  searchTool <- callModule(searchModule, "searchTool", conf, mainCorpus,
                           appControl)
  
  searchContext <- callModule(searchContextModule, "searchContext", conf, 
                           searchTool, mainCorpus) 
  
  countTool <- callModule(countModule, "countTool", conf, mainCorpus, 
                          appControl)
  
  compareTool <- callModule(compareModule, "compareTool", conf, 
                            selectCorpus, mainCorpus$restrictions, mainCorpus,
                            appControl)
  
})

# Run the application 
shinyApp(ui = ui, server = server)