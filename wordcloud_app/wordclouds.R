library(shiny)

# Makes sure that we can upload larger files - here 30 mb! 
options(shiny.maxRequestSize = 30*1024^2)

ui <- fluidPage(
  titlePanel("Wordcloud creator"),
  
  sidebarLayout(
    sidebarPanel(
    # Input: Select a file ----
    fileInput("file1", "Choose sav file",
              multiple = F,
              accept = c(".sav")),
    
    # Horizontal line ----
    tags$hr(),
    
    textInput("textcolumn", "Navn på tekst-kolonne"),
    
    sliderInput("freq",
                "Minimum frekvens:",
                min = 1,  max = 50, value = 15),
    sliderInput("max",
                "Max antal ord:",
                min = 1,  max = 300,  value = 100)
    ),

   
    mainPanel("Wordcloud")
  )
)

server <- function(input, output) {
  
}

shinyApp(ui = ui, server = server)