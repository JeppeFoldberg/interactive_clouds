pacman::p_load(
  shiny,
  wordcloud2,
  tm,
  memoise
)
# library(shiny)
# library(wordcloud2)

# Makes sure that we can upload larger files - here 30 mb! 
options(shiny.maxRequestSize = 30*1024^2)

ui <- fluidPage(
  titlePanel("Wordcloud creator"),
  
  sidebarLayout(
    sidebarPanel(
    # Input: Select a file ----
    fileInput("file", "Choose sav file",
              multiple = F,
              accept = c(".sav")),
    
    # Horizontal line ----
    tags$hr(),
    
    textInput("textcolumn", "Navn paa tekst-kolonne"),
    
    sliderInput("freq",
                "Minimum frekvens:",
                min = 1,  max = 50, value = 15),
    sliderInput("max",
                "Max antal ord:",
                min = 1,  max = 300,  value = 100)
    ),

   
    mainPanel(plotOutput("plot"))
  )
)

server <- function(input, output) {
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        file <- input$file
        getTermMatrix(file[input$textcolumn])
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud2)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
}

# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(text) {
  
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART")))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})


shinyApp(ui = ui, server = server)