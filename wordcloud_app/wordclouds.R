pacman::p_load(
  shiny,
  dplyr,
  haven,
  wordcloud2,
  tm,
  epinionR,
  quanteda,
  memoise
)

# Makes sure that we can upload larger files - here 30 mb! 
options(shiny.maxRequestSize = 30*1024^2)

ui <- fluidPage(
  titlePanel("Wordcloud vaerktoej"),
  
  sidebarLayout(
    sidebarPanel(
    # Input: Select a file ----
    fileInput("file", "Vaelg sav fil",
              accept = c(".sav")),

    # Horizontal line ----
    tags$hr(),

    textInput("textColumn", "Navn paa tekst-kolonne", value="q9_22"),

    sliderInput("freq",
                "Minimum frekvens:",
                min = 1,  max = 50, value = 1),
    sliderInput("max",
                "Max antal ord:",
                min = 1,  max = 300,  value = 100),
    actionButton("update", "Opdater")
    ),

   
    mainPanel(wordcloud2Output("wordcloud"))
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
        
        # file <- read_sav("../tekst_test_data.sav") %>% 
        #   as_factor()
        req(input$file)
        
        file <- input$file$datapath
        
        data <- read_sav(file) %>%
          as_factor() %>% 
          pull(input$textColumn)
        

        count_words(data,
                    stopwords = c('ja'),
                    n_words = input$max,
                    mention_limit = input$freq)
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud2)

  # getting colors for the word cloud - unnaming to get only vector of colors!
  colors <- unname(epi_palettes$full[-1]) # indexing to avoid epinion Red in cloud!

  output$wordcloud <- renderWordcloud2({
    v <- terms()
    wordcloud_rep(v,
                  fontFamily = 'Arial',
                  color = rep_len(colors, NROW(temp)),
                  rotateRatio = 0)
  })
}

load_stopwords <- function() {
  stop <- read.csv("../stopord.txt", 
                 sep = "\n",
                 fileEncoding = "UTF-8",
                 header = F) 

  as.character(stop$V1)
}

count_words <- function(text_data,
                        stopwords = load_stopwords(),
                        mention_limit=10,
                        n_words = 20,
                        patterns=NULL
) {
  tkns <- quanteda::tokens(text_data,
                           remove_punct = TRUE,
                           remove_symbols = TRUE,
                           remove_numbers = TRUE,
                           remove_url = TRUE,
                           remove_separators = TRUE,
                           split_hyphens = TRUE,
                           include_docvars = TRUE,
                           padding = F)
 
  dfm_question <- quanteda::dfm(x = tkns, tolower = TRUE)
  
  # problems? Search for dplyr and NSE (non standard evaluation)
  # there is no abundant information about how to do it properly.
  # https://shipt.tech/https-shipt-tech-advanced-programming-and-non-standard-evaluation-with-dplyr-e043f89deb3d
  # This is the solution used (under the section 'a note on tidyverse')
  # https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
  df <- dfm_question %>%
    quanteda::convert(to = "data.frame") %>%
    tidyr::pivot_longer(!"doc_id",
                        names_to = "word",
                        values_to = "count") %>%
    dplyr::filter(.data$count > 0) %>%
    dplyr::filter(!.data$word %in% stopwords)
  
  count <- df %>%
    dplyr::group_by(.data$word) %>%
    dplyr::summarise(mentions = sum(.data$count, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$mentions >= mention_limit) %>% 
    arrange(desc(.data$mentions)) %>% 
    slice_head(n = n_words)
}


shinyApp(ui = ui, server = server)