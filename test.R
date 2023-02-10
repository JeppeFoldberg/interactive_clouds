pacman::p_load(
  shiny,
  dplyr,
  haven,
  wordcloud2,
  epinionR,
  quanteda,
  memoise
)

file <- read_sav("tekst_test_data.sav") %>% 
  as_factor() %>% 
  pull('q9_22')



temp <- file['q9_22']
text <- file$q9_22
# attempt with Quanteda ---------------------------------------------------
load_stopwords <- function() {
  stop <- read.csv("stopord.txt", 
                 sep = "\n",
                 fileEncoding = "UTF-8",
                 header = F) 

  as.character(stop$V1)
}


count_words <- function(text_data,
                        stopwords = c(),
                        mention_limit=10,
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
    arrange(desc(.data$mentions))
}

temp <- count_words(file$q9_22, 
                    mention_limit = 1)

# getting colors for the word cloud - unnaming to get only vector of colors! 
colors <- unname(epi_palettes$full[-1]) # indexing to avoid epinion Red in cloud!

wc <- wordcloud2(temp,
           fontFamily = 'Arial',
           color = rep_len(colors, NROW(temp)),
           rotateRatio = 0)
