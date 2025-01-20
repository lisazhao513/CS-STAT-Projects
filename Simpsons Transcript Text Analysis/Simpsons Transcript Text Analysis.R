library(tidyverse)
library(tidytext)

# ===============================================
# Import Data 
# ===============================================
simpsons_transcripts <- read.delim(file = "simpsons-transcripts.txt", 
                                   sep = "^",
                                   dec = ".")


# ===============================================
# Define "ui" for application
# ===============================================
ui <- fluidPage(
  
  titlePanel("Text Analysis of The Simpsons Transcripts"),
  fluidRow(
    column(2,
           p(em("Parameters")),
           radioButtons(inputId = "season_type", 
                        label = "Choose an option", 
                        choices = c("All Seasons" = "all_seasons",
                                    "Given Season" = "given_season",
                                    "Per Season" = "per_season"), 
                        selected = "given_season")
    ),
    column(3,
           p(em("Parameters")),
           selectInput(inputId = "num_season", 
                       label = "Select a Given Season",
                       choices = c("Season 1" = 1,
                                   "Season 2" = 2,
                                   "Season 3" = 3, 
                                   "Season 4" = 4,
                                   "Season 5" = 5,
                                   "Season 6" = 6,
                                   "Season 7" = 7,
                                   "Season 8" = 8,
                                   "Season 9" = 9,
                                   "Season 10" = 10,
                                   "Season 11" = 11,
                                   "Season 12" = 12,
                                   "Season 13" = 13,
                                   "Season 14" = 14,
                                   "Season 15" = 15,
                                   "Season 16" = 16,
                                   "Season 17" = 17,
                                   "Season 18" = 18,
                                   "Season 19" = 19,
                                   "Season 20" = 20,
                                   "Season 21" = 21,
                                   "Season 22" = 22,
                                   "Season 23" = 23,
                                   "Season 24" = 24,
                                   "Season 25" = 25,
                                   "Season 26" = 26,
                                   "Season 27" = 27,
                                   "Season 28" = 28,
                                   "Season 29" = 29,
                                   "Season 30" = 30,
                                   "Season 31" = 31,
                                   "Season 32" = 32,
                                   "Season 33" = 33),
                       selected = 1)
        ),
    column(1),
    column(3,
           p(em("Word Frequency Analysis")),
           numericInput(inputId = "top_words", 
                        label = "Top-n Most Frequent Words",
                        value = 5),
           checkboxInput(inputId = "remove_stopwords",
                         label = "Remove Stopwords",
                         value = TRUE)
    ),

    # replace with your widgets
    column(3,
           p(em("N-Gram Analysis")),
           sliderInput(inputId = "num_grams",
                       label = "Choose n-grams n",
                       min = 1,
                       max = 5,
                       value = 2),
           numericInput(inputId = "top_ngrams", 
                        label = "Top-n Most Frequent n-grams",
                        value = 5)
    ),
  ),
  hr(),
  
  tabsetPanel(type = "tabs",
              tabPanel("Word Frequency Analysis)",
                       h3("Word Frequency Analysis"),
                       plotOutput("plot1",
                                  height = "1200px"),
                       hr(),
                       dataTableOutput('table1')),
              tabPanel("N-Gram Analysis)",
                       h3("N-Gram Analysis"),
                       plotOutput("plot2"),
                       hr(),
                       dataTableOutput('table2'))
  )
)


# ===============================================
# Define Server "server" logic
# ===============================================
server <- function(input, output) {
  
  tot_seasons <- reactive({
    simpsons_transcripts %>%
      pull(season) %>%
      unique() %>%
      length()
  })
  
  rm_stopwords_transcript <- reactive({
    rm_data <- simpsons_transcripts %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) %>%
      select(season, word)
    
    rm_stopwords <- group_by(rm_data, season) %>% 
      summarize(text = str_c(word, collapse = " ")) 
    
    rm_stopwords
  })

  dat = reactive({
    if (input$remove_stopwords){
      if (input$season_type == "given_season"){
        # find max words for specific season
        words <- rm_stopwords_transcript() %>% 
          filter(season == input$num_season) %>% 
          pull(text)
        words <- str_extract_all(words, "(\\w|-\\w|')+") 
        
        words <- unlist(words) 
        words <- data.frame(words)
        
        words <- words %>%
          group_by(words) %>%
          count() %>%
          arrange(desc(n)) 
        
        dat <- words[1:input$top_words, ]
        dat
      }
      else if (input$season_type == "all_seasons"){
        # find max words for specific season
        words <- rm_stopwords_transcript() %>% 
          pull(text)
        words <- str_extract_all(words, "(\\w|-\\w|')+") 
        
        words <- unlist(words) 
        words <- data.frame(words)
        
        words <- words %>%
          group_by(words) %>%
          count() %>%
          arrange(desc(n)) 
        
        dat <- words[1:input$top_words, ]
        dat
      }
      else if (input$season_type == "per_season"){
        season_words <- c()
        season_count <- c()
        season_label <- c()
        # for every season 
        for (i in 1:tot_seasons()){
          words <- rm_stopwords_transcript() %>% 
            filter(season == i) %>%
            pull(text)
          words <- str_extract_all(words, "(\\w|-\\w|')+") 
          
          words <- unlist(words) 
          words <- data.frame(words)
          
          words <- words %>%
            group_by(words) %>%
            count() %>%
            arrange(desc(n)) %>%
            head(input$top_words) 
          
          season_words <- c(season_words, pull(words, words))
          season_count <- c(season_count, pull(words, n))
          season_label <- c(season_label, rep(i, input$top_words))
        }
        
        dat = data.frame(season_words, season_count, season_label)
        dat
      }
    }
    else if (!input$remove_stopwords){
      if (input$season_type == "given_season"){
        # find max words for specific season
        words <- simpsons_transcripts %>% 
          filter(season == input$num_season) %>% 
          pull(text)
        words <- str_extract_all(words, "(\\w|-\\w|')+") 
        
        words <- unlist(words) 
        words <- data.frame(words)
        
        words <- words %>%
          group_by(words) %>%
          count() %>%
          arrange(desc(n)) 
        
        dat <- words[1:input$top_words, ]
        dat
      }
      else if (input$season_type == "all_seasons"){
        # find max words for specific season
        words <- simpsons_transcripts %>% 
          pull(text)
        words <- str_extract_all(words, "(\\w|-\\w|')+") 
        
        words <- unlist(words) 
        words <- data.frame(words)
        
        words <- words %>%
          group_by(words) %>%
          count() %>%
          arrange(desc(n)) 
        
        dat <- words[1:input$top_words, ]
        dat
      }
      else if (input$season_type == "per_season"){
        season_words <- c()
        season_count <- c()
        season_label <- c()
        # for every season 
        for (i in 1:tot_seasons()){
          words <- simpsons_transcripts %>% 
            filter(season == i) %>%
            pull(text)
          words <- str_extract_all(words, "(\\w|-\\w|')+") 
          
          words <- unlist(words) 
          words <- data.frame(words)
          
          words <- words %>%
            group_by(words) %>%
            count() %>%
            arrange(desc(n)) %>%
            head(input$top_words) 
          
          season_words <- c(season_words, pull(words, words))
          season_count <- c(season_count, pull(words, n))
          season_label <- c(season_label, rep(i, input$top_words))
        }
        
        dat = data.frame(season_words, season_count, season_label)
        dat
      }
    }
  })
  
  # ===============================================
  # Outputs for the first TAB
  # ===============================================

  # code for plot1
  output$plot1 <- renderPlot({
    if (ncol(dat()) == 2){
      order <- dat() %>% 
        pull(words)
      
      ggplot(data=dat(), aes(x= factor(words, level = order), y=n, fill=n)) + 
        geom_col(show.legend = FALSE) +
        labs(title = "", x = "Word", y = "Count") +
        scale_fill_continuous(trans = "reverse") +
        theme_minimal()
    }
    else if (ncol(dat()) == 3){
      # ggplot(data=dat(), aes(x = season_label,
      #                       y = season_count)) +
      #   geom_col(aes(fill = season_words)) +
      #   labs(title = "Per Season", x = "Season", y = "Count")

      ggplot(dat(), aes(season_words, season_count, fill = season_label)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ season_label, ncol = 3, scales = "free") +
        labs(x = "Season", y = NULL) +
        scale_fill_continuous(trans = "reverse") +
        theme_minimal()
    }
  })
  
  # code for numeric summaries of frequencies
  output$table1 <- renderDataTable({
    dat()
  })
  
  
  # ===============================================
  # Outputs for the second TAB
  # ===============================================
  simpsons_bigrams <- reactive({
    if (input$season_type == "given_season"){
      bigrams <- simpsons_transcripts %>%
        filter(season == input$num_season) %>%
        unnest_tokens(bigram, text, token = "ngrams", n = input$num_grams) %>%
        filter(!is.na(bigram)) %>%
        count(bigram, sort = TRUE)
      
      bigrams
    }
    else if (input$season_type == "all_seasons"){
      bigrams <- simpsons_transcripts %>%
        unnest_tokens(bigram, text, token = "ngrams", n = input$num_grams) %>%
        filter(!is.na(bigram)) %>%
        count(bigram, sort = TRUE)
      
      bigrams
    }
    else if (input$season_type == "per_season"){
      datalist = vector("list", length = tot_seasons())
      for (i in 1:tot_seasons()){
        simpsons_bigrams <- simpsons_transcripts %>%
          filter(season == i) %>%
          unnest_tokens(bigram, text, token = "ngrams", n = input$num_grams) %>%
          filter(!is.na(bigram)) %>%
          count(bigram, sort = TRUE)

        simpsons_bigrams <- simpsons_bigrams %>%
          add_column(season = rep(i, nrow(simpsons_bigrams)))

        datalist[[i]] <- simpsons_bigrams
      }
      big_data <- data.table::rbindlist(datalist)
      big_data
    }
  })
  
  bigrams_united = reactive({
    names <- c()
    for (i in 1:input$num_grams){
      names <- c(names, paste0("word", i))
    }
    bigrams_separated <- simpsons_bigrams() %>%
      separate(bigram, names, sep = " ")
    
    
    bigrams_filtered <- {
      if (input$num_grams >= 1){
        bigrams_filtered <- bigrams_separated %>%
          filter(!word1 %in% stop_words$word)
      }
      else if (input$num_grams >= 2){
        bigrams_filtered <- bigrams_filtered %>%
          filter(!word2 %in% stop_words$word)
      }
      else if (input$num_grams >= 3){
        bigrams_filtered <- bigrams_filtered %>%
          filter(!word3 %in% stop_words$word)
      }
      else if (input$num_grams >= 4){
        bigrams_filtered <- bigrams_filtered %>%
          filter(!word4 %in% stop_words$word)
      }
      else if (input$num_grams >= 5){
        bigrams_filtered <- bigrams_filtered %>%
          filter(!word5 %in% stop_words$word)
      }
    }
    
    united <- {
      if (input$num_grams == 1){
        united <- bigrams_filtered %>%
          unite(bigram, word1, sep = " ")
      }
      else if (input$num_grams == 2){
        united <- bigrams_filtered %>%
          unite(bigram, word1, word2, sep = " ")
      }
      else if (input$num_grams == 3){
        united <- bigrams_filtered %>%
          unite(bigram, word1, word2, word3, sep = " ")
      }
      else if (input$num_grams == 4){
        united <- bigrams_filtered %>%
          unite(bigram, word1, word2, word3, word4, sep = " ")
      }
      else if (input$num_grams == 5){
        united <- bigrams_filtered %>%
          unite(bigram, word1, word2, word3, word4, word5, sep = " ")
      }
    }
    united
  })
  
  dat2 <- reactive({
    if (input$season_type == "given_season" | input$season_type == "all_seasons"){
      data <- bigrams_united()
      data[1:input$top_ngrams, ]
    }
    else if (input$season_type == "per_season"){
      data <- bigrams_united()
      data <- data[ , .SD[1:input$top_ngrams], by = season] 
      data <- data[ , c("bigram", "n", "season")]
      data[1:15, ]
    }
  })
  
  # code for plot2
  output$plot2 <- renderPlot({
    # replace the code below with your code!!!
    if (ncol(dat2() == 2)){
      order <- dat2() %>%
        pull(bigram) %>%
        rev()
      
      ggplot(dat = dat2(), aes(x = n, y = factor(bigram, order), fill = n)) +
        geom_col(show.legend = FALSE) +
        labs(x = "Number of n-gram", 
             y = "n-gram",
             title = "Most Frequent n-grams") +
        scale_fill_continuous(trans = "reverse") +
        theme_minimal()
    }
    else if (ncol(dat2() == 3)){
      order2 <- dat2() %>%
        pull(bigram) %>%
        rev()
      
      ggplot(dat2(), aes(x = n, 
                         y = factor(bigram, order2), 
                         fill = n)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ season, ncol = 3, scales = "free") +
        labs(x = "Number of n-gram", 
             y = "n-gram",
             title = "Most Frequent n-grams Per Season") +
        scale_fill_continuous(trans = "reverse") +
        theme_minimal() 
    }
  })
  
  # code for statistics
  output$table2 <- renderDataTable({
    # replace the code below with your code!!!
    dat2()
  })
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

