# Data load
## Load n-grams
load(file="./prediction-word/pentagram.Rda")
load(file="./prediction-word/quadrigram.Rda")
load(file="./prediction-word/trigram.Rda")
load(file="./prediction-word/bigram.Rda")

## Load Swear_Words -  only for English swear words
data("swear_words")
swear_words = swear_words %>% filter(language == "en") %>% select(word)

# Functions

## Input Cleaning

prep_input <- function(input,df_rm_words) {
  input_clean <- tolower(input) #Transform all characters in lowercase 
  input_clean <- rm_non_ascii(input_clean) #Remove non ASCII words
  input_clean <- gsub(".*?($|'|[^[:punct:]]).*?","\\1", input_clean)
  input_clean <- gsub("[[:digit:]]+", "", input_clean)
  input_clean <- removeWords(input_clean,words = df_rm_words$word)
  input_clean <- str_squish(input_clean)
  input_clean <- word(input_clean,start = -4,end = -1) #Restrict for the last 4 words
  return(input_clean)   
}

## Prediction Function

predict_word <- function(df_text) {
  
  qty_words_input <- str_count(df_text,pattern = "\\w+") #Quantity of words in the input
  final_words <- tibble(pred_word = character(0))
  
  #Pentagram Prediction
  if (qty_words_input == 4) {
    predict_words <- pentagram %>% 
      filter(input_setence == df_text & !(pred_word %in% final_words$pred_word)) %>%
      select(pred_word) %>% slice(1:(3-nrow(final_words)))
    
    if (nrow(predict_words)+nrow(final_words)< 3) {
      qty_words_input <- 3
      df_text <- word(df_text,start = -3,end = -1)
    }
    final_words <- rbind(final_words,predict_words)
  }
  
  #Quadrigram Prediction
  if (qty_words_input == 3) {
    predict_words <- quadrigram %>% 
      filter(input_setence == df_text & !(pred_word %in% final_words$pred_word)) %>%
      select(pred_word) %>% slice(1:(3-nrow(final_words)))
    
    if (nrow(predict_words) + nrow(final_words) < 3) {
      qty_words_input <- 2
      df_text <- word(df_text,start = -2,end = -1)
    }
    final_words <- rbind(final_words,predict_words)
  }
  
  #Trigram Prediction
  if (qty_words_input == 2) {
    predict_words <- trigram %>% 
      filter(input_setence == df_text & !(pred_word %in% final_words$pred_word)) %>%
      select(pred_word) %>% slice(1:(3-nrow(final_words)))
    
    if (nrow(predict_words) + nrow(final_words) < 3) {
      qty_words_input <- 1
      df_text <- word(df_text,start = -1,end = -1)
    }
    final_words <- rbind(final_words,predict_words)
  }
  
  #Bigram
  if (qty_words_input == 1) {
    predict_words <- bigram %>% 
      filter(input_setence == df_text & !(pred_word %in% final_words$pred_word)) %>%
      select(pred_word) %>% slice(1:(3-nrow(final_words)))
    final_words <- rbind(final_words,predict_words)
  }
  
  return(final_words)
}
