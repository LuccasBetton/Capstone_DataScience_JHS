packages <- c("sqldf", "quanteda", "tm", "data.table","stringr","dplyr", "tibble", "qdap", "tidytext", "wordcloud2","forcats","tidyr","htmlwidgets", "sweary","webshot")
install.packages(setdiff(packages, rownames(installed.packages()))) 
lapply(packages, require, character.only = TRUE)

rm(packages)

# Build N-grams

## Load data

Connec <- file("./final/en_US/en_US.news.txt","rb")
News_RD <- readLines(Connec, skipNul = TRUE) 
close(Connec)

Connec <- file("./final/en_US/en_US.twitter.txt","rb")
Twitter_RD <- readLines(Connec, skipNul = TRUE)
close(Connec)

Connec <- file("./final/en_US/en_US.blogs.txt","r")
Blog_RD <- readLines(Connec,skipNul = TRUE)
close(Connec)

rm(Connec)

RawData <- c(News_RD,Blog_RD,Twitter_RD) #Raw Data from all data source

rm(News_RD,Blog_RD,Twitter_RD)

# Sample Data for train and test

set.seed(2023)

factor_sample = 0.45
RawData_Sample <- RawData[sample(1:length(RawData),factor_sample*length(RawData))]

RawData_Train <- RawData_Sample[1:(length(RawData_Sample)*0.67)]
RawData_Test <- RawData_Sample[(length(RawData_Sample)*0.67):length(RawData_Sample)]

rm(RawData)
rm(RawData_Sample, factor_sample)

# Clean Data

TidyData_Train <- RawData_Train
TidyData_Train <- tolower(TidyData_Train) #Transform all characters in lowercase reduced
TidyData_Train <- rm_url(TidyData_Train) #Remove URL strings
TidyData_Train <- rm_non_ascii(TidyData_Train) #Remove non ASCII words
TidyData_Train <- gsub(".*?($|'|[^[:punct:]]).*?","\\1", TidyData_Train) #Remove punctations expect for apostrophe
TidyData_Train <- gsub("[[:digit:]]+", "", TidyData_Train) #Remove digits

data("swear_words")
swear_words = filter(swear_words,language == "en") #Filter only for English swear words
TidyData_Train <- removeWords(TidyData_Train,words = swear_words$word)

TidyData_Train <- str_squish(TidyData_Train)

TidyData_Train <- gsub(" ' ", " ", TidyData_Train)

#TD_Spl <- TD_Spl[which(str_count(TD_Spl,'\\W+')>=4)] #Filter out texts with less than 4 words 
#TD_Spl_DF <- tibble(Text = TidyData_Train)  #Tidy Date Format Data Frame

my_corpus <- corpus(TidyData_Train)  # build a new corpus from the texts
my_tokens <- tokens(my_corpus)

penta_tokens <- tokens_ngrams(my_tokens, n = 5, concatenator = " ")
penta_dfm <- dfm(penta_tokens)
pentagram <- colSums(penta_dfm)
pentagram <- tibble(Gram = names(pentagram), n = pentagram)
pentagram <- pentagram %>% arrange(desc(n))
save(pentagram,file="./pentagram_full.Rda")
pentagram <- pentagram %>% filter(n > 1)
input_setence <- word(pentagram$Gram,end = -2)
pred_word <- word(pentagram$Gram,-1)
pentagram <- tibble(input_setence = input_setence, pred_word = pred_word, n = pentagram$n)
save(pentagram,file="./pentagram.Rda")

rm(penta_dfm,penta_tokens)

quadri_tokens <- tokens_ngrams(my_tokens, n = 4, concatenator = " ")
quadri_dfm <- dfm(quadri_tokens)
quadrigram <- colSums(quadri_dfm)
quadrigram <- tibble(Gram = names(quadrigram), n = quadrigram)
quadrigram <- quadrigram %>% arrange(desc(n))
save(quadrigram,file="./quadrigram_full.Rda")
quadrigram <- quadrigram %>% filter(n > 1)
input_setence <- word(quadrigram$Gram,end = -2)
pred_word <- word(quadrigram$Gram,-1)
quadrigram <- tibble(input_setence = input_setence, pred_word = pred_word, n = quadrigram$n)
save(quadrigram,file="./quadrigram.Rda")
rm(quadri_dfm,quadri_tokens)

tri_tokens <- tokens_ngrams(my_tokens, n = 3, concatenator = " ")
tri_dfm <- dfm(tri_tokens)
trigram_full <- colSums(tri_dfm)
trigram_full <- tibble(Gram = names(trigram_full), n = trigram_full)
trigram_full <- trigram_full %>% arrange(desc(n))
#save(trigram_full,file="./prediction-word/Trigram_full.Rda")
trigram <- trigram_full %>% filter(n > 1)
input_setence <- word(trigram$Gram,end = -2)
pred_word <- word(trigram$Gram,-1)
trigram <- tibble(input_setence = input_setence, pred_word = pred_word, n = trigram$n)
save(trigram,file="./trigram.Rda")
rm(tri_dfm,tri_tokens,trigram_full)

bi_tokens <- tokens_ngrams(my_tokens, n = 2, concatenator = " ")
bi_dfm <- dfm(bi_tokens)
bigram_full <- colSums(bi_dfm)
bigram_full <- tibble(Gram = names(bigram_full), n = bigram_full)
bigram_full <- bigram_full %>% arrange(desc(n))
#save(bigram_full,file="./prediction-word/bigram_full.Rda")
bigram <- bigram_full %>% filter(n > 1)
input_setence <- word(bigram$Gram,end = -2)
pred_word <- word(bigram$Gram,-1)
bigram <- tibble(input_setence = input_setence, pred_word = pred_word, n = bigram$n)
save(bigram,file="./bigram.Rda")

rm(input_setence,my_corpus,pred_word,my_tokens,bigram_full,bi_dfm,bi_tokens)

rm(RawData_Train,TidyData_Train)