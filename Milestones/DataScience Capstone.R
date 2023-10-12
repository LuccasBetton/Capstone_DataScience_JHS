## Libraries necessary for Script Running
#remotes::install_github("pdrhlik/sweary")
#install.packages("webshot")
#webshot::install_phantomjs()
packages <- c("stringr","ggplot2", "dplyr", "tibble", "qdap", "tidytext", "wordcloud2","forcats","tidyr","htmlwidgets", "sweary","webshot")
install.packages(setdiff(packages, rownames(installed.packages()))) 
lapply(packages, require, character.only = TRUE)

rm(packages)

## Raw data Download

filepath_news <- "./final/en_US/en_US.news.txt"
filepath_blogs <- "./final/en_US/en_US.blogs.txt"
filepath_twitter <- "./final/en_US/en_US.twitter.txt"

if(!(file.exists(filepath_news)*file.exists(filepath_blogs)*file.exists(filepath_twitter))){
    if(!file.exists("./Coursera-SwiftKey.zip")){
        fileURL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
        download.file(fileURL, destfile = "Coursera-SwiftKey.zip", method = "curl")
    }
    unzip("./Coursera-SwiftKey.zip")
}

# Read Data
Connec <- file("./final/en_US/en_US.news.txt","rb")
News_RD <- readLines(Connec, skipNul = TRUE) 
close(Connec)

Connec <- file("./final/en_US/en_US.twitter.txt","rb")
Twitter_RD <- readLines(Connec, skipNul = TRUE)
close(Connec)

Connec <- file("./final/en_US/en_US.blogs.txt","r")
Blog_RD <- readLines(Connec,skipNul = TRUE)
close(Connec)

rm(Connec,filepath_news,filepath_blogs,filepath_twitter)

#Summary table for each file - Blog, News and Twitter

Source_Data <- c("Blogs","News","Twitter") #Source of data 
Sizes_Data <- c(length(Blog_RD),length(News_RD),length(Twitter_RD )) #Lines per data source 
Word_Data <- c(sum(str_count(Blog_RD,'\\W+')),sum(str_count(News_RD,'\\W+')),sum(str_count(Twitter_RD ,'\\W+'))) #Qty of words per source of data 
Char_Data <- c(sum(nchar(Blog_RD)),sum(nchar(News_RD)),sum(nchar(Twitter_RD ))) #Qty  of characters per data source

Summary_Table <- data.frame(Source_Data,Sizes_Data,Word_Data,Char_Data,round(Word_Data/Sizes_Data),round(Char_Data/Sizes_Data))
colnames(Summary_Table) <- c("Data Source", "Lines","Words",'Characters',"Avg Word per line","Avg Char per line")
Summary_Table 

rm(Summary_Table,Char_Data,Sizes_Data, Source_Data,Word_Data)

## Exploratory Analysis

#Summary Table shows a lower Avg word and Avg character per line as it is expected due platform restrictions for text size in Twitter

### Sample Data

#Sample Data
set.seed(1990)
factor = c(0.1,0.1,0.025)
News_RD_Spl <- News_RD[sample(1:length(News_RD),factor[1]*length(News_RD))] #Sample Data from News Data
Blog_RD_Spl <- Blog_RD[sample(1:length(Blog_RD),factor[2]*length(Blog_RD))] #Sample Data from Blog Data
Twitter_RD_Spl <- Twitter_RD[sample(1:length(Twitter_RD),factor[3]*length(Twitter_RD))] #Sample Data from Twitter Data

#Frequency data_Frame words per line
FreqWord_News_Spl <- data.frame(str_count(News_RD_Spl,'\\w+'),factor("News"))
colnames(FreqWord_News_Spl) <- c("Words","Data_Source")

FreqWord_Blog_Spl <- data.frame(str_count(Blog_RD_Spl,'\\w+'),factor("Blog"))
colnames(FreqWord_Blog_Spl) <- c("Words","Data_Source")

FreqWord_Twitter_Spl <- data.frame(str_count(Twitter_RD_Spl,'\\w+'),factor("Twitter"))
colnames(FreqWord_Twitter_Spl) <- c("Words","Data_Source")

FreqWord_Spl <- bind_rows(FreqWord_News_Spl, FreqWord_Blog_Spl ,FreqWord_Twitter_Spl)
save(FreqWord_Spl,file="FreqWord_Spl.Rda")

ggplot(FreqWord_Spl,aes(x=Words, color=Data_Source))+geom_histogram(binwidth = 5, fill = "white", alpha = 0.5, position = 'identity')+xlim(0,150)+facet_wrap(.~Data_Source)+xlab("Words per Text")+ylab("")

# Remove data 
rm(Blog_RD,Twitter_RD,News_RD)
rm(FreqWord_Blog_Spl,FreqWord_News_Spl,FreqWord_Twitter_Spl,FreqWord_Spl)

#Combine data frames
RD_Spl <- c(News_RD_Spl,Blog_RD_Spl,Twitter_RD_Spl) #Raw Data Sample

#Create txt for Sample Date
write.table(RD_Spl, file =  "./RawData_Sample.txt", quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)

#Read Sample Date
#Connec <- file("./RawData_Sample.txt","rb")
#RD_Spl <- readLines(Connec, skipNul = TRUE)
#close(Connec)
#rm(Connec)

## Cleaning Data

TD_Spl <- RD_Spl
TD_Spl <- tolower(TD_Spl) #Transform all characters in lowercase reduced
TD_Spl <- rm_url(TD_Spl) #Remove URL strings
TD_Spl <- rm_non_ascii(TD_Spl) #Remove non ASCII words
TD_Spl <- gsub(".*?($|'|[^[:punct:]]).*?","\\1", TD_Spl) #Remove punctations expect for apostrophe
TD_Spl <- TD_Spl[which(str_count(TD_Spl,'\\W+')>=4)] #Filter out texts with less than 4 words 

write.table(TD_Spl, file =  "./TidyData_Sample.txt", quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)

TD_Spl_DF <- tibble(Text = TD_Spl)  #Tidy Date Format Data Frame
save(TD_Spl_DF,file="TD_Spl_DF.Rda")
rm(RD_Spl,TD_Spl)
#load(file='TD_Spl_DF.rda')

#Data base for stop words and Swear Words
data("stop_words")
data("swear_words")
swear_words = filter(swear_words,language == "en") #Filter only for English swear words

# Unigrams

TD_1gram <- unnest_tokens(TD_Spl_DF, Gram, input = Text, token = "ngrams", n = 1)
TD_1gram <- filter(TD_1gram,!str_detect(Gram, "\\d+"))
TD_1gram <- anti_join(TD_1gram,swear_words, join_by(Gram == word))

TD_1gram_Full <- TD_1gram
save(TD_1gram_Full,file="TD_1gram_Full.Rda")
#load(file="TD_1gram_Full.Rda")

WordCnt_1gram_Full <- TD_1gram_Full %>%
  group_by_all() %>%
  count()
WordCnt_1gram_Full <- WordCnt_1gram_Full %>% arrange(desc(n))
WordCnt_1gram_Full$freq <- WordCnt_1gram_Full$n*100/sum(WordCnt_1gram_Full$n)
WordCnt_1gram_Full$cumfreq <- cumsum(WordCnt_1gram_Full$freq)
save(WordCnt_1gram_Full,file="WordCnt_1gram_Full.Rda")
#load(file="WordCnt_1gram_Full.Rda")
Words_1app <- c(nrow(filter(WordCnt_1gram_Full,n>1))/nrow(WordCnt_1gram_Full)*100)
Words_50freq <- c(nrow(filter(WordCnt_1gram_Full,cumfreq<=50))/nrow(WordCnt_1gram_Full)*100)

TD_1gram_WoutSw <- anti_join(TD_1gram,stop_words, join_by(Gram == word))
save(TD_1gram_WoutSw,file="TD_1gram_WoutSw.Rda")
#load(file="TD_1gram_WoutSw.Rda")

WordCnt_1gram_WoutSw <- TD_1gram_WoutSw %>%
  group_by_all() %>%
  count()
WordCnt_1gram_WoutSw <- WordCnt_1gram_WoutSw %>% arrange(desc(n))
WordCnt_1gram_WoutSw$freq <- with(WordCnt_1gram_WoutSw,WordCnt_1gram_WoutSw$n*100/sum(WordCnt_1gram_WoutSw$n))
save(WordCnt_1gram_WoutSw,file="WordCnt_1gram_WoutSw.Rda")

# Unigram Exploration Analysis
max_plot <- 50
ggplot(WordCnt_1gram_Full[1:max_plot,],aes(x = freq, y = fct_reorder(Gram,freq)))+geom_col()+xlab("Frequency")+ylab("")+ggtitle("Unigram - Frequency - With Stop Words")
ggsave("Colunms_1gram_Full.png",plot = last_plot())

max_cloud <- 50
wd_cloud1gram <- wordcloud2(WordCnt_1gram_Full[1:max_cloud,], color = 'random-dark', size = 1)
wd_cloud1gram
saveWidget(wd_cloud1gram,"wordcloud_1gram_Full.html",selfcontained = F)
webshot("wordcloud_1gram_Full.html","wordcloud_1gram_Full.png", delay =5, vwidth = 500, vheight=500) # changed to png 

max_plot <- 50
ggplot(WordCnt_1gram_WoutSw[1:max_plot,],aes(x = freq, y = fct_reorder(Gram,freq)))+geom_col()+xlab("Frequency")+ylab("Word")+ggtitle("Unigram - Frequency")
ggsave("Colunms_1gram_WoutStopWord.png",plot = last_plot())

max_cloud <- 100
wd_cloud1gram_WoutSw <- wordcloud2(WordCnt_1gram_WoutSw[1:max_cloud,], color = 'random-dark', size = 1)
wd_cloud1gram_WoutSw
saveWidget(wd_cloud1gram_WoutSw,"wordcloud_1gram_WoutSw.html",selfcontained = F)
webshot("wordcloud_1gram_WoutSw.html","wordcloud_1gram_WoutSw.png", delay =5, vwidth = 1000, vheight=1000) # changed to png 

# Bigrams

TD_2gram <- unnest_tokens(TD_Spl_DF, Gram, input = Text, token = "ngrams", n = 2)
TD_2gram <- filter(TD_2gram,!str_detect(Gram, "\\d+")) #Filter out numbers 
TD_2gram <- separate(TD_2gram,Gram,c("Gram1", "Gram2"),sep = " ") # Split in two Grams
TD_2gram <- TD_2gram %>% filter(!(Gram1 %in% swear_words$word | Gram2 %in% swear_words$word)) #Filter out bad words
save(TD_2gram,file="TD_2gram.Rda")

WordCnt_2gram <- TD_2gram %>%
  group_by_all() %>%
  count()
WordCnt_2gram <- WordCnt_2gram %>% arrange(desc(n))
WordCnt_2gram$freq <- WordCnt_2gram$n*100/sum(WordCnt_2gram$n)
WordCnt_2gram$cumfreq <- cumsum(WordCnt_2gram$freq)
save(WordCnt_2gram,file="WordCnt_2gram.Rda")
#load(file='WordCnt_2gram.Rda')
Words_1app <- c(Words_1app,nrow(filter(WordCnt_2gram,n>1))/nrow(WordCnt_2gram)*100) #Observations with more than one appearence
Words_50freq <- c(Words_50freq,nrow(filter(WordCnt_2gram,cumfreq<=50))/nrow(WordCnt_2gram)*100)

#TD 2gram filtered if both words are stop words
#TD_2gram_Sw <- TD_2gram %>% filter(!(Gram1 %in% stop_words$word & Gram2 %in% stop_words$word)) #Filter out when the two words are Stop Words
#save(TD_2gram_Sw,file="TD_2gram_Sw.Rda")
#load(file='TD_2gram_Sw.Rda')

#WordCnt_2gram_Sw <- TD_2gram_Sw %>%
#  group_by_all() %>%
#  count()
#WordCnt_2gram_Sw <- WordCnt_2gram_Sw %>% arrange(desc(n))
#WordCnt_2gram_Sw$freq <- WordCnt_2gram_Sw$n*100/sum(WordCnt_2gram_Sw$n)
#save(WordCnt_2gram_Sw,file="WordCnt_2gram_Sw.Rda")
#load(file='WordCnt_2gram_WoutSw.Rda')

#TD_2gram Without any stop words
TD_2gram_WoutSw <- TD_2gram %>% 
  filter(!Gram1 %in% stop_words$word) %>%
  filter(!Gram2 %in% stop_words$word)
save(TD_2gram_WoutSw,file="TD_2gram_WoutSw.Rda")
#load(file='TD_2gram_Spl_FullFilt.Rda')
WordCnt_2gram_WoutSw <- TD_2gram_WoutSw %>%
  group_by_all() %>%
  count()
WordCnt_2gram_WoutSw <- WordCnt_2gram_WoutSw %>% arrange(desc(n))
WordCnt_2gram_WoutSw$freq <- with(WordCnt_2gram_WoutSw,WordCnt_2gram_WoutSw$n*100/sum(WordCnt_2gram_WoutSw$n))
WordCnt_2gram_WoutSw$cumfreq <- cumsum(WordCnt_2gram_WoutSw$freq)
save(WordCnt_2gram_WoutSw,file="WordCnt_2gram_WoutSw.Rda")
#load(file='WordCnt_2gram_WoutSw.Rda')

# Exploratory Analysis Bigram with stop words
max_plot <- 50
plot_df <- WordCnt_2gram[50:100,]
plot_df <- plot_df %>% unite(Gram, c("Gram1", "Gram2"),sep = " ")
ggplot(mutate(plot_df,Gram = reorder(Gram,n)),aes(x = freq, y = Gram))+geom_col()+xlab("Frequency")+ylab("Word")+ggtitle("Bigram - Frequency")
ggsave("Columns_Bigram.png",plot = last_plot())

max_plot <- 50
plot_df <- WordCnt_2gram_WoutSw[1:max_plot,]
plot_df <- plot_df %>% unite(Gram, c("Gram1", "Gram2"),sep = " ")
ggplot(mutate(plot_df,Gram = reorder(Gram,n)),aes(x = freq, y = Gram))+geom_col()+xlab("Frequency")+ylab("Word")+ggtitle("Bigram - Frequency - Full Filtered")

max_cloud <- 50
plot_cloud_df <- WordCnt_2gram[1:max_cloud,]
plot_cloud_df <- plot_cloud_df %>% unite(Gram, c("Gram1", "Gram2"),sep = " ")
wd_cloud2gram <- wordcloud2(plot_cloud_df, color = 'random-dark', size = 1)
#wd_cloud2gram
saveWidget(wd_cloud2gram,"wordcloud_2gram.html",selfcontained = F)
webshot("wordcloud_2gram.html","wordcloud_2gram.png", delay =5, vwidth = 480, vheight=480)

max_cloud <- 50
plot_cloud_df <- WordCnt_2gram_WoutSw[1:max_cloud,]
plot_cloud_df <- plot_cloud_df %>% unite(Gram, c("Gram1", "Gram2"),sep = " ")
wd_cloud2gram <- wordcloud2(plot_cloud_df, color = 'random-dark', size = 0.75)
wd_cloud2gram
saveWidget(wd_cloud2gram,"wordcloud_2gram.html",selfcontained = F)
webshot("wordcloud_2gram.html","wordcloud_2gram.png", delay =5, vwidth = 480, vheight=480)

# Trigram

## Trigrams Processing

TD_3gram <- unnest_tokens(TD_Spl_DF, Gram, input = Text, token = "ngrams", n = 3)
TD_3gram <- filter(TD_3gram,!str_detect(Gram, "\\d+"))
TD_3gram <- TD_3gram %>% separate(Gram,c("Gram1", "Gram2","Gram3"),sep = " ")
TD_3gram <- TD_3gram %>% filter(!(Gram1 %in% swear_words$word | Gram2 %in% swear_words$word | Gram3 %in% swear_words$word)) #Filtered when all words are stopwords
TD_3gram <- TD_3gram %>% filter(!(Gram1 %in% stop_words$word & Gram2 %in% stop_words$word & Gram3 %in% stop_words$word))  #Filter bad words
save(TD_3gram,file="TD_3gram.Rda")

WordCnt_3gram <- TD_3gram %>%
  group_by_all() %>%
  count()
WordCnt_3gram <- WordCnt_3gram %>% arrange(desc(n))
WordCnt_3gram$freq <- WordCnt_3gram$n*100/sum(WordCnt_3gram$n)
WordCnt_3gram$cumfreq <- cumsum(WordCnt_3gram$freq)
save(WordCnt_3gram,file="WordCnt_3gram.Rda")
#load(file='WordCnt_3gram.Rda')
Words_1app <- c(Words_1app,nrow(filter(WordCnt_3gram,n>1))/nrow(WordCnt_3gram)*100) #Observations with more than one appearence
Words_50freq <- c(Words_50freq,nrow(filter(WordCnt_3gram,cumfreq<=50))/nrow(WordCnt_3gram)*100)

max_plot <- 50
plot_df <- WordCnt_3gram[1:max_plot,]
plot_df <- plot_df %>% unite(Gram, c("Gram1", "Gram2","Gram3"),sep = " ")
ggplot(mutate(plot_df,Gram = reorder(Gram,n)),aes(x = freq, y = Gram))+geom_col()+xlab("Frequency")+ylab("Word")+ggtitle("Trigram - Frequency")
ggsave("Columns_Trigram.png",plot = last_plot())

max_cloud <- 100
plot_cloud_df <- WordCnt_3gram[1:max_cloud,]
plot_cloud_df <- plot_cloud_df %>% unite(Gram, c("Gram1", "Gram2","Gram3"),sep = " ")
wd_cloud3gram <- wordcloud2(plot_cloud_df, color = 'random-dark', size = 0.5)
#wd_cloud3gram
saveWidget(wd_cloud3gram,"wordcloud_3gram.html",selfcontained = F)
webshot("wordcloud_3gram.html","wordcloud_3gram.png", delay =5, vwidth = 480, vheight=480)

#Quadrigram
TD_4gram <- unnest_tokens(TD_Spl_DF, Gram, input = Text, token = "ngrams", n = 4)
TD_4gram <- filter(TD_4gram,!str_detect(Gram, "\\d+"))
TD_4gram <- TD_4gram %>% separate(Gram,c("Gram1", "Gram2","Gram3","Gram4"),sep = " ")
TD_4gram <- TD_4gram %>% filter(!(Gram1 %in% swear_words$word | Gram2 %in% swear_words$word | Gram3 %in% swear_words$word | Gram4 %in% swear_words$word)) #Filtered when any word are badword
TD_4gram <- TD_4gram %>% filter(!(Gram1 %in% stop_words$word & Gram2 %in% stop_words$word & Gram3 %in% stop_words$word & Gram4 %in% stop_words$word))  #Filter if all words are stop words
save(TD_4gram,file="TD_4gram.Rda")
#load(file='TD_4gram.Rda')

WordCnt_4gram <- TD_4gram %>%
  group_by_all() %>%
  count()
WordCnt_4gram <- WordCnt_4gram %>% arrange(desc(n))
WordCnt_4gram$freq <- with(WordCnt_4gram,WordCnt_4gram$n*100/sum(WordCnt_4gram$n))
WordCnt_4gram$cumfreq <- cumsum(WordCnt_4gram$freq)
save(WordCnt_4gram,file="WordCnt_4gram.Rda")
#load(file='WordCnt_4gram.Rda')
Words_1app <- c(Words_1app,nrow(filter(WordCnt_4gram,n>1))/nrow(WordCnt_4gram)*100) #Observations with more than one appearence
Words_50freq <- c(Words_50freq,nrow(filter(WordCnt_4gram,cumfreq<=50))/nrow(WordCnt_4gram)*100)

max_plot <- 50
plot_df <- WordCnt_4gram[1:max_plot,]
plot_df <- plot_df %>% unite(Gram, c("Gram1", "Gram2","Gram3","Gram4"),sep = " ")
ggplot(mutate(plot_df,Gram = reorder(Gram,n)),aes(x = freq, y = Gram))+geom_col()+xlab("Frequency")+ylab("Word")+ggtitle("Quadrigram - Frequency")
ggsave("Columns_Quadrigram.png",plot = last_plot())

max_cloud <- 50
plot_cloud_df <- WordCnt_4gram[1:max_cloud,]
plot_cloud_df <- plot_cloud_df %>% unite(Gram, c("Gram1", "Gram2","Gram3","Gram4"),sep = " ")
wd_cloud4gram <- wordcloud2(plot_cloud_df, color = 'random-dark', size = 0.5)
wd_cloud4gram
saveWidget(wd_cloud4gram,"wordcloud_4gram.html",selfcontained = F)
webshot("wordcloud_4gram.html","wordcloud_4gram.png", delay =5, vwidth = 480, vheight=480)

#Summary of Grams
NGram_length <- c(length(WordCnt_1gram_Full$n),length(WordCnt_2gram$n),length(WordCnt_3gram$n),length(WordCnt_4gram$n))
Gram_List <- c("Unigram","Bigram","Trigram","Quadrigram") #Source of data 
SumGram_Table <- data.frame(Gram_List,NGram_length,round(Words_1app,1),round(Words_50freq,1))
colnames(SumGram_Table) <- c("N-Gram", "Total Qty Grams",  "% Gram More One app","% of Grams cover 50%") 

#Predictions DataFrames
Unigram <- WordCnt_1gram_Full
Bigram <- WordCnt_2gram
Trigram <- WordCnt_3gram
Quadrigram <- WordCnt_4gram

save(Unigram,file="Unigram.Rda")
save(Bigram,file="Bigram.Rda")
save(Trigram,file="Trigram.Rda")
save(Quadrigram,file="Quadrigram.Rda")