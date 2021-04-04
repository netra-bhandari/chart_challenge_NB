library(tidyverse)
library(wordcloud)
#install.packages("RColorBrewer")
library(RColorBrewer)
#install.packages("wordcloud2")
library(wordcloud2)
#install.packages("tm")
library(tm)
#https://www.francescocauteruccio.info
#https://medium.com/@finalfire/one-hundred-years-of-solitude-how-i-analyzed-my-favorite-book-6c20456480c8

mr <- read.csv("magical_realism.csv", encoding = "UTF-16", header = FALSE)


mr <- data.frame(mr)
# Create a corpus  
docs <- Corpus(VectorSource(mr))
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("spanish"))
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
df_max <- df %>% filter(freq >= 100) 
##remove common words 
df_rare <- df %>% filter(freq <= 100)

png("day4_wc_max.png",res = 300, width =5 , height = 5,units="in" )

set.seed(1234)
par(bg="#1a3657") 
wc_max <- wordcloud(words = df_max$word, freq = df_max$freq, min.freq = 1,           
                    max.words= 500, random.order=FALSE,rot.per=0.35,   
                    colors=brewer.pal(8,"Dark2"))

dev.off()

png("day4_wc_rare.png", width=12, height=8, units="in", res=300)

set.seed(1234)
par(bg="#1a3657") 
wc_rare <- wordcloud(words = df_rare$word, freq = df_rare$freq, min.freq = 1,           
                     max.words= 10000, random.order=FALSE, rot.per=0.35,         
                     colors=brewer.pal(2, "Dark2"))

dev.off()

