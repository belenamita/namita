#For Text Mining assignment

#ONE:
#  1) Extract tweets for any user (try choosing a user who has more tweets)
#  2) Perform sentimental analysis on the tweets extracted from the above

library(tm)
library(wordcloud)
library(wordcloud2)

tweet<-read.csv("C://Data_science--data//Raj_Class//Text mining//Vaishnavi Files//Tweets.csv")

View(tweet)
class(tweet)
dim(tweet)
text<-as.character(tweet$text) # use just the text column
View(text)
length(text)
text<-Corpus(VectorSource(text))
class(text) # simple corpus, corpus class
inspect(text[1]) # View the first tweet 
inspect(text[10]) # view 10th tweet

# data cleaning
text1<-tm_map(text,tolower) # have to remove punctuation, numbers etc separately
text1<-tm_map(text1,removePunctuation)
text1<-tm_map(text1,removeNumbers)
text1<-tm_map(text1,removeWords, stopwords('english'))
removeURL <- function(z) gsub('http[[:alnum:]]*', '', z)
text1 <- tm_map(text1, content_transformer(removeURL))
text1<-tm_map(text1, stripWhitespace)
inspect(text1[1]) 
inspect(text1[10])

# TDM/DTM
tdm<-TermDocumentMatrix(text1)
tdm

dtm<-t(tdm)
dtm

tdm<-as.matrix(tdm)
dim(tdm) # 4818, 2131
View(tdm[1:20, 1:10]) # selecting to view few rows and columns

#plots
count<-rowSums(tdm) # text plot of frequency of words repeated in each row
count
count_high<-subset(count, count>=25) # consider those words which are repeated >=25 times
count_high
barplot(count_high, las=2, col = rainbow(150))

count_max<-subset(count,count>=80) # max repeated words
count_max
barplot(count_max, las=2, col = rainbow(150))

count_small <- subset(count, count >= 10)
count_small
barplot(count_small, las=2, col = rainbow(30))

# wordcloud
count_all <- sort(rowSums(tdm), decreasing = TRUE)
count_all

wordcloud(words = names(count_high), freq = count_high)
wordcloud(words = names(count_all), freq = count_all)
wordcloud(words = names(count_high), freq = count_high, random.order = F, colors = rainbow(50))

# create df, using wordcloud2 library
df <- data.frame(names(count_small), count_small) # dataframe of word repeated >=10 times
colnames(df) <- c('word', 'freq')
View(df)

write.csv(df, "Tweet assignment.csv")
getwd() # get the saved path of the above csv file


# sentiment mining

install.packages("syuzhet")
library("syuzhet")
library(lubridate)
library(ggplot2)
library(scales)
library(dbplyr)
library(reshape2)


emotion = readLines("C://Data_science--data//Raj_Class//Text mining//Vaishnavi Files//Tweets.csv")

x <- iconv(emotion, "UTF-8")

sentiment <- get_nrc_sentiment(x)
head(sentiment)

x[5] # for 5th tweet check the below mentions sentiment count
get_nrc_sentiment('unfriendly')
get_nrc_sentiment('happy')
barplot(colSums(sentiment), las = 2, col = rainbow(10), ylab = 'Count', main = 'Emotion scores')

