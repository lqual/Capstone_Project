#libraries
library(readr)
library(ngram)
library(textreg)
library(tm)
library(caret)
library(SnowballC)
library(dplyr)
library(stringr)


#read in data
blogs <- read_lines(file("split_data/blogs_20.txt"), skip = 0)
news <- read_lines(file("split_data/news_20.txt"), skip = 0)
twitter <- read_lines(file("split_data/twitter_20.txt"), skip = 0)


#get dataframe of phrases
id <- c(1:length(blogs))
blogsdf <- data.frame(num = id, phrase = "taco", stringsAsFactors = FALSE)
set.seed(92)
for(i in id) {
        numwords <- c(1:wordcount(blogs[i]))
        rwords <- sample(numwords, 1)
        text <- word(blogs[i], 1, rwords)
        text <- as.character(word(text, -9, rwords))
        blogsdf$phrase[i] <- text
}

id <- c(1:length(news))
newsdf <- data.frame(num = id, phrase = "taco", stringsAsFactors = FALSE)
set.seed(192)
for(i in id) {
        numwords <- c(1:wordcount(news[i]))
        rwords <- sample(numwords, 1)
        text <- word(news[i], 1, rwords)
        text <- as.character(word(text, -9, rwords))
        newsdf$phrase[i] <- text
}

id <- c(1:length(twitter))
twitterdf <- data.frame(num = id, phrase = "taco", stringsAsFactors = FALSE)
set.seed(1192)
for(i in id) {
        numwords <- c(1:wordcount(twitter[i]))
        rwords <- sample(numwords, 1)
        text <- word(twitter[i], 1, rwords)
        text <- as.character(word(text, -9, rwords))
        twitterdf$phrase[i] <- text
}

testdata <- rbind(blogsdf, newsdf, twitterdf)


#processing text
id <- c(1:length(testdata))
id <- c(1:5)
for(i in id) {
        text <- testdata$phrase[i]
        length <- wordcount(text)
        text <- VCorpus(VectorSource(text))
        text <- tm_map(text, stripWhitespace)
        text <- tm_map(text, content_transformer(tolower))
        text <- tm_map(text, removePunctuation)
        text <- tm_map(text, removeNumbers)
        uno <- function(x) unlist(lapply(ngrams(words(x), length), paste, collapse = " "), 
                                  use.names = FALSE)
        text <- DocumentTermMatrix(text,control=list(tokenize=uno))
        text <- findMostFreqTerms(text, n = 1)
        text <- as.data.frame(text)
        text <- rownames(text)
        testdata$phrase[i] <- text
        
}


#save data
write.table(testdata, "testdata.csv", col.names=T, row.name=F, append=F, sep=",")





