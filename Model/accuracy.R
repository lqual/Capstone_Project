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
for(i in id) {
        numwords <- min(wordcount(blogs[i]), 8)
        text <- word(blogs[i], 1, numwords)
        blogsdf$phrase[i] <- text
}

id <- c(1:length(news))
newsdf <- data.frame(num = id, phrase = "taco", stringsAsFactors = FALSE)
for(i in id) {
        numwords <- min(wordcount(news[i]), 8)
        text <- word(news[i], 1, numwords)
        newsdf$phrase[i] <- text
}

id <- c(1:length(twitter))
twitterdf <- data.frame(num = id, phrase = "taco", stringsAsFactors = FALSE)
for(i in id) {
        numwords <- min(wordcount(twitter[i]), 8)
        text <- word(twitter[i], 1, numwords)
        twitterdf$phrase[i] <- text
}

testdata <- rbind(blogsdf, newsdf, twitterdf)


#processing text
id <- c(1:length(testdata$phrase))
for(i in id) {
        text <- testdata$phrase[i]
        #text <- paste("Sandler's", text, sep = " ")
        text <- gsub('[0-9]+', '', text)
        text <- tolower(text)
        text <- trimws(text, which = "both")
        text <- removePunctuation(text)
        text <- removeNumbers(text)
        testdata$phrase[i] <- text
        
}

testdata <- testdata %>% mutate(words = 0)
id <- c(1:length(testdata$phrase))
for(i in id) {
        numwords <- wordcount(testdata$phrase[i])
        testdata$words[i] <- numwords
}

testdata <- testdata %>% filter(words > 1) %>% 
        mutate(next_word = word(phrase, -1)) %>%
        mutate(phrase = word(phrase, 1, words - 1)) 
datasave <- testdata

#add in predictions
source('~/Capstone_Project/Model/textPredictor.R')
testdata$pred1 <- "taco"
testdata$pred2 <- "taco"
testdata$pred3 <- "taco"

id <- c(1:length(testdata$phrase))
for(i in id) {
        answer <- getword(testdata$phrase[i])
        testdata$pred1[i] <- answer[1, 1]
        testdata$pred2[i] <- answer[2, 1]
        testdata$pred3[i] <- answer[3, 1]
}









#save data
#write.table(testdata, "testdata.csv", col.names=T, row.name=F, append=F, sep=",")





