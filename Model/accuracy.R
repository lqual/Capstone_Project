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

#load ngrams
octogram <- read.csv("Model/octogram_final.csv", stringsAsFactors = F)
heptagram <- read.csv("Model/heptagram_final.csv", stringsAsFactors = F)
hexagram <- read.csv("Model/hexagram_final.csv", stringsAsFactors = F)
pentagram <- read.csv("Model/pentagram_final.csv", stringsAsFactors = F)
quadragram <- read.csv("Model/quadragram_final.csv", stringsAsFactors = F)
trigram <- read.csv("Model/trigram_final.csv", stringsAsFactors = F)
bigram <- read.csv("Model/bigram_final.csv", stringsAsFactors = F)

id <- c(456:length(testdata$phrase))
for(i in id) {
        text <- getword(testdata$phrase[i])
        
        text <- paste("Sandler's", text, sep = " ")
        text <- gsub('[0-9]+', '', text)
        #if(wordcount(text) == 1) {
        text <- tolower(text)
        text <- trimws(text, which = "both")
        text <- removePunctuation(text)
        text <- removeNumbers(text)
        #} else {
        #length <- wordcount(text)
        #text <- VCorpus(VectorSource(text))
        #text <- tm_map(text, stripWhitespace)
        #text <- tm_map(text, content_transformer(tolower))
        #text <- tm_map(text, removePunctuation)
        #text <- tm_map(text, removeNumbers)
        #uno <- function(x) unlist(lapply(ngrams(words(x), length), paste, collapse = " "), 
        #                        use.names = FALSE)
        #text <- DocumentTermMatrix(text,control=list(tokenize=uno))
        #text <- findMostFreqTerms(text, n = 1)
        #text <- as.data.frame(text)
        #text <- rownames(text)
        #}
        
        
        
        
        
        #lookup in file
        answer8 <- data.frame(matrix(ncol = 1))
        colnames(answer8) <- "answer"
        answer7 <- data.frame(matrix(ncol = 1))
        colnames(answer7) <- "answer"
        answer6 <- data.frame(matrix(ncol = 1))
        colnames(answer6) <- "answer"
        answer5 <- data.frame(matrix(ncol = 1))
        colnames(answer5) <- "answer"
        answer4 <- data.frame(matrix(ncol = 1))
        colnames(answer4) <- "answer"
        answer3 <- data.frame(matrix(ncol = 1))
        colnames(answer3) <- "answer"
        
        numwords <- wordcount(text)
        if(numwords > 6) {
                text8 <- word(text, -7, numwords)
                #filtdf5 <- pentagram %>% filter(nchar == nchar(text5))
                lookup8 <- grep(text8, octogram$string)
                answer8 <- data.frame(octogram$next_word[lookup8])
                colnames(answer8) <- "answer"
        }
        
        if(numwords > 5) {
                text7 <- word(text, -6, numwords)
                #filtdf5 <- pentagram %>% filter(nchar == nchar(text5))
                lookup7 <- grep(text7, heptagram$string)
                answer7 <- data.frame(heptagram$next_word[lookup7])
                colnames(answer7) <- "answer"
        }
        
        if(numwords > 4) {
                text6 <- word(text, -5, numwords)
                #filtdf5 <- pentagram %>% filter(nchar == nchar(text5))
                lookup6 <- grep(text6, hexagram$string)
                answer6 <- data.frame(hexagram$next_word[lookup6])
                colnames(answer6) <- "answer"
        }
        
        if(numwords > 3) {
                text5 <- word(text, -4, numwords)
                #filtdf5 <- pentagram %>% filter(nchar == nchar(text5))
                lookup5 <- grep(text5, pentagram$string)
                answer5 <- data.frame(pentagram$next_word[lookup5])
                colnames(answer5) <- "answer"
        }
        
        if(numwords > 2) {
                text4 <- word(text, -3, numwords)
                #filtdf4 <- quadragram #%>% filter(nchar == nchar(text4))
                lookup4 <- grep(text4, quadragram$string)
                answer4 <- data.frame(quadragram$next_word[lookup4])
                colnames(answer4) <- "answer"
        }
        
        if(numwords > 1) {
                text3 <- word(text, -2, numwords)
                #filtdf3 <- trigram #%>% filter(nchar == nchar(text3))
                lookup3 <- grep(text3, trigram$string)
                answer3 <- data.frame(trigram$next_word[lookup3])
                colnames(answer3) <- "answer"
        }
        
        text2 <- word(text, -1, numwords)
        #filtdf2 <- bigram #%>% filter(nchar == nchar(text2))
        lookup2 <- grep(text2, bigram$string)
        answer2 <- data.frame(bigram$next_word[lookup2])
        colnames(answer2) <- "answer"
        
        myprobs <- c(10/26, 5/26, 2/26, 2/26, 2/26, 1/26, 1/26, 1/26, 1/26, 1/26)
        lookup1 <- sample(1:10, size = 3, replace = FALSE, prob = myprobs)
        unigram <- data.frame(next_word = c("the", "and", "for", "that", "you",
                                            "with", "Was", "this", "have","are"))
        answer1 <- data.frame(unigram$next_word[lookup1])
        colnames(answer1) <- "answer"
        
        answer0 <- data.frame(answer = c("taco", "taco"))
        
        #return an answer
        finalAnswer <- rbind(answer8, answer7, answer6, answer5, 
                             answer4, answer3, answer2, answer1, 
                             answer0)
        index <- which(duplicated(finalAnswer$answer))
        finalAnswer <- finalAnswer %>% mutate(column = 1)
        finalAnswer <- finalAnswer[-index, ]
        finalAnswer <- finalAnswer[!is.na(finalAnswer$answer), ]
        finalAnswer <- finalAnswer %>% select(answer)
        finalAnswer$answer <- as.character(finalAnswer$answer)
        answer <- head(finalAnswer, 3)
        
        testdata$pred1[i] <- answer[1, 1]
        testdata$pred2[i] <- answer[2, 1]
        testdata$pred3[i] <- answer[3, 1]
}

test <- testdata[1:2140, ]
test <- test






#save data
#write.table(testdata, "testdata.csv", col.names=T, row.name=F, append=F, sep=",")





