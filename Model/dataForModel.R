#libraries
library(readr)
library(ngram)
library(textreg)
library(tm)
library(caret)
library(SnowballC)
library(dplyr)
library(stringr)


#read in data for corpus and combine
blogs1 <- read_lines(file("split_data/blogs_1.txt"), skip = 0)
news1 <- read_lines(file("split_data/news_1.txt"), skip = 0)
twitter1 <- read_lines(file("split_data/news_1.txt"), skip = 0)
string <- concatenate(blogs1, news1, twitter1)


#prepare corpus
corpus <- VCorpus(VectorSource(string))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)


#build ngrams
dtm <- DocumentTermMatrix(corpus)
unigram <- findMostFreqTerms(dtm, n = 2L)

dos <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
dtm2 <- DocumentTermMatrix(corpus,control=list(tokenize=dos))
bilength <- round(length(as.matrix(dtm2))*.75)
bigram <- findMostFreqTerms(dtm2, n = bilength)

tres <- function(x) unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
dtm3 <- DocumentTermMatrix(corpus,control=list(tokenize=tres))
trilength <- round(length(as.matrix(dtm3))*.75)
trigram <- findMostFreqTerms(dtm3, n = trilength)

cuatro <- function(x) unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = FALSE)
dtm4 <- DocumentTermMatrix(corpus,control=list(tokenize=cuatro))
quadralength <- round(length(as.matrix(dtm4))*.75)
quadragram <- findMostFreqTerms(dtm4, n = quadralength)

cinco <- function(x) unlist(lapply(ngrams(words(x), 5), paste, collapse = " "), use.names = FALSE)
dtm5 <- DocumentTermMatrix(corpus,control=list(tokenize=cinco))
pentalength <- round(length(as.matrix(dtm5))*.75)
pentagram <- findMostFreqTerms(dtm5, n = pentalength)

############## run cleanNgrams here ########################
pentagram <- cleanNgrams(pentagram, 5)
quadragram <- cleanNgrams(quadragram, 4)
trigram <- cleanNgrams(trigram, 3)
bigram <- cleanNgrams(bigram, 2)


#old code
#pentagram <- as.data.frame(pentagram, row.names = NULL)
#head(rownames(pentagram))
#pentagram <- data.frame(string = rownames(pentagram), num = pentagram$X1)
#pentagram$string <- as.character(pentagram$string)
#pentagram1 <- pentagram %>% filter(num > 5) %>% 
#        mutate(next_word = word(string, -1), nchar = 0) %>%
#        mutate(string = word(string, 1, 4)) 
#index <- which(duplicated(pentagram1$string))
#pentagram1 <- pentagram1[-index, ]
#sum(pentagram1$num)
#id <- c(1:length(pentagram1$string))
#for(i in id) {
#        char <- nchar(pentagram1$string[i])
#        pentagram1$nchar[i] <- char
#}

head(pentagram1)
tail(pentagram1)





