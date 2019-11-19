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
blogs2 <- read_lines(file("split_data/blogs_2.txt"), skip = 0)
news2 <- read_lines(file("split_data/news_2.txt"), skip = 0)
twitter2 <- read_lines(file("split_data/twitter_2.txt"), skip = 0)
blogs3 <- read_lines(file("split_data/blogs_3.txt"), skip = 0)
news3 <- read_lines(file("split_data/news_3.txt"), skip = 0)
twitter3 <- read_lines(file("split_data/twitter_3.txt"), skip = 0)
blogs4 <- read_lines(file("split_data/blogs_4.txt"), skip = 0)
news4 <- read_lines(file("split_data/news_4.txt"), skip = 0)
twitter4 <- read_lines(file("split_data/twitter_4.txt"), skip = 0)
string <- concatenate(blogs2, news2, twitter2, 
                      blogs3, news3, twitter3,
                      blogs4, news4, twitter4)
rm(blogs2, news2, twitter2, 
     blogs3, news3, twitter3,
     blogs4, news4, twitter4)


#prepare corpus
corpus <- VCorpus(VectorSource(string))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)


#build ngrams
dtm <- DocumentTermMatrix(corpus)
unigram <- findMostFreqTerms(dtm, n = 10L)

dos <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
dtm2 <- DocumentTermMatrix(corpus,control=list(tokenize=dos))
bilength <- round(length(as.matrix(dtm2))*.9)
bigram <- findMostFreqTerms(dtm2, n = bilength)
rm(dos, bilength)

tres <- function(x) unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
dtm3 <- DocumentTermMatrix(corpus,control=list(tokenize=tres))
trilength <- round(length(as.matrix(dtm3))*.9)
trigram <- findMostFreqTerms(dtm3, n = trilength)
rm(tres, trilength)

cuatro <- function(x) unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = FALSE)
dtm4 <- DocumentTermMatrix(corpus,control=list(tokenize=cuatro))
quadralength <- round(length(as.matrix(dtm4))*.9)
quadragram <- findMostFreqTerms(dtm4, n = quadralength)
rm(cuatro, quadralength)

cinco <- function(x) unlist(lapply(ngrams(words(x), 5), paste, collapse = " "), use.names = FALSE)
dtm5 <- DocumentTermMatrix(corpus,control=list(tokenize=cinco))
pentalength <- round(length(as.matrix(dtm5))*.9)
pentagram <- findMostFreqTerms(dtm5, n = pentalength)
rm(cinco, pentalength)

seis <- function(x) unlist(lapply(ngrams(words(x), 6), paste, collapse = " "), use.names = FALSE)
dtm6 <- DocumentTermMatrix(corpus,control=list(tokenize=seis))
hexalength <- round(length(as.matrix(dtm6))*.9)
hexagram <- findMostFreqTerms(dtm6, n = hexalength)
rm(seis, hexalength)

siete <- function(x) unlist(lapply(ngrams(words(x), 7), paste, collapse = " "), use.names = FALSE)
dtm7 <- DocumentTermMatrix(corpus,control=list(tokenize=siete))
heptalength <- round(length(as.matrix(dtm7))*.9)
heptagram <- findMostFreqTerms(dtm7, n = heptalength)
rm(siete, heptalength)

ocho <- function(x) unlist(lapply(ngrams(words(x), 8), paste, collapse = " "), use.names = FALSE)
dtm8 <- DocumentTermMatrix(corpus,control=list(tokenize=ocho))
octolength <- round(length(as.matrix(dtm8))*.9)
octogram <- findMostFreqTerms(dtm8, n = octolength)
rm(ocho, octolength)


############## run cleanNgrams here ########################
source('~/Capstone_Project/Model/cleanNgrams.R')
octogram <- cleanNgrams(octogram, 8)
write.table(octogram, "Model/octogram.csv", col.names=T, row.name=F, append=F, sep=",")

heptagram <- cleanNgrams(heptagram, 7)
write.table(heptagram, "Model/heptagram.csv", col.names=T, row.name=F, append=F, sep=",")
hexagram <- cleanNgrams(hexagram, 6)
write.table(hexagram, "Model/hexagram.csv", col.names=T, row.name=F, append=F, sep=",")

pentagram <- cleanNgrams(pentagram, 5)
write.table(pentagram, "Model/pentagram.csv", col.names=T, row.name=F, append=F, sep=",")
quadragram <- cleanNgrams(quadragram, 4)
write.table(quadragram, "Model/quadragram.csv", col.names=T, row.name=F, append=F, sep=",")
trigram <- cleanNgrams(trigram, 3, filt = 5)
write.table(trigram, "Model/trigram.csv", col.names=T, row.name=F, append=F, sep=",")
bigram <- cleanNgrams(bigram, 2, filt = 10)
write.table(bigram, "Model/bigram.csv", col.names=T, row.name=F, append=F, sep=",")

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







