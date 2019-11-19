#libraries
library(readr)
library(ngram)
library(textreg)
library(tm)
library(caret)
library(SnowballC)
library(dplyr)
library(stringr)


#Partitions for each set of Ngrams
set.seed(18)
set1 <- sample(c(1:15), 15, replace = FALSE)
set2 <- sample(c(1:15), 15, replace = FALSE)
octopart <- set1[1:5]
heptapart <- set1[6:10]
hexapart <- set1[11:15]
pentapart <- set2[1:4]
quadrapart <- set2[5:8]
tripart <- set2[9:12]
bipart <- set2[13:15]
datapart <- data.frame(blogs = c("blogs1", "blogs2", "blogs3", "blogs4", "blogs5",
                                 "blogs6", "blogs7", "blogs8", "blogs9", "blogs10",
                                 "blogs11", "blogs12", "blogs13", "blogs14", "blogs15"),
                       news = c("news1", "news2", "news3", "news4", "news5",
                                "news6", "news7", "news8", "news9", "news10",
                                "news11", "news12", "news13", "news14", "news15"),
                       twitter = c("twitter1", "twitter2", "twitter3", "twitter4", "twitter5",
                                   "twitter6", "twitter7", "twitter8", "twitter9", "twitter10",
                                   "twitter11", "twitter12", "twitter13", "twitter14", "twitter15"),
                       place = c(1:15))


#read in data
blogs1 <- read_lines(file("split_data/blogs_1.txt"), skip = 0)
news1 <- read_lines(file("split_data/news_1.txt"), skip = 0)
twitter1 <- read_lines(file("split_data/twitter_1.txt"), skip = 0)
blogs2 <- read_lines(file("split_data/blogs_2.txt"), skip = 0)
news2 <- read_lines(file("split_data/news_2.txt"), skip = 0)
twitter2 <- read_lines(file("split_data/twitter_2.txt"), skip = 0)
blogs3 <- read_lines(file("split_data/blogs_3.txt"), skip = 0)
news3 <- read_lines(file("split_data/news_3.txt"), skip = 0)
twitter3 <- read_lines(file("split_data/twitter_3.txt"), skip = 0)
blogs4 <- read_lines(file("split_data/blogs_4.txt"), skip = 0)
news4 <- read_lines(file("split_data/news_4.txt"), skip = 0)
twitter4 <- read_lines(file("split_data/twitter_4.txt"), skip = 0)
blogs5 <- read_lines(file("split_data/blogs_5.txt"), skip = 0)
news5 <- read_lines(file("split_data/news_5.txt"), skip = 0)
twitter5 <- read_lines(file("split_data/twitter_5.txt"), skip = 0)
blogs6 <- read_lines(file("split_data/blogs_6.txt"), skip = 0)
news6 <- read_lines(file("split_data/news_6.txt"), skip = 0)
twitter6 <- read_lines(file("split_data/twitter_6.txt"), skip = 0)
blogs7 <- read_lines(file("split_data/blogs_7.txt"), skip = 0)
news7 <- read_lines(file("split_data/news_7.txt"), skip = 0)
twitter7 <- read_lines(file("split_data/twitter_7.txt"), skip = 0)
blogs8 <- read_lines(file("split_data/blogs_8.txt"), skip = 0)
news8 <- read_lines(file("split_data/news_8.txt"), skip = 0)
twitter8 <- read_lines(file("split_data/twitter_8.txt"), skip = 0)
blogs9 <- read_lines(file("split_data/blogs_9.txt"), skip = 0)
news9 <- read_lines(file("split_data/news_9.txt"), skip = 0)
twitter9 <- read_lines(file("split_data/twitter_9.txt"), skip = 0)
blogs10 <- read_lines(file("split_data/blogs_10.txt"), skip = 0)
news10 <- read_lines(file("split_data/news_10.txt"), skip = 0)
twitter10 <- read_lines(file("split_data/twitter_10.txt"), skip = 0)
blogs11 <- read_lines(file("split_data/blogs_11.txt"), skip = 0)
news11 <- read_lines(file("split_data/news_11.txt"), skip = 0)
twitter11 <- read_lines(file("split_data/twitter_11.txt"), skip = 0)
blogs12 <- read_lines(file("split_data/blogs_12.txt"), skip = 0)
news12 <- read_lines(file("split_data/news_12.txt"), skip = 0)
twitter12 <- read_lines(file("split_data/twitter_12.txt"), skip = 0)
blogs13 <- read_lines(file("split_data/blogs_13.txt"), skip = 0)
news13 <- read_lines(file("split_data/news_13.txt"), skip = 0)
twitter13 <- read_lines(file("split_data/twitter_13.txt"), skip = 0)
blogs14 <- read_lines(file("split_data/blogs_14.txt"), skip = 0)
news14 <- read_lines(file("split_data/news_14.txt"), skip = 0)
twitter14 <- read_lines(file("split_data/twitter_14.txt"), skip = 0)
blogs15 <- read_lines(file("split_data/blogs_15.txt"), skip = 0)
news15 <- read_lines(file("split_data/news_15.txt"), skip = 0)
twitter15 <- read_lines(file("split_data/twitter_15.txt"), skip = 0)

##################### octogram #####################
#combine data
#list <- datapart[octopart, c(1:3)]
#list <- paste(list[1, 1], list[1, 2], list[1, 3],
#              list[2, 1], list[2, 2], list[2, 3],
#              list[3, 1], list[3, 2], list[3, 3],
#              list[4, 1], list[4, 2], list[4, 3],
#              list[5, 1], list[5, 2], list[5, 3], sep = ", ")
#list


#manually paste list into concatenate function
string <- concatenate(blogs10, news10, twitter10, blogs1, news1, twitter1, blogs12, news12, 
                      twitter12, blogs2, news2, twitter2, blogs6, news6, twitter6)


#prepare corpus
corpus <- VCorpus(VectorSource(string))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)


#build ngrams

ocho <- function(x) unlist(lapply(ngrams(words(x), 8), paste, collapse = " "), use.names = FALSE)
dtm8 <- DocumentTermMatrix(corpus,control=list(tokenize=ocho))
octolength <- round(length(as.matrix(dtm8))*.9)
octogram <- findMostFreqTerms(dtm8, n = octolength)


#run cleanNgrams

source('~/Capstone_Project/Model/cleanNgrams.R')
octogram <- cleanNgrams(octogram, 8)
write.table(octogram, "Model/octogram.csv", col.names=T, row.name=F, append=F, sep=",")

############### end Octogram #######################


##################### heptagram #####################
#combine data
#list <- datapart[heptapart, c(1:3)]
#list <- paste(list[1, 1], list[1, 2], list[1, 3],
#              list[2, 1], list[2, 2], list[2, 3],
#              list[3, 1], list[3, 2], list[3, 3],
#              list[4, 1], list[4, 2], list[4, 3],
#              list[5, 1], list[5, 2], list[5, 3], sep = ", ")
#list


#manually paste list into concatenate function

string <- concatenate(blogs9, news9, twitter9, blogs4, news4, twitter4, blogs14, news14, 
                      twitter14, blogs11, news11, twitter11, blogs15, news15, twitter15)


#prepare corpus

corpus <- VCorpus(VectorSource(string))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)


#build ngrams

siete <- function(x) unlist(lapply(ngrams(words(x), 7), paste, collapse = " "), use.names = FALSE)
dtm7 <- DocumentTermMatrix(corpus,control=list(tokenize=siete))
heptalength <- round(length(as.matrix(dtm7))*.9)
heptagram <- findMostFreqTerms(dtm7, n = heptalength)


#run cleanNgrams 

source('~/Capstone_Project/Model/cleanNgrams.R')
heptagram <- cleanNgrams(heptagram, 7)
write.table(heptagram, "Model/heptagram.csv", col.names=T, row.name=F, append=F, sep=",")

############### end heptagram #######################


##################### hexagram #####################
#combine data
#list <- datapart[hexapart, c(1:3)]
#list <- paste(list[1, 1], list[1, 2], list[1, 3],
#              list[2, 1], list[2, 2], list[2, 3],
#              list[3, 1], list[3, 2], list[3, 3],
#              list[4, 1], list[4, 2], list[4, 3],
#              list[5, 1], list[5, 2], list[5, 3], sep = ", ")
#list


#manually paste list into concatenate function

string <- concatenate(blogs8, news8, twitter8, blogs13, news13, twitter13, blogs5, news5, 
                      twitter5, blogs7, news7, twitter7, blogs3, news3, twitter3)


#prepare corpus

corpus <- VCorpus(VectorSource(string))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)


#build ngrams

seis <- function(x) unlist(lapply(ngrams(words(x), 6), paste, collapse = " "), use.names = FALSE)
dtm6 <- DocumentTermMatrix(corpus,control=list(tokenize=seis))
hexalength <- round(length(as.matrix(dtm6))*.9)
hexagram <- findMostFreqTerms(dtm6, n = hexalength)


#run cleanNgrams 

source('~/Capstone_Project/Model/cleanNgrams.R')
hexagram <- cleanNgrams(hexagram, 6)
write.table(hexagram, "Model/hexagram.csv", col.names=T, row.name=F, append=F, sep=",")

############### end hexagram #######################


##################### pentagram #####################
#combine data
#list <- datapart[pentapart, c(1:3)]
#list <- paste(list[1, 1], list[1, 2], list[1, 3],
#              list[2, 1], list[2, 2], list[2, 3],
#              list[3, 1], list[3, 2], list[3, 3],
#              list[4, 1], list[4, 2], list[4, 3], sep = ", ")
#list


#manually paste list into concatenate function

string <- concatenate(blogs13, news13, twitter13, blogs15, news15, twitter15, blogs8, 
                      news8, twitter8, blogs11, news11, twitter11)


#prepare corpus

corpus <- VCorpus(VectorSource(string))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)


#build ngrams

cinco <- function(x) unlist(lapply(ngrams(words(x), 5), paste, collapse = " "), use.names = FALSE)
dtm5 <- DocumentTermMatrix(corpus,control=list(tokenize=cinco))
pentalength <- round(length(as.matrix(dtm5))*.9)
pentagram <- findMostFreqTerms(dtm5, n = pentalength)


#run cleanNgrams 

source('~/Capstone_Project/Model/cleanNgrams.R')
pentagram <- cleanNgrams(pentagram, 5)
write.table(pentagram, "Model/pentagram.csv", col.names=T, row.name=F, append=F, sep=",")

############### end pentagram #######################


##################### quadragram #####################
#combine data
#list <- datapart[quadrapart, c(1:3)]
#list <- paste(list[1, 1], list[1, 2], list[1, 3],
#              list[2, 1], list[2, 2], list[2, 3],
#              list[3, 1], list[3, 2], list[3, 3],
#              list[4, 1], list[4, 2], list[4, 3], sep = ", ")
#list


#manually paste list into concatenate function

string <- concatenate(blogs5, news5, twitter5, blogs6, news6, twitter6, 
                      blogs14, news14, twitter14, blogs1, news1, twitter1)


#prepare corpus

corpus <- VCorpus(VectorSource(string))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)


#build ngrams

cuatro <- function(x) unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = FALSE)
dtm4 <- DocumentTermMatrix(corpus,control=list(tokenize=cuatro))
quadralength <- round(length(as.matrix(dtm4))*.9)
quadragram <- findMostFreqTerms(dtm4, n = quadralength)


#run cleanNgrams 

source('~/Capstone_Project/Model/cleanNgrams.R')
quadragram <- cleanNgrams(quadragram, 4)
write.table(quadragram, "Model/quadragram.csv", col.names=T, row.name=F, append=F, sep=",")

############### end quadragram #######################


##################### trigram #####################
#combine data
#list <- datapart[tripart, c(1:3)]
#list <- paste(list[1, 1], list[1, 2], list[1, 3],
#              list[2, 1], list[2, 2], list[2, 3],
#              list[3, 1], list[3, 2], list[3, 3],
#              list[4, 1], list[4, 2], list[4, 3], sep = ", ")
#list


#manually paste list into concatenate function

string <- concatenate(blogs7, news7, twitter7, blogs9, news9, twitter9, 
                      blogs2, news2, twitter2, blogs10, news10, twitter10)


#prepare corpus

corpus <- VCorpus(VectorSource(string))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)


#build ngrams

tres <- function(x) unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
dtm3 <- DocumentTermMatrix(corpus,control=list(tokenize=tres))
trilength <- round(length(as.matrix(dtm3))*.9)
trigram <- findMostFreqTerms(dtm3, n = trilength)


#run cleanNgrams 

source('~/Capstone_Project/Model/cleanNgrams.R')
trigram <- cleanNgrams(trigram, 3, filt = 5)
write.table(trigram, "Model/trigram.csv", col.names=T, row.name=F, append=F, sep=",")

############### end trigram #######################


##################### bigram #####################
#combine data
#list <- datapart[bipart, c(1:3)]
#list <- paste(list[1, 1], list[1, 2], list[1, 3],
#              list[2, 1], list[2, 2], list[2, 3],
#              list[3, 1], list[3, 2], list[3, 3], sep = ", ")
#list


#manually paste list into concatenate function

string <- concatenate(blogs4, news4, twitter4, blogs12, news12, 
                      twitter12, blogs3, news3, twitter3)


#prepare corpus

corpus <- VCorpus(VectorSource(string))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)


#build ngrams

dos <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
dtm2 <- DocumentTermMatrix(corpus,control=list(tokenize=dos))
bilength <- round(length(as.matrix(dtm2))*.9)
bigram <- findMostFreqTerms(dtm2, n = bilength)


#run cleanNgrams 

source('~/Capstone_Project/Model/cleanNgrams.R')
bigram <- cleanNgrams(bigram, 2, filt = 10)
write.table(bigram, "Model/bigram.csv", col.names=T, row.name=F, append=F, sep=",")

############### end bigram #######################


#build unigrams
dtm <- DocumentTermMatrix(corpus)
unigram <- findMostFreqTerms(dtm, n = 10L)

























