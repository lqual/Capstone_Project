#libraries
library(readr)
library(ngram)
library(textreg)
library(tm)
library(caret)
library(SnowballC)


#read data into R
blogs <- read_lines(file("final/en_US/en_US.blogs.txt"), skip = 0)
news <- read_lines(file("final/en_US/en_US.news.txt"), skip = 0)
twitter <- read_lines(file("final/en_US/en_US.twitter.txt"), skip = 0)


#table summation of the data
sum_table <- data.frame(matrix(nrow = 3, ncol = 4))
colnames(sum_table) <- c("File", "Size", "Lines", "Words")
sum_table$File <- c("Blogs", "News", "Twitter")
sum_table$Size <- c("255.4 MB", "257.3 MB", "319.0 MB")
sum_table$Lines <- c(length(blogs), length(news), length(twitter))
sum_table$Words <- c(wordcount(blogs), wordcount(news), wordcount(twitter))
sum_table

#data processing
        #see downloadFile.R for data partitioning
blogs1 <- read_lines(file("split_data/blogs_1.txt"), skip = 0)
news1 <- read_lines(file("split_data/news_1.txt"), skip = 0)
twitter1 <- read_lines(file("split_data/news_1.txt"), skip = 0)
string <- concatenate(blogs1, news1, twitter1)
#string <- preprocess(string, case = "lower", remove.punct = TRUE, 
#                     fix.spacing = TRUE, remove.numbers = TRUE)
corpus <- VCorpus(VectorSource(string))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
writeCorpus(corpus, filenames = "corpus1.txt")


#ngrams
dtm <- DocumentTermMatrix(corpus)
unigram <- findMostFreqTerms(dtm, n = 10L)
dos <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
dtm2 <- DocumentTermMatrix(corpus,control=list(tokenize=dos))
bigram <- findMostFreqTerms(dtm2, n = 10L)
tres <- function(x) unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
dtm3 <- DocumentTermMatrix(corpus,control=list(tokenize=tres))
trigram <- findMostFreqTerms(dtm3, n = 10L)
cinco <- function(x) unlist(lapply(ngrams(words(x), 5), paste, collapse = " "), use.names = FALSE)
dtm5 <- DocumentTermMatrix(corpus,control=list(tokenize=cinco))
pentagram <- findMostFreqTerms(dtm5, n = 10L)
#n1 <- head(get.phrasetable(ngram(string, n=1)), 10)
#n2 <- head(get.phrasetable(ngram(string, n=2)), 10)
#n3 <- head(get.phrasetable(ngram(string, n=3)), 10)
#n4 <- head(get.phrasetable(ngram(string, n=4)), 10)
#n5 <- head(get.phrasetable(ngram(string, n=5)), 10)

#plot ngrams
unigram <- as.data.frame(unigram)
par(mar=c(4, 4, 2, 1))
barplot(unigram$X1, horiz = TRUE, names.arg = row.names(unigram), las = 1, xlab = "Occurences",
        main = "Top Ten Unigrams", col = "green")
bigram <- as.data.frame(bigram)
par(mar=c(4, 5, 2, 1))
barplot(bigram$X1, horiz = TRUE, names.arg = row.names(bigram), las = 1, xlab = "Occurences",
        main = "Top Ten Bigrams", col = "purple")
trigram <- as.data.frame(trigram)
par(mar=c(4, 6, 2, 1))
barplot(trigram$X1, horiz = TRUE, names.arg = row.names(trigram), las = 1, xlab = "Occurences",
        main = "Top Ten Trigrams", col = "red")
pentagram <- as.data.frame(pentagram)
par(mar=c(4, 9, 2, 1))
barplot(pentagram$X1, horiz = TRUE, names.arg = row.names(pentagram), las = 1, xlab = "Occurences",
        main = "Top Ten Pentagrams", col = "blue")








