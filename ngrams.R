#read file
blogs <- read_lines(file("final/en_US/en_US.blogs.txt"), skip = 0, 
                         n_max = 50)
news <- read_lines(file("final/en_US/en_US.news.txt"), skip = 0, 
                    n_max = 50)
twitter <- read_lines(file("final/en_US/en_US.twitter.txt"), skip = 0, 
                    n_max = 50)

#ngram
library(ngram)
string <- concatenate(blogs, news, twitter)
string <- preprocess(string, case = "lower", remove.punct = TRUE, fix.spacing = TRUE)
ng <- ngram(string, n=3)
head(get.phrasetable(ngram(string, n=3)), 15)
babble(ng, 10, seed = 10)

#tm
library(tm)
library(SnowballC)
library(ngram)
string <- concatenate(blogs, news, twitter)
corpus <- VCorpus(VectorSource(string))
writeCorpus(corpus, filenames = "corpus.txt")
inspect(corpus)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, stemDocument)
dtm <- DocumentTermMatrix(corpus)
inspect(dtm)
findFreqTerms(dtm, 5)
inspect(removeSparseTerms(dtm, 0.4))









