library(dplyr)
library(ngram)
library(tm)


text <- "I hope you are"
length <- wordcount(text)
text <- VCorpus(VectorSource(text))
text <- tm_map(text, stripWhitespace)
text <- tm_map(text, content_transformer(tolower))
text <- tm_map(text, removePunctuation)
text <- tm_map(text, removeNumbers)
uno <- function(x) unlist(lapply(ngrams(words(x), length), paste, collapse = " "), use.names = FALSE)
text <- DocumentTermMatrix(text,control=list(tokenize=uno))
text <- findMostFreqTerms(text, n = 1)
text[1]















