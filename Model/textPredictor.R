library(dplyr)
library(ngram)
library(tm)


#processing text
text <- "It would mean the"
length <- wordcount(text)
text <- VCorpus(VectorSource(text))
text <- tm_map(text, stripWhitespace)
text <- tm_map(text, content_transformer(tolower))
text <- tm_map(text, removePunctuation)
text <- tm_map(text, removeNumbers)
uno <- function(x) unlist(lapply(ngrams(words(x), length), paste, collapse = " "), use.names = FALSE)
text <- DocumentTermMatrix(text,control=list(tokenize=uno))
text <- findMostFreqTerms(text, n = 1)
text <- as.data.frame(text)
text <- rownames(text)


#lookup in file
numwords <- wordcount(text)
text5 <- word(text, -4, numwords)
#filtdf5 <- pentagram %>% filter(nchar == nchar(text5))
lookup5 <- grep(text5, pentagram$string)
answer5 <- data.frame(pentagram$next_word[lookup5])

text4 <- word(text, -3, numwords)
#filtdf4 <- quadragram #%>% filter(nchar == nchar(text4))
lookup4 <- grep(text4, quadragram$string)
answer4 <- data.frame(quadragram$next_word[lookup4])

text3 <- word(text, -2, numwords)
#filtdf3 <- trigram #%>% filter(nchar == nchar(text3))
lookup3 <- grep(text3, trigram$string)
answer3 <- data.frame(trigram$next_word[lookup3])

text2 <- word(text, -1, numwords)
#filtdf2 <- bigram #%>% filter(nchar == nchar(text2))
lookup2 <- grep(text2, bigram$string)
answer2 <- data.frame(bigram$next_word[lookup2])






