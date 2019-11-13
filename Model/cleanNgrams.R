#build dataframes function
cleanNgrams <- function(ngram, numWords, filt = 5) {
        library(dplyr)
        library(stringr)
        library(tm)
        library(ngram)
        ngram <- as.data.frame(ngram, row.names = NULL)
        head(rownames(ngram))
        ngram <- data.frame(string = rownames(ngram), num = ngram$X1)
        ngram$string <- as.character(ngram$string)
        ngram <- ngram %>% filter(num > filt) %>% 
                mutate(next_word = word(string, -1), nchar = 0) %>%
                mutate(string = word(string, 1, numWords - 1)) 
        index <- which(duplicated(ngram$string))
        ngram <- ngram[-index, ]
        id <- c(1:length(ngram$string))
        for(i in id) {
                char <- nchar(ngram$string[i])
                ngram$nchar[i] <- char
        }
        ngram
}