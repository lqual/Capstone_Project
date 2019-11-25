library(dplyr)
library(ngram)
library(tm)
library(stringr)

#loads data file onto computer if it isn't already there
if(!file.exists("swearWords.csv")) {
        url <- "http://www.bannedwordlist.com/lists/swearWords.csv"
        download.file(url, "swearWords.csv")
}


#load ngrams and profanity
octogram <- read.csv("Model/octogram_final.csv", stringsAsFactors = F)
heptagram <- read.csv("Model/heptagram_final.csv", stringsAsFactors = F)
hexagram <- read.csv("Model/hexagram_final.csv", stringsAsFactors = F)
pentagram <- read.csv("Model/pentagram_final.csv", stringsAsFactors = F)
quadragram <- read.csv("Model/quadragram_final.csv", stringsAsFactors = F)
trigram <- read.csv("Model/trigram_final.csv", stringsAsFactors = F)
bigram <- read.csv("Model/bigram_final.csv", stringsAsFactors = F)


#clean profanity to match words
profanity <- read.csv("swearWords.csv", stringsAsFactors = F)
profanity <- colnames(profanity)
profanity <- tolower(profanity)
profanity <- gsub("\\.", "", profanity)


#filter profanity out of ngram results
id <- c(1:length(profanity))
for(i in id) {
      octogram <- octogram %>% filter(next_word != profanity[i])
      heptagram <- heptagram %>% filter(next_word != profanity[i])
      hexagram <- hexagram %>% filter(next_word != profanity[i])
      pentagram <- pentagram %>% filter(next_word != profanity[i])
      quadragram <- quadragram %>% filter(next_word != profanity[i])
      trigram <- trigram %>% filter(next_word != profanity[i])
      bigram <- bigram %>% filter(next_word != profanity[i])
}


#replace files
write.table(octogram, "Model/octogram_final.csv", col.names=T, row.name=F, append=F, sep=",")
write.table(heptagram, "Model/heptagram_final.csv", col.names=T, row.name=F, append=F, sep=",")
write.table(hexagram, "Model/hexagram_final.csv", col.names=T, row.name=F, append=F, sep=",")
write.table(pentagram, "Model/pentagram_final.csv", col.names=T, row.name=F, append=F, sep=",")
write.table(quadragram, "Model/quadragram_final.csv", col.names=T, row.name=F, append=F, sep=",")
write.table(trigram, "Model/trigram_final.csv", col.names=T, row.name=F, append=F, sep=",")
write.table(bigram, "Model/bigram_final.csv", col.names=T, row.name=F, append=F, sep=",")











