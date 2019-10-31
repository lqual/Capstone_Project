#read data into R
library(readr)
blogs <- read_lines(file("final/en_US/en_US.blogs.txt"), skip = 0)
news <- read_lines(file("final/en_US/en_US.news.txt"), skip = 0)
twitter <- read_lines(file("final/en_US/en_US.twitter.txt"), skip = 0)

#find longest line
max(nchar(blogs))
max(nchar(news))
max(nchar(twitter))

#love/hate
love <- grepl("love", twitter)
hate <- grepl("hate", twitter)
sum(love)/sum(hate)

#biostats
bio <- grepl("biostats", twitter)
bio2 <- data.frame(location = c(1:2360148), bio)
library(dplyr)
bio2 <- bio2 %>% filter(bio == TRUE)

#exact match
exact <- grepl("A computer once beat me at chess, but it was no match for me at kickboxing", twitter)
exact2 <- data.frame(location = c(1:2360148), exact)
exact2 <- exact2 %>% filter(exact == TRUE)
exact2
grep("A computer once beat me at chess, but it was no match for me at kickboxing", twitter)





