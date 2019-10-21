#read data into R
library(readr)
blogs <- read_lines(file("final/en_US/en_US.blogs.txt"), skip = 0)
news <- read_lines(file("final/en_US/en_US.news.txt"), skip = 0)
twitter <- read_lines(file("final/en_US/en_US.twitter.txt"), skip = 0)

#find longest line
max(nchar(blogs))
max(nchar(news))
max(nchar(twitter))






