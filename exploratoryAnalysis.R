#libraries
library(readr)
library(ngram)
library(textreg)
library(tm)
library(parallel)
library(doParallel)

#read data into R
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)
blogs <- read_lines(file("final/en_US/en_US.blogs.txt"), skip = 0)
news <- read_lines(file("final/en_US/en_US.news.txt"), skip = 0)
twitter <- read_lines(file("final/en_US/en_US.twitter.txt"), skip = 0)
stopCluster(cluster)
registerDoSEQ()

#table summation of the data
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)
sum_table <- data.frame(matrix(nrow = 3, ncol = 4))
colnames(sum_table) <- c("File", "Size", "Lines", "Words")
sum_table$File <- c("Blogs", "News", "Twitter")
sum_table$Size <- c("255.4 MB", "257.3 MB", "319.0 MB")
sum_table$Lines <- c(length(blogs), length(news), length(twitter))
sum_table$Words <- c(wordcount(blogs), wordcount(news), wordcount(twitter))
sum_table
stopCluster(cluster)
registerDoSEQ()

#making 10 sets of data
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)
set.seed(21)
twitter <- data.frame(twitter = twitter, group = sample(1:10, 1, replace = TRUE)) 


stopCluster(cluster)
registerDoSEQ()


#build a corpus for analysis
full_corpus <- data.frame(blogs = blogs, news = news, twitter = twitter)
small_corpus <- list(blogs = NA, news = NA, twitter = NA)
set.seed(21)
group <- lapply(full_corpus, function() sample(1:10, 1, replace = TRUE))











