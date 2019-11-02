#loads data file onto computer if it isn't already there
if(!file.exists("final")) {
        url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
        download.file(url, "data.zip")
        unzip("data.zip")
}

blogsPartition <- function() {
        library(readr)
        library(caret)
        #splits blogs into manageable chunks
        start <- Sys.time()
        blogs <- read_lines(file("final/en_US/en_US.blogs.txt"), skip = 0)
        set.seed(485)
        folds<-createFolds(blogs, 20)
        for(i in 1:20) {
                partition <- blogs[folds[[i]]]
                name <- paste("split_data/blogs_", i, ".txt", sep = "")
                write_lines(partition, name)
        }
finish <- Sys.time()
print(finish - start)
}


newsPartition <- function() {
        library(readr)
        library(caret)
        #splits news into manageable chunks
        start <- Sys.time()
        news <- read_lines(file("final/en_US/en_US.news.txt"), skip = 0)
        set.seed(123)
        folds<-createFolds(news, 20)
        for(i in 1:20) {
                partition <- news[folds[[i]]]
                name <- paste("split_data/news_", i, ".txt", sep = "")
                write_lines(partition, name)
        }
        finish <- Sys.time()
        print(finish - start)
}


twitterPartition <- function() {
        library(readr)
        library(caret)
        #splits twitter into manageable chunks
        start <- Sys.time()
        twitter <- read_lines(file("final/en_US/en_US.twitter.txt"), skip = 0)
        set.seed(987)
        folds<-createFolds(twitter, 20)
        for(i in 1:20) {
                partition <- twitter[folds[[i]]]
                name <- paste("split_data/twitter_", i, ".txt", sep = "")
                write_lines(partition, name)
        }
        finish <- Sys.time()
        print(finish - start)
}










