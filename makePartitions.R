#read data into R
library(readr)
blogs <- read_lines(file("final/en_US/en_US.blogs.txt"), skip = 0)
nLines <- round(0.005 * length(blogs)-1)
partition <- round(length(blogs)/60-1)
#skipdf <- data.frame(skip = c(1:60))
set.seed(542)
start <- Sys.time()
for(i in 1:60) {
        low <- (i - 1) * partition
        high <- low + partition - nLines
        num <- sample(low:high, 1)
        #skipdf[i, 1] <- num
        split_blog <- read_lines(file("final/en_US/en_US.blogs.txt"), skip = num, 
                            n_max = nLines)
        name <- paste("split_data/blogs/blogs_", i, ".txt", sep = "")
        write_lines(split_blog, name)
}
finish <- Sys.time()
print(finish - start)

#######################Trial 2 ##########################
#read data into R
library(readr)
blogs <- read_lines(file("final/en_US/en_US.blogs.txt"), skip = 0)
nLines <- round(0.005 * length(blogs)-1)
partition <- round(length(blogs)/60-1)
set.seed(542)
id <- sample(1:60, 60, replace = F)
start <- Sys.time()
for(i in id) {
        low <- (i - 1) * partition
        high <- low + partition - nLines
        num <- sample(low:high, 1)
        #skipdf[i, 1] <- num
        split_blog <- read_lines(file("final/en_US/en_US.blogs.txt"), skip = num, 
                                 n_max = nLines)
        name <- paste("split_data/blogs/blogs_", i, ".txt", sep = "")
        write_lines(split_blog, name)
}
finish <- Sys.time()
print(finish - start)

#making a training, test, and validation set (2 of each)
combine <- sample(1:60, 60, replace = F)
file_list <- list.files("split_data/blogs")
library(plyr)
training1 <- ldply(file_list[combine[1:4]], read_lines)



#library(dplyr)
#skipdf1 <- skipdf %>% mutate(dif = skip + nLines)






news <- read_lines(file("final/en_US/en_US.news.txt"), skip = 0)
twitter <- read_lines(file("final/en_US/en_US.twitter.txt"), skip = 0)

max(nchar(news))
max(nchar(twitter))