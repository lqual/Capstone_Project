#loads data file onto computer if it isn't already there
if(!file.exists("final")) {
        url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
        download.file(url, "data.zip")
        unzip("data.zip")
}
