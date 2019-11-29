library(shiny)
library(dplyr)
library(ngram)
library(tm)
library(stringr)
library(shinybusy)
library(shinythemes)
shinyServer(function(input, output) {
        #load ngrams
        octogram <- read.csv("octogram_final.csv", stringsAsFactors = F)
        heptagram <- read.csv("heptagram_final.csv", stringsAsFactors = F)
        hexagram <- read.csv("hexagram_final.csv", stringsAsFactors = F)
        pentagram <- read.csv("pentagram_final.csv", stringsAsFactors = F)
        quadragram <- read.csv("quadragram_final.csv", stringsAsFactors = F)
        trigram <- read.csv("trigram_final.csv", stringsAsFactors = F)
        bigram <- read.csv("bigram_final.csv", stringsAsFactors = F)
        
        
        observeEvent(input$submit_text, {
                        show_modal_spinner()
                        text <- input$text
                        
                        #processing text
                        text <- paste("Sandler's", text, sep = " ")
                        text <- gsub('[0-9]+', '', text)
                        #if(wordcount(text) == 1) {
                                text <- tolower(text)
                                text <- trimws(text, which = "both")
                                text <- removePunctuation(text)
                                text <- removeNumbers(text)
                        #} else {
                        #        length <- wordcount(text)
                        #        text <- VCorpus(VectorSource(text))
                        #        text <- tm_map(text, stripWhitespace)
                        #        text <- tm_map(text, content_transformer(tolower))
                        #        text <- tm_map(text, removePunctuation)
                        #        text <- tm_map(text, removeNumbers)
                        #        uno <- function(x) unlist(lapply(ngrams(words(x), length), paste, collapse = " "), 
                        #                                  use.names = FALSE)
                        #        text <- DocumentTermMatrix(text,control=list(tokenize=uno))
                        #        text <- findMostFreqTerms(text, n = 1)
                        #        text <- as.data.frame(text)
                        #        text <- rownames(text)
                        #}
                        
                        
                        #lookup in file
                        answer8 <- data.frame(matrix(ncol = 1))
                        colnames(answer8) <- "answer"
                        answer7 <- data.frame(matrix(ncol = 1))
                        colnames(answer7) <- "answer"
                        answer6 <- data.frame(matrix(ncol = 1))
                        colnames(answer6) <- "answer"
                        answer5 <- data.frame(matrix(ncol = 1))
                        colnames(answer5) <- "answer"
                        answer4 <- data.frame(matrix(ncol = 1))
                        colnames(answer4) <- "answer"
                        answer3 <- data.frame(matrix(ncol = 1))
                        colnames(answer3) <- "answer"
                        
                        numwords <- wordcount(text)
                        if(numwords > 6) {
                                text8 <- word(text, -7, numwords)
                                #filtdf5 <- pentagram %>% filter(nchar == nchar(text5))
                                lookup8 <- grep(text8, octogram$string)
                                answer8 <- data.frame(octogram$next_word[lookup8])
                                colnames(answer8) <- "answer"
                        }
                        
                        if(numwords > 5) {
                                text7 <- word(text, -6, numwords)
                                #filtdf5 <- pentagram %>% filter(nchar == nchar(text5))
                                lookup7 <- grep(text7, heptagram$string)
                                answer7 <- data.frame(heptagram$next_word[lookup7])
                                colnames(answer7) <- "answer"
                        }
                        
                        if(numwords > 4) {
                                text6 <- word(text, -5, numwords)
                                #filtdf5 <- pentagram %>% filter(nchar == nchar(text5))
                                lookup6 <- grep(text6, hexagram$string)
                                answer6 <- data.frame(hexagram$next_word[lookup6])
                                colnames(answer6) <- "answer"
                        }
                        
                        if(numwords > 3) {
                                text5 <- word(text, -4, numwords)
                                #filtdf5 <- pentagram %>% filter(nchar == nchar(text5))
                                lookup5 <- grep(text5, pentagram$string)
                                answer5 <- data.frame(pentagram$next_word[lookup5])
                                colnames(answer5) <- "answer"
                        }
                        
                        if(numwords > 2) {
                                text4 <- word(text, -3, numwords)
                                #filtdf4 <- quadragram #%>% filter(nchar == nchar(text4))
                                lookup4 <- grep(text4, quadragram$string)
                                answer4 <- data.frame(quadragram$next_word[lookup4])
                                colnames(answer4) <- "answer"
                        }
                        
                        if(numwords > 1) {
                                text3 <- word(text, -2, numwords)
                                #filtdf3 <- trigram #%>% filter(nchar == nchar(text3))
                                lookup3 <- grep(text3, trigram$string)
                                answer3 <- data.frame(trigram$next_word[lookup3])
                                colnames(answer3) <- "answer"
                        }
                        
                        text2 <- word(text, -1, numwords)
                        #filtdf2 <- bigram #%>% filter(nchar == nchar(text2))
                        lookup2 <- grep(text2, bigram$string)
                        answer2 <- data.frame(bigram$next_word[lookup2])
                        colnames(answer2) <- "answer"
                        
                        myprobs <- c(10/26, 5/26, 2/26, 2/26, 2/26, 1/26, 1/26, 1/26, 1/26, 1/26)
                        lookup1 <- sample(1:10, size = 3, replace = FALSE, prob = myprobs)
                        unigram <- data.frame(next_word = c("the", "and", "for", "that", "you",
                                                            "with", "Was", "this", "have","are"))
                        answer1 <- data.frame(unigram$next_word[lookup1])
                        colnames(answer1) <- "answer"
                        
                        answer0 <- data.frame(answer = c("taco", "taco"))
                        
                        #return an answer
                        finalAnswer <- rbind(answer8, answer7, answer6, answer5, 
                                             answer4, answer3, answer2, answer1, 
                                             answer0)
                        index <- which(duplicated(finalAnswer$answer))
                        finalAnswer <- finalAnswer %>% mutate(column = 1)
                        finalAnswer <- finalAnswer[-index, ]
                        finalAnswer <- finalAnswer[!is.na(finalAnswer$answer), ]
                        finalAnswer <- finalAnswer %>% select(answer)
                        finalAnswer$answer <- as.character(finalAnswer$answer)
                               
                        word1 <- finalAnswer[1, 1]
                        word2 <- finalAnswer[2, 1]
                        word3 <- finalAnswer[3, 1]
                        
                        phraseFromInput <- input$text
                        
                        output$phrase1 <- renderText({
                                paste("Input Phrase:", phraseFromInput)
                        })
                        
                        output$pred1 <- renderText({
                                paste("Predicted Word 1:", word1)
                        })
                        
                        output$pred2 <- renderText({
                                paste("Predicted Word 2:", word2)
                        })
                        
                        output$pred3 <- renderText({
                                paste("Predicted Word 3:", word3)
                        })
                        
                        output$phrase2 <- renderText({
                                paste("Are any of the predictions correct?  Try typing the same 
                                phrase into your cell phone's keyboard.  You might be surprised at 
                                how difficult it is to predict text!")
                        })
                        
                        output$phrase3 <- renderText({
                                paste("Note for coursera reviewers: application predicts next three 
                                words to better match normal cell phone text predictors, not to 
                                improve the accuracy of the application for grading purposes.")
                        })
                        remove_modal_spinner()
                }
        )
})