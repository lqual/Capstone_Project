quiz <- function(word1, word2, word3, word4, finalAnswer) {
        loc1 <- grep(word1, finalAnswer)
        loc2 <- grep(word2, finalAnswer) 
        loc3 <- grep(word3, finalAnswer) 
        loc4 <- grep(word4, finalAnswer)
        answer <- data.frame(word = c(word1, word2, word3, word4),
                             location = c(loc1[1], loc2[1], loc3[1], loc4[1]))
        answer
}        
        
        
        
        
        