#---------------------------------------------------------------------------------------
# filename: predictNextWord
# author: Luis Padua
# summary:
#       it grabs the user text input, since it found a space as last character
#       the function will find what word will be the most frequent token found in the 
#       4gram / 3gram or 2gram Term matrix to return the top3 possible choices
#---------------------------------------------------------------------------------------

predictNextWord <- function(input) {
        input <- removePunctuation(input)
        input <- stripWhitespace(input)
        input <- tolower(input)
        input <- strsplit(input, split = " ")
        names(input) <- "token"
        
        lastWord <- function(x) tail(strsplit(x,split=" ")[[1]],1)
        
        ind <- grep(paste("^", input$token[length(input$token)-2], " ",
                          input$token[length(input$token)-1], " ",
                          input$token[length(input$token)], " ", sep = ""), fourgramDTM$dimnames$Terms)
        a <- rowSums(as.matrix(fourgramDTM[ind,]))
        a <- sort(a, decreasing = TRUE)
        a <- names(head(a, 3))
        
        if (length(a) < 3) {
                ind <- grep(paste("^", input$token[length(input$token)-1], " ",
                                  input$token[length(input$token)], " ", sep = ""), trigramDTM$dimnames$Terms)
                b <- rowSums(as.matrix(trigramDTM[ind,]))
                b <- sort(b, decreasing = TRUE)
                b <- names(head(b, 3))
                a <- c(a, b)
                
                if (length(a) < 3) {
                        ind <- grep(paste("^", input$token[length(input$token)], " ", sep = ""), bigramDTM$dimnames$Terms)
                        b <- rowSums(as.matrix(bigramDTM[ind,]))
                        b <- sort(b, decreasing = TRUE)
                        b <- names(head(b, 3))
                        a <- c(a, b)
                        if (length(a) == 0) a <- c(a, "", "", "")
                        if (length(a) == 1) a <- c(a, "", "")
                        if (length(a) == 2) a <- c(a, "")
                }
        }
        
        a <- sapply(a, lastWord)
        
        return(unname(a))
}

