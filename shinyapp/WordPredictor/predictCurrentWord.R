#---------------------------------------------------------------------------------------
# filename: predictCurrentWord
# author: Luis Padua
# summary:
#       it grabs the user text input, since the last word would be incomplete
#       the function will find what will be the most frequent token found in the corpus
#       that contains the initials typed by the user
#---------------------------------------------------------------------------------------

predictCurrentWord <- function(input) {
        
        inputraw <- input
        input <- removePunctuation(input)
        input <- stripWhitespace(input)
        input <- tolower(input)
        input <- strsplit(input, split = " ")
        names(input) <- "token"
        a <- vector()
        if (length(input$token)> 2) {
                a <- predictNextWordwithInitials(inputraw)
        }
        if (length(a) < 3) {
                ind <- grep(paste("^", input$token[length(input$token)], sep = ""), unigramDTM$dimnames$Terms)
                b <- rowSums(as.matrix(unigramDTM[ind,]))
                b <- sort(b, decreasing = TRUE)
                a <- c(a, head(names(b), 3))
                if (length(a) == 0) a <- c(a, "", "", "")
                if (length(a) == 1) a <- c(a, "", "")
                if (length(a) == 2) a <- c(a, "")
        }
        if(is.list(a)) unlist(a)
        
        return(a)
}

