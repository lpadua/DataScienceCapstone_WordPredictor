#------------------------------------------------------------------------
#      Capstone Project - Word Predictor
#                Luis Padua
#                jan2018
#------------------------------------------------------------------------

# required libraries
setwd("C:/DataScienceProjects/CapstoneProject/DataScienceCapstone_WordPredictor")
library(stringr)
library(tm)
library(ggplot2)
library(dplyr)

#------------------------------------------------------------------------
#      Loading the data and merging all
#------------------------------------------------------------------------
readData <- function(fileName) {
        # it is important to set "rb" to READ BINARY
        con <- file(fileName, "rb")
        #skipNul equals TRUE is necessary to Twitter data
        data <- readLines(con, skipNul = TRUE)
        data <- iconv(data, 'UTF-8', 'ASCII', sub='') #remove the UTF-8 strange character to a blank space
        close(con)
        return(data)
}

blogs <- readData("en_US/en_US.blogs.txt")
news <- readData("en_US/en_US.news.txt")
twitter <- readData("en_US/en_US.twitter.txt")


#------------------------------------------------------------------------
#      Basic characteristics of each file
#------------------------------------------------------------------------

corpusFiles <- list(blogs, news, twitter)

corpusSummary <- data.frame(textName = c("Blogs", "News", "Twitter"),
                            fileSizeMB = sapply(corpusFiles, function(x) object.size(x)/1048576),
                            linesCount = sapply(corpusFiles, length),
                            wordCount = sapply(corpusFiles, function(x) {sum(sapply(str_split(x, boundary("word")), length))}),
                            maxNumWordpLine = sapply(corpusFiles, function(x) {max(sapply(str_split(x, boundary("word")), length))}))

corpusSummary

remove(corpusSummary)
remove(corpusFiles)

#------------------------------------------------------------------------
#      Sampling the Data
#------------------------------------------------------------------------
set.seed(2018)
sizepct <- 0.005

blogsS <- sample(blogs, length(blogs)*sizepct)
newsS <- sample(news, length(news)*sizepct)
twitterS <- sample(twitter, length(twitter)*sizepct)

remove(blogs)
remove(news)
remove(twitter)

sampleAll <- c(blogsS, newsS, twitterS)

#------------------------------------------------------------------------
#      Creating the Corpus 
#------------------------------------------------------------------------
corpus <- VCorpus(VectorSource(sampleAll), 
                  readerControl = list(reader = readPlain,
                                       language = "en_US"))

corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers) 
#not sure about remove StopWords 
#corpus <- tm_map(corpus, removeWords, stopwords("english"))

corpus <- tm_map(corpus, stripWhitespace) 
corpus <- tm_map(corpus, content_transformer(tolower))

#------------------------------------------------------------------------
#      Calculating word frequency with highest frequencies
#------------------------------------------------------------------------
unigramDTM <- TermDocumentMatrix(corpus, control = list(wordLengths=c(1, Inf)))
words_frequency <- findFreqTerms(unigramDTM,lowfreq =1)      #lowfreq should be >1 with samplesize is more than 1%
# arrays can go greater than 10Gb - MEMORY PROBLEMS

unigramFreq <- rowSums(as.matrix(unigramDTM[words_frequency,]))
unigramTotal <- sum(unigramFreq)
unigramFreq <- data.frame(word=names(unigramFreq),frequency=unigramFreq, pctSample=100*unigramFreq/unigramTotal)
unigramFreq <- arrange(unigramFreq, desc(frequency))

ggplot(head(unigramFreq, 10), aes(x = reorder(word, -frequency), y = frequency, label = paste(round(pctSample, 1), "%", sep = ""))) + 
        geom_col() + 
        geom_label()+
        labs(x = "Tokens", y = "Frequency Count & Percentage of Sample") +
        annotate("text", label = paste("Total unigram Tokens in the sample: ", unigramTotal, 
                                       "\nTotal unique Tokens:", length(Terms(unigramDTM))), x = 8, y = unigramFreq$frequency[1]) +
        theme_bw()

#------------------------------------------------------------------------
#      Calculating word frequency with highest frequencies without Stopwords
#       interesting to evaluate the difference of using or not the StopWords
#------------------------------------------------------------------------
corpusWOstopwords <- tm_map(corpus, removeWords, stopwords("english"))
unigramDTMwoSW <- TermDocumentMatrix(corpusWOstopwords)
words_frequency <- findFreqTerms(unigramDTMwoSW,lowfreq =1)     #lowfreq should be >1 with samplesize is more than 1%
# arrays can go greater than 10Gb - MEMORY PROBLEMS

mostWordfreq <- rowSums(as.matrix(unigramDTMwoSW[words_frequency,]))
mostWordTotal <- sum(mostWordfreq)
mostWordfreq <- data.frame(word=names(mostWordfreq),frequency=mostWordfreq, pctSample=100*mostWordfreq/mostWordTotal)
mostWordfreq <- arrange(mostWordfreq, desc(frequency))

ggplot(head(mostWordfreq, 10), aes(x = reorder(word, -frequency), y = frequency, label = paste(round(pctSample, 1), "%", sep = ""))) + 
        geom_col() + 
        geom_label()+
        labs(x = "Tokens", y = "Frequency Count & Percentage of Sample") +
        annotate("text", label = paste("Total unigram Tokens in the sample: ", mostWordTotal, 
                                       "\nTotal unique Tokens:", length(Terms(unigramDTMwoSW))), x = 8, y = mostWordfreq$frequency[1]) +
        theme_bw()

#------------------------------------------------------------------------
#      Calculating 2grams word frequency with highest frequencies
#------------------------------------------------------------------------

Bigramtokenizer <- function(x)
        unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

bigramDTM <- TermDocumentMatrix(corpus,control = list(tokenize = Bigramtokenizer))

lowfreq = 1
words_frequency <- findFreqTerms(bigramDTM,lowfreq = lowfreq)

bigramFreq <- rowSums(as.matrix(bigramDTM[words_frequency,]))
bigramTotal <- sum(bigramFreq)
bigramFreq <- data.frame(word=names(bigramFreq),frequency=bigramFreq, pctSample=100*bigramFreq/bigramTotal)
bigramFreq <- arrange(bigramFreq, desc(frequency))

ggplot(head(bigramFreq, 10), aes(x = reorder(word, -frequency), y = frequency, label = paste(round(pctSample, 1), "%", sep = ""))) + 
        geom_col() +
        geom_label()+
        labs(x = "Tokens", y = "Frequency Count & Percentage of Sample") +
        annotate("text", label = paste("Total biGrams Tokens with freq > ", lowfreq, ":", bigramTotal,
                                       "\nTotal unique Tokens:", length(Terms(bigramDTM))), x = 8, y = bigramFreq$frequency[1]) +
        theme_bw()

#------------------------------------------------------------------------
#      Calculating 2grams word frequency with highest frequencies of Corpus w/o StopWords
#------------------------------------------------------------------------

bigramDTMwoSW <- TermDocumentMatrix(corpusWOstopwords,control = list(tokenize = Bigramtokenizer))

lowfreq = 2
words_frequency <- findFreqTerms(bigramDTMwoSW,lowfreq = lowfreq)

bigramFreqwoSW <- rowSums(as.matrix(bigramDTMwoSW[words_frequency,]))
bigramTotal <- sum(bigramFreqwoSW)
bigramFreqwoSW <- data.frame(word=names(bigramFreqwoSW),frequency=bigramFreqwoSW, pctSample=100*bigramFreqwoSW/bigramTotal)
bigramFreqwoSW <- arrange(bigramFreqwoSW, desc(frequency))

ggplot(head(bigramFreqwoSW, 10), aes(x = reorder(word, -frequency), y = frequency, label = paste(round(pctSample, 1), "%", sep = ""))) + 
        geom_col() +
        geom_label()+
        labs(x = "Tokens", y = "Frequency Count & Percentage of Sample") +
        annotate("text", label = paste("Total biGrams Tokens with freq > ", lowfreq, ":", bigramTotal,
                                       "\nTotal unique Tokens:", length(Terms(bigramDTMwoSW))), x = 8, y = bigramFreqwoSW$frequency[1]) +
        theme_bw()

#------------------------------------------------------------------------
#      Calculating 3grams word frequency with highest frequencies
#------------------------------------------------------------------------

trigramtokenizer <- function(x) {
        unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE) }

trigramDTM <- TermDocumentMatrix(corpus,control = list(tokenize = trigramtokenizer))

lowfreq = 2
words_frequency <- findFreqTerms(trigramDTM,lowfreq = lowfreq)

trigramFreq <- rowSums(as.matrix(trigramDTM[words_frequency,]))
trigramTotal <- sum(trigramFreq)
trigramFreq <- data.frame(word=names(trigramFreq),frequency=trigramFreq, pctSample=100*trigramFreq/trigramTotal)
trigramFreq <- arrange(trigramFreq, desc(frequency))

ggplot(head(trigramFreq, 10), aes(x = reorder(word, -frequency), y = frequency, label = paste(round(pctSample, 1), "%", sep = ""))) + 
        geom_col() + 
        geom_label() +
        labs(x = "Tokens", y = "Frequency Count & Percentage of Sample") +
        annotate("text", label = paste("Total triGrams Tokens with freq > ", lowfreq, ":", trigramTotal,
                                       "\nTotal unique Tokens:", length(Terms(trigramDTM))), 
                 x = 8, y = trigramFreq$frequency[1]) +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) + theme_bw()


#------------------------------------------------------------------------
#      Calculating 3grams word frequency with highest frequencies without StopWords
#------------------------------------------------------------------------

trigramDTMwoSW <- TermDocumentMatrix(corpusWOstopwords,control = list(tokenize = trigramtokenizer))

lowfreq = 2
words_frequency <- findFreqTerms(trigramDTMwoSW,lowfreq = lowfreq)

trigramFreqwoSW <- rowSums(as.matrix(trigramDTMwoSW[words_frequency,]))
trigramTotal <- sum(trigramFreqwoSW)
trigramFreqwoSW <- data.frame(word=names(trigramFreqwoSW),frequency=trigramFreqwoSW, pctSample=100*trigramFreqwoSW/trigramTotal)
trigramFreqwoSW <- arrange(trigramFreqwoSW, desc(frequency))

ggplot(head(trigramFreqwoSW, 10), aes(x = reorder(word, -frequency), y = frequency, label = paste(round(pctSample, 1), "%", sep = ""))) + 
        geom_col() + 
        geom_label() +
        labs(x = "Tokens", y = "Frequency Count & Percentage of Sample") +
        annotate("text", label = paste("Total triGrams Tokens with freq > ", lowfreq, ":", trigramTotal,
                                       "\nTotal unique Tokens:", length(Terms(trigramDTMwoSW))), 
                 x = 8, y = trigramFreqwoSW$frequency[1]) + theme_bw() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) 

#------------------------------------------------------------------------
#      Calculating 4grams word frequency with highest frequencies
#------------------------------------------------------------------------

fourgramtokenizer <- function(x) {
        unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = FALSE) }

fourgramDTM <- TermDocumentMatrix(corpus,control = list(tokenize = fourgramtokenizer))

lowfreq = 2
most_words <- findFreqTerms(fourgramDTM,lowfreq = lowfreq)

fourgramFreq <- rowSums(as.matrix(fourgramDTM[most_words,]))
fourgramTotal <- sum(fourgramFreq)
fourgramFreq <- data.frame(word=names(fourgramFreq),frequency=fourgramFreq, pctSample=100*fourgramFreq/fourgramTotal)
fourgramFreq <- arrange(fourgramFreq, desc(frequency))

ggplot(head(fourgramFreq, 10), aes(x = reorder(word, -frequency), y = frequency, label = paste(round(pctSample, 2), "%", sep = ""))) + 
        geom_col() + 
        geom_label() +
        labs(x = "Tokens", y = "Frequency Count & Percentage of Sample") +
        annotate("text", label = paste("Total fourGrams Tokens with freq > ", lowfreq, ":", fourgramTotal,
                                       "\nTotal unique Tokens:", length(Terms(fourgramDTM))), 
                 x = 8, y = fourgramFreq$frequency[1]) + theme_bw() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) 

#------------------------------------------------------------------------
#      Calculating 4grams word frequency with highest frequencies without StopWords
#------------------------------------------------------------------------

fourgramDTMwoSW <- TermDocumentMatrix(corpusWOstopwords,control = list(tokenize = fourgramtokenizer))

lowfreq = 2
most_words <- findFreqTerms(fourgramDTMwoSW,lowfreq = lowfreq)

fourgramFreqwoSW <- rowSums(as.matrix(fourgramDTMwoSW[most_words,]))
fourgramTotal <- sum(fourgramFreqwoSW)
fourgramFreqwoSW <- data.frame(word=names(fourgramFreqwoSW),frequency=fourgramFreqwoSW, pctSample=100*fourgramFreqwoSW/fourgramTotal)
fourgramFreqwoSW <- arrange(fourgramFreqwoSW, desc(frequency))

ggplot(head(fourgramFreqwoSW, 10), aes(x = reorder(word, -frequency), y = frequency, label = paste(round(pctSample, 2), "%", sep = ""))) + 
        geom_col() + 
        geom_label() +
        labs(x = "Tokens", y = "Frequency Count & Percentage of Sample") +
        annotate("text", label = paste("Total fourGrams Tokens with freq > ", lowfreq, ":", fourgramTotal,
                                       "\nTotal unique Tokens:", length(Terms(fourgramDTMwoSW))), 
                 x = 8, y = fourgramFreqwoSW$frequency[1]) + theme_bw() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) 

#------------------------------------------------------------------------
#      Next Word Prediction
#------------------------------------------------------------------------

predictNext <- function(input, ngram) {
        
        
        inputSW <- removeWords(input, stopwords())
        inputSW <- removePunctuation(inputSW)
        inputSW <- stripWhitespace(inputSW)
        
        #just extracting the last words that we are going to work
        lastW <- tail(strsplit(input,split=" ")[[1]], ngram)
        lastW <- c(lastW, paste(lastW, collapse = " ")) #disposed in vector each word and its concatenation
        
        lastWwoSW <- tail(strsplit(inputSW,split=" ")[[1]], ngram)
        lastWwoSW <- c(lastWwoSW, paste(lastWwoSW, collapse = " "))
        
        whichDTM <- function(n) {
                switch(n,
                       "1" = bigramFreq,
                       "2" = trigramFreq,
                       "3" = fourgramFreq)
        }
        
        whichDTMwoSW <- function(n) {
                switch(n,
                       "1" = bigramFreqwoSW,
                       "2" = trigramFreqwoSW,
                       "3" = fourgramFreqwoSWwoSW)
        }
        
        ind <- grep(paste("^", lastW[ngram+1], " ", sep = ""), whichDTM(ngram)$word)
        if (length(ind) == 0) {
                result <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("frequency", "pctSample","token", "nextWord"))
        }else {
                result <- head(whichDTM(ngram)[ind,], 3)
                result$word <- as.character(result$word)
                result <- mutate(result, token = lastW[ngram+1])
                aux <- do.call(rbind, strsplit(as.character(result$word),' '))
                result <- mutate(result, nextWord = aux[,ngram+1])
                result <- select(result, -word)
        }
        
        indSW <- grep(paste("^", lastWwoSW[ngram+1], " ", sep = ""), whichDTMwoSW(ngram)$word)
        
        if (length(indSW) == 0) {
                resultSW <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("frequency", "pctSample","token", "nextWord"))
        }else {
                resultSW <- head(whichDTMwoSW(ngram)[indSW,], 3)
                resultSW$word <- as.character(resultSW$word)
                resultSW <- mutate(resultSW, token = lastWwoSW[ngram+1])
                aux <- do.call(rbind, strsplit(as.character(resultSW$word),' '))
                resultSW <- mutate(resultSW, nextWord = aux[,ngram+1])
                resultSW <- select(resultSW, -word)
        }
        
        return(bind_rows(result, resultSW))
        
}

#------------------------------------------------------------------------
#      quiz 2: Natural Language Processing I
#------------------------------------------------------------------------

Q1 <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
Q2 <- "You're the reason why I smile everyday. Can you follow me please? It would mean the"
Q3 <- "Hey sunshine, can you follow me and make me the"
Q4 <- "Very early observations on the Bills game: Offense still struggling but the"
Q5 <- "Go on a romantic date at the"
Q6 <- "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"
Q7 <- "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some"
Q8 <- "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"
Q9 <- "Be grateful for the good times and keep the faith during the"
Q10 <- "If this isn't the cutest thing you've ever seen, then you must be"

predictNext(Q1, 1)
predictNext(Q2, 1) # found in biGram
predictNext(Q3, 3)
predictNext(Q4, 3)
predictNext(Q5, 3)
predictNext(Q6, 3)
predictNext(Q7, 1) # found in biGram
predictNext(Q8, 3)
predictNext(Q9, 3)
predictNext(Q10, 3)


#------------------------------------------------------------------------
# exporting the corpus to use in Shiny App
#------------------------------------------------------------------------

writeCorpus(corpus, path = "C:/DataScienceProjects/CapstoneProject/DataScienceCapstone_WordPredictor/shinyapp/WordPredictor/smallCorpus")

writeLines(as.character(corpus), con="smallCorpus.txt")
