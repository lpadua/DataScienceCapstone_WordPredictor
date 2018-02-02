#---------------------------------------------------------------------------------------
# filename: LoadVCorpus
# author: Luis Padua
# summary:
#       load the Vcorpus previously formatted where punctuation, numbers, spaces 
#       and case-sensitive to lower
#       Following the Termdocumentmatrix of 1-gram, 2-gram, 3-gram and 4-gram
#---------------------------------------------------------------------------------------

corpus <- VCorpus(DirSource("./smallCorpus"),
                  readerControl = list(language = "en-US"))

bigramTokenizer <- function(x) {
        unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE) }
trigramTokenizer <- function(x) {
        unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE) }
fourgramTokenizer <- function(x) {
        unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = FALSE) }

unigramDTM <- TermDocumentMatrix(corpus, control = list(wordLengths=c(1, Inf)))
bigramDTM <- TermDocumentMatrix(corpus,control = list(tokenize = bigramTokenizer))
trigramDTM <- TermDocumentMatrix(corpus,control = list(tokenize = trigramTokenizer))
fourgramDTM <- TermDocumentMatrix(corpus,control = list(tokenize = fourgramTokenizer))

remove(corpus)

