# ==================================================================== #
# Predictive Text Model Application
# Data Science Specialization - Capstone Project 
# ==================================================================== #

library(ngram)
library(tm)
library(hashmap)


# ----------------------------------------------------------------
# Prepare clean sentences and remove inappropriate words
# ----------------------------------------------------------------

SentencePreprocessing <- function(inputText, badWordsList) {
    
    # Swap all sentence ends with code 'xwx'
    textSample <- gsub(pattern = ";|\\.|!|\\?", x = inputText, replacement = "xwx")
    
    # Split sentences by split code
    textSample <- unlist(strsplit(x = textSample, split="xwx", fixed = TRUE))
    
    # Remove all non-alpha text (numbers etc), but keeps apostrophes
    textSample <- gsub(pattern = "[^[:alpha:]']", x = textSample, replacement = " ")
    
    # Force all characters to lower case
    textSample <- tolower(textSample)
    
    # Remove profanity and other words we do not want to predict
    textSample <- removeWords(textSample, badWordsList)

    # Remove leading and trailing whitespaces
    textSample <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", textSample)
    
    # Remove contiguous whitespaces
    textSample <- gsub(pattern = "\\s+", x = textSample, replacement = " ")
    
    return(textSample)
}    


# ----------------------------------------------------------------
# Construction of N-gram frequency table
# ----------------------------------------------------------------

CreateNGramFreqTable <- function(inputText, ngramSize, minFreq = 2) {
    
    # Filter for sentences that have more than ngramSize words
    idxValid <- sapply(strsplit(inputText, split=" "), function(x) length(x)) > ngramSize
    textSample <- inputText[idxValid]

    freqTable <- get.phrasetable(ngram(textSample, ngramSize + 1))
    freqTable <- subset(freqTable, freq >= minFreq)
    
    looseWords <- strsplit(freqTable$ngrams, split=" ")
    
    # Create the Key for the hashmap
    if(ngramSize == 1) {
        freqTable$Key <- sapply(looseWords, function(x) x[1])
    } 
    else if(ngramSize == 2) {
        freqTable$Key <- sapply(looseWords, function(x) paste(x[1:2], collapse = " "))
    } 
    else if(ngramSize == 3) {
        freqTable$Key <- sapply(looseWords, function(x) paste(x[1:3], collapse = " "))
    } 
    
    # Create the Value for the hashmap
    freqTable$Value <- sapply(looseWords, function(x) x[length(x)])
    
    nGramFreqTable <- freqTable[, c("Key", "Value", "freq")]
    
    return(nGramFreqTable)
}


# ----------------------------------------------------------------
# Prediction of next word using unigram, bigram and trigram maps
# ----------------------------------------------------------------

PredictNextWord <- function(inputText, nGramMapList) {
    
    uniGramMap <- nGramMapList$uniGramMap
    biGramMap <- nGramMapList$biGramMap
    triGramMap <- nGramMapList$triGramMap
    
    # Force all characters to lower case
    textSample <- tolower(inputText)
    
    # Remove leading and trailing whitespaces
    textSample <- gsub("(^[[:space:]]+|[[:space:]]+$)", replacement = "", textSample)
    
    # Remove contiguous whitespaces
    textSample <- gsub("\\s+", replacement = " ", textSample)
    
    looseWords <- unlist(strsplit(textSample, split=" "))
    nWords <- length(looseWords)
    
    if(nWords >= 3) {
        key <- paste(looseWords[(nWords-2):nWords], collapse = " ")
        if(triGramMap$has_key(key)){
            value <- triGramMap[[key]]
        } 
        else {
            key <- paste(looseWords[(nWords-1):nWords], collapse = " ")
            if(biGramMap$has_key(key)) {
                value <- biGramMap[[key]]
            } 
            else {
                key <- looseWords[nWords]
                if(uniGramMap$has_key(key)) {
                    value <- uniGramMap[[key]]
                } 
                else {
                    value <- "the"
                }
            }
        }
    } 
    else if(nWords == 2) {
        key <- paste(looseWords[1:2], collapse = " ")
        if(biGramMap$has_key(key)) {
            value <- biGramMap[[key]]
        } 
        else {
            key <- looseWords[nWords]
            if(uniGramMap$has_key(key)) {
                value <- uniGramMap[[key]]
            } 
            else {
                value <- "the"
            }
        }
    } 
    else if(nWords == 1) {
        key <- looseWords[nWords]
        if(uniGramMap$has_key(key)) {
            value <- uniGramMap[[key]]
        } 
        else {
            value <- "the"
        }
    } 
    else {
        value <- "the"
    }
    
    return(value)
}




