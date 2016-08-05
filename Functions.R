# ==================================================================== #
# Capstone Project 
# ==================================================================== #

library(ngram)
library(tm)
library(hashmap)


# ----------------------------------------------------------------
# Cleaning of the text and filtering of profanity
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
# Construction of N-gram map table
# ----------------------------------------------------------------

CreateNGramMapTable <- function(inputText, ngramSize, minFreq = 1) {
    
    # Filter for sentences that have more than ngramSize words
    idxValid <- sapply(strsplit(inputText, split=" "), function(x) length(x)) > ngramSize
    textSample <- inputText[idxValid]

    freqTable <- get.phrasetable(ngram(textSample, ngramSize + 1))
    freqTable <- subset(freqTable, freq > minFreq)
    
    looseWords <- strsplit(freqTable$ngrams, split=" ")
    
    if(ngramSize == 1) {
        freqTable$Key <- sapply(looseWords, function(x) x[1])
    } 
    else if(ngramSize == 2) {
        freqTable$Key <- sapply(looseWords, function(x) paste(x[1:2], collapse = " "))
    } 
    else if(ngramSize == 3) {
        freqTable$Key <- sapply(looseWords, function(x) paste(x[1:3], collapse = " "))
    } 
    
    freqTable$Value <- sapply(looseWords, function(x) x[length(x)])
    
    nGramMapTable <- freqTable[order(freqTable$Key, -freqTable$freq), c("Key", "Value", "freq")]
    
    # ======================== REMOVE ========================
    # Global variables stored for later inspection
    if(ngramSize == 1) {
        gUniGramMapTable <<- nGramMapTable
    } 
    else if(ngramSize == 2) {
        gBiGramMapTable <<- nGramMapTable
    } 
    else if(ngramSize == 3) {
        gTriGramMapTable <<- nGramMapTable
    } 
    # ======================== REMOVE ========================
    
    # Keeps only the (Key, Value) pair with the highest frequency
    idxUnique <- !duplicated(nGramMapTable$Key)   
    
    return(nGramMapTable[idxUnique,])
}

PredictNextWord <- function(inputText) {
    
    # Force all characters to lower case
    textSample <- tolower(inputText)
    
    # Remove leading and trailing whitespaces
    textSample <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", textSample)
    
    # Remove contiguous whitespaces
    textSample <- gsub(pattern = "\\s+", x = textSample, replacement = " ")

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
                    value <- "Perdeu praiboy"
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
                value <- "Perdeu praiboy"
            }
        }
    } 
    else if(nWords == 1) {
        key <- looseWords[nWords]
        if(uniGramMap$has_key(key)) {
            value <- uniGramMap[[key]]
        } 
        else {
            value <- "Perdeu praiboy"
        }
    } 
    else {
        value <- "Perdeu praiboy"
    }

    return(value)
}




