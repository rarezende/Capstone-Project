# ==================================================================== #
# Capstone Project 
# ==================================================================== #

library(ngram)
library(tm)
library(hashmap)


# ----------------------------------------------------------------
# Cleaning sentences and filtering of profanity
# ----------------------------------------------------------------

SentencePreprocessing <- function(inputText) {
    
    # Swap all sentence ends with code 'xwx'
    textSample <- gsub(pattern = ";|\\.|!|\\?", x = inputText, replacement = "xwx")
    
    # Split sentences by split code
    textSample <- unlist(strsplit(x = textSample, split="xwx", fixed = TRUE))
    
    # Remove all non-alpha text (numbers etc), but keeps apostrophes
    textSample <- gsub(pattern = "[^[:alpha:]']", x = textSample, replacement = " ")
    
    # Force all characters to lower case
    textSample <- tolower(textSample)
    
    # Remove profanity and other words we do not want to predict
    textSample <- removeWords(textSample, profanityFilter)

    # Remove leading and trailing whitespaces
    textSample <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", textSample)
    
    # Remove contiguous whitespaces
    textSample <- gsub(pattern = "\\s+", x = textSample, replacement = " ")
    
    return(textSample)
}    


# ----------------------------------------------------------------
# Construction of N-gram map
# ----------------------------------------------------------------

CreateNGramMap <- function(inputText, ngramSize, minFreq = 10) {
    
    # Filter for sentences that have more than ngramSize words
    idxValid <- sapply(strsplit(inputText, split=" "), function(x) length(x)) > ngramSize
    textSample <- inputText[idxValid]

    freqTable <- get.phrasetable(ngram(textSample, ngramSize + 1))
    freqTable <- subset(freqTable, freq > minFreq)
    
    looseWords <- strsplit(freqTable$ngrams, split=" ")
    
    if(ngramSize == 1) {
        freqTable$Key <- sapply(looseWords, function(x) x[1])
    } else if(ngramSize == 2) {
        freqTable$Key <- sapply(looseWords, function(x) paste(x[1:2], collapse = " "))
    } else if(ngramSize == 3) {
        freqTable$Key <- sapply(looseWords, function(x) paste(x[1:3], collapse = " "))
    } else {
        return()
    }
    
    freqTable$Value <- sapply(looseWords, function(x) x[length(x)])
    
    freqTable <- freqTable[order(freqTable$Key, -freqTable$freq), c("Key", "Value", "freq")]
    
    # ======================== REMOVE ========================
    # Global variables stored for later inspection
    if(ngramSize == 1) {
        gfreqTableUniGram <<- freqTable
    } else if(ngramSize == 2) {
        gfreqTableBiGram <<- freqTable
    } else if(ngramSize == 3) {
        gfreqTableTriGram <<- freqTable
    } 
    # ======================== REMOVE ========================
    
    # Keeps only the (Key, Value) pair with the highest frequency
    idxUnique <- !duplicated(freqTable$Key)    
    
    hashmap(freqTable$Key[idxUnique], freqTable$Value[idxUnique])
}

pt = proc.time()

nLines = 200000

con = file("./en_US/en_US.twitter.txt", "r") 
sampleTwitter = readLines(con, nLines, encoding = "UTF-8")
close(con) 

con = file("./en_US/en_US.news.txt", "r") 
sampleNews = readLines(con, nLines, encoding = "UTF-8")
close(con) 

con = file("./en_US/en_US.blogs.txt", "r") 
sampleBlogs = readLines(con, nLines, encoding = "UTF-8")
close(con) 

con = file("./ProfanityFilter.txt", "r") 
profanityFilter = readLines(con, -1, encoding = "UTF-8")
close(con) 

sampleTwitter = SentencePreprocessing(sampleTwitter)
sampleNews    = SentencePreprocessing(sampleNews)
sampleBlogs   = SentencePreprocessing(sampleBlogs)

textSample = c(sampleTwitter, sampleNews, sampleBlogs)

gfreqTableUniGram = c()
gfreqTableBiGram = c()
gfreqTableTriGram = c()

uniGramMap = CreateNGramMap(textSample, ngramSize = 1, minFreq = 30)
biGramMap = CreateNGramMap(textSample, ngramSize = 2, minFreq = 20)
triGramMap = CreateNGramMap(textSample, ngramSize = 3, minFreq = 10)


runningTime = proc.time() - pt

print(runningTime)





