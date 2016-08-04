# -------------------------------------------------------------------- #
# Capstone Project 
# -------------------------------------------------------------------- #

library(ngram)
library(tm)
library(hashmap)

# ----------------------------------------------------------------
# Cleaning and preprocessing of raw text
# ----------------------------------------------------------------

SentencePreprocessing <- function(inputText) {
    
    # Swap all sentence ends with code 'xwx'
    textSample <- gsub(pattern = ";|\\.|!|\\?", x = inputText, replacement = "xwx")
    
    # Split sentences by split code
    textSample <- unlist(strsplit(x = textSample, split="xwx", fixed = TRUE))
    
    # Remove all non-alpha text (numbers etc), but keeps apostrophes
    textSample <- gsub(pattern = "[^[:alpha:]']", x = textSample, replacement = " ")
    
    # Remove leading and trailing whitespaces
    textSample <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", textSample)
    
    # Remove contiguous whitespaces
    textSample <- gsub(pattern = "\\s+", x = textSample, replacement = " ")
    
    # Force all characters to lower case
    textSample <- tolower(textSample)
    
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
    
    gfreqTable <<- freqTable
    
    # Keeps only the (Key, Value) pair with the highest frequency
    idxUnique <- !duplicated(freqTable$Key)    
    
    hashmap(freqTable$Key[idxUnique], freqTable$Value[idxUnique])
}

pt = proc.time()

nLines = 10000

con = file("./en_US/en_US.twitter.txt", "r") 
sampleTwitter = readLines(con, nLines)
close(con) 

con = file("./en_US/en_US.news.txt", "r") 
sampleNews = readLines(con, nLines)
close(con) 

con = file("./en_US/en_US.blogs.txt", "r") 
sampleBlogs = readLines(con, nLines)
close(con) 

sampleTwitter = SentencePreprocessing(sampleTwitter)
sampleNews    = SentencePreprocessing(sampleNews)
sampleBlogs   = SentencePreprocessing(sampleBlogs)

textSample = c(sampleTwitter, sampleNews, sampleBlogs)

gfreqTable = c()

uniGramMap = CreateNGramMap(textSample, ngramSize = 1, minFreq = 10)
#biGramMap = CreateNGramMap(textSample, ngramSize = 2, minFreq = 10)
#triGramMap = CreateNGramMap(textSample, ngramSize = 3, minFreq = 10)


runningTime = proc.time() - pt

print(runningTime)
