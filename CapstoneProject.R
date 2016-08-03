# -------------------------------------------------------------------- #
# Capstone Project 
# -------------------------------------------------------------------- #

library(ngram)
library(tm)
library(hashmap)

SentencePreprocessing <- function(textSample) {
    
    # Swap all sentence ends with code 'xwx'
    textSample <- gsub(pattern = ";|\\.|!|\\?", x = textSample, replacement = "xwx")
    
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

GetFirstWord <- function(x) {
    x[1]
}

GetFirstTwoWords <- function(x) {
    paste(x[1:2], collapse = " ")
}

GetFirstThreeWords <- function(x) {
    paste(x[1:3], collapse = " ")
}

GetLastWord <- function(x) {
    x[length(x)]
}


pt = proc.time()

nLines = 20000

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

# Filter for sentences that have at least ngram_size words or, equivalently,
# (ngram_size - 1) whitespaces, since we have clean, trimmed sentences
ngram_size = 3
textSample = textSample[sapply(gregexpr("\\s", textSample), length) >= (ngram_size - 1)]

freqTable = get.phrasetable(ngram(textSample, ngram_size))
freqTable = subset(freqTable, freq > 20)

triGrams = strsplit(x = freqTable$ngrams, split=" ")
freqTable$BiGrams  = sapply(triGrams, GetFirstTwoWords)
freqTable$UniGrams = sapply(triGrams, GetLastWord)

freqTable = freqTable[order(freqTable$BiGrams, -freqTable$freq), c("BiGrams", "UniGrams", "freq")]

indUnique = !duplicated(freqTable$BiGrams)    

bigramMap = hashmap(freqTable$BiGrams[indUnique], freqTable$UniGrams[indUnique])

runningTime = proc.time() - pt

print(runningTime)
