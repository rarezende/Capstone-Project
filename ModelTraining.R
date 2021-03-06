# ==================================================================== #
# Predictive Text Model Application
# Data Science Specialization - Capstone Project 
# ==================================================================== #

library(dplyr, warn.conflicts = FALSE)

setwd("C:/Users/Rodrigo/Data-Science/Capstone-Project")
source("./PredictiveTextModel/PredictiveTextModel.R")

startTime = proc.time()

N_FILE_LINES = -1L
BATCH_SIZE = 100000

con = file("./BadWordsList.txt", "r") 
badWordsList = readLines(con, n = -1L, encoding = "UTF-8")
close(con) 

fileList = list("./en_US/en_US.twitter.txt",
                "./en_US/en_US.news.txt",
                "./en_US/en_US.blogs.txt")

unigramParams = list()
unigramParams$ngramSize = 1
unigramParams$minFreq = 2
unigramParams$fileName = "UniGramMapTable.csv"

bigramParams = list()
bigramParams$ngramSize = 2
bigramParams$minFreq = 2
bigramParams$fileName = "BiGramMapTable.csv"

trigramParams = list()
trigramParams$ngramSize = 3
trigramParams$minFreq = 2
trigramParams$fileName = "TriGramMapTable.csv"

ngramParamList = list(unigramParams, bigramParams, trigramParams)

for(ngramParam in ngramParamList) {

    print(sprintf("Creating nGramMapTable file: %s", ngramParam$fileName))
    
    nGramFreqTable = c()
    
    for(fileName in fileList) {
        
        print(sprintf("  Processing training file: %s", fileName))
        
        con = file(fileName, "r") 
        textFile = readLines(con, n = N_FILE_LINES, encoding = "UTF-8", skipNul = TRUE)
        close(con) 
        
        beginLine = 1
        endLine = beginLine + BATCH_SIZE
        
        while (endLine <= length(textFile)) {
            
            textSample = textFile[beginLine:endLine]
            
            textSample = SentencePreprocessing(textSample, badWordsList)
            
            freqTableSample = CreateNGramFreqTable(textSample, 
                                                   ngramParam$ngramSize,
                                                   ngramParam$minFreq)
            
            nGramFreqTable = rbind(nGramFreqTable, freqTableSample)

            print(sprintf("    File processed up to line: %d", endLine))
            
            if (endLine == length(textFile)) {
                break;
            }
            else {
                beginLine = endLine + 1
                endLine = min(beginLine + BATCH_SIZE, length(textFile))
            }
        }
    }

    # Aggregate ngram count from all batches
    nGramMapTable = nGramFreqTable %>%
                    group_by(Key, Value) %>%
                    summarize(freq = sum(freq)) %>%
                    top_n(1, freq)
    
    # Remove duplicates generated by top_n() for ties in freq
    nGramMapTable = nGramMapTable[!duplicated(nGramMapTable$Key),]
    
    write.csv(nGramMapTable, ngramParam$fileName)
}

runningTime = proc.time() - startTime

print(runningTime)





