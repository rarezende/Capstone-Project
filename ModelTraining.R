# ==================================================================== #
# Predictive Text Model Application
# Data Science Specialization - Capstone Project 
# ==================================================================== #

setwd("C:/Users/Rodrigo/Data-Science/Capstone-Project")
source("./PredictiveTextModel/PredictiveTextModel.R")


pt = proc.time()

nLines = 10000

con = file("./en_US/en_US.twitter.txt", "r") 
sampleTwitter = readLines(con, nLines, encoding = "UTF-8")
close(con) 

con = file("./en_US/en_US.news.txt", "r") 
sampleNews = readLines(con, nLines, encoding = "UTF-8")
close(con) 

con = file("./en_US/en_US.blogs.txt", "r") 
sampleBlogs = readLines(con, nLines, encoding = "UTF-8")
close(con) 

con = file("./BadWordsList.txt", "r") 
badWordsList = readLines(con, -1, encoding = "UTF-8")
close(con) 

textSample = c(sampleTwitter, sampleNews, sampleBlogs)

textSample = SentencePreprocessing(textSample, badWordsList)

gUniGramMapTable = c()
gBiGramMapTable = c()
gTriGramMapTable = c()

nGramMapTable = CreateNGramMapTable(textSample, ngramSize = 1, minFreq = 5)
write.csv(nGramMapTable, "UniGramMapTable.csv")
uniGramMap = hashmap(nGramMapTable$Key, nGramMapTable$Value)

nGramMapTable = CreateNGramMapTable(textSample, ngramSize = 2, minFreq = 4)
write.csv(nGramMapTable, "BiGramMapTable.csv")
biGramMap = hashmap(nGramMapTable$Key, nGramMapTable$Value)

nGramMapTable = CreateNGramMapTable(textSample, ngramSize = 3, minFreq = 3)
write.csv(nGramMapTable, "TriGramMapTable.csv")
triGramMap = hashmap(nGramMapTable$Key, nGramMapTable$Value)

runningTime = proc.time() - pt

print(runningTime)




