# ==================================================================== #
# Capstone Project 
# ==================================================================== #

source("./CapstoneProject.R")

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