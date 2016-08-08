# ----------------------------------------------------------------------
# Server code for application Predictive Text Model
# ----------------------------------------------------------------------

library(shiny)
source("../Functions.R")

nGramMapList <- list()
    
nGramMapTable = read.csv("UniGramMapTable.csv", row.names = 1, stringsAsFactors = FALSE)
nGramMapList$uniGramMap = hashmap(nGramMapTable$Key, nGramMapTable$Value)

nGramMapTable = read.csv("BiGramMapTable.csv", row.names = 1, stringsAsFactors = FALSE)
nGramMapList$biGramMap = hashmap(nGramMapTable$Key, nGramMapTable$Value)

nGramMapTable = read.csv("TriGramMapTable.csv", row.names = 1, stringsAsFactors = FALSE)
nGramMapList$triGramMap = hashmap(nGramMapTable$Key, nGramMapTable$Value)

shinyServer(function(input, output) {
    
    output$prediction <- renderPrint({PredictNextWord(input$inputText, nGramMapList)})
})


