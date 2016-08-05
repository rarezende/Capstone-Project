# ----------------------------------------------------------------------
# Server code for application Predictive Text Model
# ----------------------------------------------------------------------

library(shiny)
source("../Functions.R")

nGramMapTable = read.csv("UniGramMapTable.csv")
uniGramMap = hashmap(nGramMapTable$Key, nGramMapTable$Value)

nGramMapTable = read.csv("BiGramMapTable.csv")
biGramMap = hashmap(nGramMapTable$Key, nGramMapTable$Value)

nGramMapTable = read.csv("TriGramMapTable.csv")
triGramMap = hashmap(nGramMapTable$Key, nGramMapTable$Value)


shinyServer(function(input, output) {
    
    output$prediction <- renderPrint({PredictNextWord(input$inputText)})
})
