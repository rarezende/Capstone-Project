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

