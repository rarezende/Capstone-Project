# ----------------------------------------------------------------------
# User interface code for application Predictive Text Model
# ----------------------------------------------------------------------

library(shiny)

shinyUI(pageWithSidebar(
    # Application title
    headerPanel("Predictive Text Model"),
    
    sidebarPanel(
        textInput(inputId="inputText", label = "Text Input"),
        submitButton('Submit'),
        p("")
    ),
    mainPanel(
        h3('The predicted next word is:'),
        verbatimTextOutput("prediction")
    )
))
