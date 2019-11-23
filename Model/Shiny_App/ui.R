library(shiny)
library(dplyr)
library(ngram)
library(tm)
library(stringr)
library(shinybusy)
library(shinythemes)
shinyUI(fluidPage(theme = shinytheme("flatly"),
        titlePanel(
                h1("Next Word Prediction Application", align = "center")
        ),
        sidebarLayout(
                sidebarPanel(
                        h4("This application attempts to predict the next word of a phrase."),
                        h4("To use the application, type a phrase in the text box below, and hit the 
                           submit button.  A spinner will indicate the application is running 
                           (approximately 2-5 seconds)."),
                        h4("Once the application is done running, you will see three predicted words 
                           in order of the application's confidence level."),  
                        textInput("text", "Input Phrase:", "write a phrase here"),
                        actionButton(
                                inputId = "submit_text",
                                label = "Submit"
                        ),
                        h5("If you want to try a different phrase after running the application, repeat 
                           the process of typing a phrase into the text box and hitting the submit 
                           button.  The spinner will pop up again until the application is done running, 
                           and the new prediction appears.")
                ),
                mainPanel(
                        textOutput("phrase1"),
                        tags$head(tags$style("#phrase1{color: black;
                                 font-size: 25px;
                                 }"
                                 )
                        ),
                        h2(""),
                        textOutput("pred1"),
                        tags$head(tags$style("#pred1{color: green;
                                 font-size: 25px;
                                 }"
                        )
                        ),
                        h2(""),
                        textOutput("pred2"),
                        tags$head(tags$style("#pred2{color: green;
                                 font-size: 25px;
                                 }"
                        )
                        ),
                        h2(""),
                        textOutput("pred3"),
                        tags$head(tags$style("#pred3{color: green;
                                 font-size: 25px;
                                 }"
                        )
                        ),
                        h3(""),
                        textOutput("phrase2"),
                        tags$head(tags$style("#phrase2{color: black;
                                 font-size: 20px;
                                 }"
                        )
                        ),
                        h4(""),
                        textOutput("phrase3"),
                        tags$head(tags$style("#phrase3{color: black;
                                 font-size: 15px;
                                 font-style: italic;
                                 }"
                        )
                        ),
                )
        )        
))