library(shiny)
require(markdown)
require(stringi)
require(sqldf)
require(fastmatch)

shinyUI(
  fluidPage(h1("SwiftKey's Word Predictor"), 
                      sidebarLayout(
                        sidebarPanel(width=4,
                                     textInput("entry", 
                                               "This app will predict the word you are likely typing as well as the most likely next word. Please type your input here:",
                                               "What a wonderful "),
                                     hr(),
                                     helpText(h5("Help Instructions:")),
                                     helpText("Before start typing, please allow a few seconds for the initial sentence to appear on the right side.",style="color:blue"),
                                     helpText("Please check the prediction results by using the dashboard on the right side."),
                                     helpText("The app works as follows: "),
                                     helpText("1. Start typing your sentence in the text field.", style="color:gray"),
                                     helpText("2. While you are typing the App will make a single word prediction for the word you are currently typing.", style="color:gray"),
                                     helpText("3. Whenever you type a space the App will make a prediction for the top 5 most likely words to follow.", style="color:gray"),
                                     hr(),
                                     h6("This App was built for:"),
                                     a("Data Science Capstone (SwiftKey)", href="https://www.coursera.org/course/dsscapstone"),
                                     p("class started on March 9th 2015"),
                                     p('The source codes', 
                                       code("server.R"), code("ui.R"), code("PredictionSource.R"),'can be found at', a("Word Predictor.",href="https://github.com/dvgodoy/WordPredictor")),
                                     hr(),
                                     h6("For more information about Daniel Voigt Godoy:"),
                                     a(img(src = "Linkedin-256.png", height = 30, width = 30),href="https://br.linkedin.com/in/dvgodoy"),
                                     a(img(src = "Gmail-48.png", height = 30, width = 30),href="mailto: dvgodoy@gmail.com"),
                                     a(img(src = "Github-256.png", height = 30, width = 30),href="https://github.com/dvgodoy")                                     
                                     ),
                        mainPanel(
                                 h3("Word Prediction"),
                                 hr(),
                                 h5('The sentence you just typed:'),                             
                                 wellPanel(span(h4(textOutput('predictor')),style = "color:green")),
                                 hr(),
                                 h5('Single Word Prediction:'),
                                 wellPanel(span(h4(textOutput('n1')),style = "color:red")),
                                 hr(),
                                 conditionalPanel(condition = "input.entry.charAt(input.entry.length-1) == ' '",
                                           h5('Other Possible Single Word Predictions:'),                                                  
                                           span(h5(textOutput('n2')),style = "color:blue"),
                                           span(h5(textOutput('n3')),style = "color:blue"),
                                           span(h5(textOutput('n4')),style = "color:blue"),
                                           span(h5(textOutput('n5')),style = "color:blue"))
                        )
                        )
  )
)