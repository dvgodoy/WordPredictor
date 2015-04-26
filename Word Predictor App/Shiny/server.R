library(shiny)

load('data/PredictorEnv.RData', envir = .GlobalEnv)

shinyServer(function(input, output) {
  
  dataInput <- reactive(Prediction(input$entry))
  
  output$n1 <- renderText({
    paste0("#1:", input$entry, dataInput()[1])
  })
  output$n2 <- renderText({
    paste0("#2:", input$entry, dataInput()[2])
  })
  output$n3 <- renderText({
    paste0("#3:", input$entry, dataInput()[3])
  })
  output$n4 <- renderText({
    paste0("#4:", input$entry, dataInput()[4])
  })
  output$n5 <- renderText({
    paste0("#5:", input$entry, dataInput()[5])
  })
  
  output$text <- renderText({
    dataInput()
  })
  output$predictor <- renderText({
    input$entry
  })
  
})
