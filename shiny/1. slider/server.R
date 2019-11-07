

shinyServer(function(input, output) {
  output$text1 <- renderText({
    paste("You have selected", input$range[1])
  })

})
