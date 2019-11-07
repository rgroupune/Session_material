library(fields)

shinyServer(
  function(input, output) {
    output$map <- renderPlot({
      par(las = 1, bty = 'n')
      surface(Krig(ChicagoO3$x, ChicagoO3$y, theta=input$theta), type="C") # look at the surface
      if(input$points){
        points(ChicagoO3$x, cex = 1+(ChicagoO3$y - min(ChicagoO3$y))/max(ChicagoO3$y), pch = 16)
      }
    })
  }
)
