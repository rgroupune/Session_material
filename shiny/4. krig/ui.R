shinyUI(fluidPage(
  titlePanel("Spatial interpolation"),
  
  sidebarLayout(
    sidebarPanel(
      
      tags$div(tags$p("Interpolate values between measured ones. Increasing", code("theta"), "increases the flexibility and detail of the interpolation")),
      
      sliderInput("theta", 
                  label = "Theta:",
                  min = 1, max = 20, value = 10,
                  animate = animationOptions(interval=200, loop=T)),
      checkboxInput("points",
                    label = "Show points of measurement?", value = T)
    ),
    mainPanel(
      plotOutput("map")
    ) 
  )
))