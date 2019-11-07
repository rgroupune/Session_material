shinyUI(fluidPage(
  titlePanel("Demonstrate server-user interface communication"),

  sidebarLayout(
    sidebarPanel(
      sliderInput("range",
                  label = "Range of interest:",
                  min = 0, max = 100, value = 57)
      ),

    mainPanel(
      textOutput("text1")
    )
  )
))
