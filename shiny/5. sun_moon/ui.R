library(shiny)

TZs <- c('UTC−12:00'=-12, 'UTC−11:00'=-11, 'UTC−10:00'=-10, 'UTC−09:30'=-9.5, 'UTC−09:00'=-9, 'UTC−08:00'=-8, 'UTC−07:00'=-7, 'UTC−06:00'=-6, 'UTC−05:00'=-5, 'UTC−04:00'=-4, 'UTC−03:00'=-3, 'UTC−02:00'=-2, 'UTC−01:00'=-1, 'UTC±00:00'=0, 'UTC+01:00'=1, 'UTC+02:00'=2, 'UTC+03:00'=3, 'UTC+04:00'=4, 'UTC+04:30'=4.5, 'UTC+05:00'=5, 'UTC+05:30'=5.5, 'UTC+06:00'=6, 'UTC+06:30'=6.5, 'UTC+07:00'=7, 'UTC+08:00'=8, 'UTC+08:30'=8.5, 'UTC+09:00'=9, 'UTC+09:30'=9.5, 'UTC+10:00'=10, 'UTC+11:00'=11, 'UTC+12:00'=12, 'UTC+13:00'=13, 'UTC+14:00'=14)

# Define UI for plotting sun and moon positions
fluidPage(

  #  Application title
  titlePanel("Plot hourly elevations above the horizon of Sun and Moon from any point on earth, for any day of the year."),

  sidebarLayout(
    sidebarPanel(
      # Input the Longitude
      sliderInput('lat', 'Latitude (use negative values for the southern hemisphere:', min = -90, max = 90, value = 0),
      sliderInput('lon', 'Longitude (use negative values for the western hemisphere:', min = -180, max = 180, value = 0),
      selectInput('TZ', 'Time zone:', choices = TZs, multiple = F, selected = 0),
      checkboxGroupInput("object", "Celestial objects to show:", c("Moon" = "moon", "Sun" = "sun"), inline = T, selected = c('moon', 'sun')),
      sliderInput('month', 'Months to show', min = 1, max = 12, value = c(1, 12), round = T, step = 1)),

  mainPanel(
    plotOutput("plot")
  )
  )
)
