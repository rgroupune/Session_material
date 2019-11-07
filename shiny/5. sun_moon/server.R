source('helpers.R')
# Define server logic for slider examples
function(input, output) {
  
  d.per.m <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  today <- Sys.Date()
  output$plot <- renderPlot({
    month <- as.numeric(input$month[1]):as.numeric(input$month[2])
    par(mfrow = c(length(month), 1), mar = c(0, 0, 0, 0), oma = c(3, 2, 2, 1), bty = "o", tcl = -0.2, las = 1)
    for(m in month) {
      at <-  expand.grid(hour = 0:23, day = 1:d.per.m[m])
      pos.sun  <- sunpos(ISOdate(format(today, '%Y'), m, at$day, at$hour), lat = input$lat, lon = input$lon, TZ = as.numeric(input$TZ))
      pos.moon <- moonpos(ISOdate(format(today, '%Y'), m, at$day, at$hour), lat = input$lat, lon = input$lon, TZ = as.numeric(input$TZ))
      plot(1, type = 'n', xlim = range(pos.sun$date[1], pos.sun$date[1]+(31*24*60*60)), ylim = c(0, 90), axes = F, xlab = "", ylab = "")
      if('sun' %in%input$object){lines(pos.sun$date, pos.sun$elevation, lwd = 2,  col = "gold")}
      if('moon' %in%input$object){lines(pos.moon$date, pos.moon$elevation, lwd = 2, col = "gray")}

      mtext(month.abb[m], side = 2, xpd = NA, line = 0, col = "gray", font = 2, las = 3)
      abline(v = ISOdate(format(today, '%Y'), m, 1:31, 0), col = "gray", lty = 3, xpd = T)
      abline(v = ISOdate(format(today, '%Y'), m, 1:4*7, 0), col = "gray", lty = 1, xpd = T)
      rect(pos.sun$date[1], -45,  pos.sun$date[1]+(31*24*60*60), 0, col = "white", lty = 1, xpd = F, border = NA)
      segments(pos.sun$date[1], 0,  pos.sun$date[1]+(31*24*60*60), 0, col = "gray", lty = 1, xpd = F)
      axis(2, at = c(0, 30, 60), labels = c("Horizon", "30º", "60º"), cex.axis = 0.6, line = -2)
    }
    axis(1, at =  ISOdate(format(today, '%Y'), m, c(1, 10, 20, 30), 12), label = format(ISOdate(format(today, '%Y'), m, c(1, 10, 20, 30), 12), "%d"))
    mtext(side = 1, text = 'Day of the month', line = 2)
  })
}