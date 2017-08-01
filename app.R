# This is a small shiny app that displays an analog clock
# showing the current system time. The clock is updated
# twice a second, thus it always shows the correct time.

library(shiny)
library(tidyverse)

ui <- fluidPage(
  titlePanel("Clock"),
  mainPanel(
    plotOutput("clockPlot")
  )
)

server <- function(input, output) {
  autoInvalidate <- reactiveTimer(500)
  output$clockPlot <- renderPlot({
    autoInvalidate()
    data <- data.frame(a = 0:60, value = 96)
    data$value[data$a %% 5 == 0] <- 90
    data$hours <- NA
    data$hours[data$value == 90] <- c(12, 1:11, NA)
    hour <- as.numeric(format(Sys.time(), "%I")) * 5
    minute <- as.numeric(format(Sys.time(), "%M"))
    second <- as.numeric(format(Sys.time(), "%S"))
    ggplot(data, aes(x = a, y = value, xend = a, yend = 100)) + 
      geom_segment() + 
      geom_text(aes(label = hours), nudge_y = -10) +
      geom_segment(aes(x = hour + minute / 60 * 5, y = 0, xend = hour + minute / 60 * 5, yend = 50), size = 2) +
      geom_segment(aes(x = minute + second / 60, y = 0, xend = minute + second / 60, yend = 75), size = 1) +
      geom_segment(aes(x = second, y = 0, xend = second, yend = 75), size = 0.3) +
      coord_polar() + 
      ylim(0, 100) + 
      theme(axis.title = element_blank(),
            axis.line.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_blank(),
            panel.background = element_blank())
  })
}

shinyApp(ui = ui, server = server)
