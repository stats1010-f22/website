#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyverse)

make_dbinom_plot <- function(size){
  outcomes <- (1:size)
  probabilites <- dbinom(size  = size, prob = 0.5, x = outcomes)

  d <- tibble(outcomes, probabilites)

  print(plot(outcomes, probabilites))
}



# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Binomial limits to normal"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("trials",
                  "Number of trials:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$distPlot <- renderPlot({
    make_dbinom_plot(input$trials)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
