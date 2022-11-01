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
library(random)


# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Sample from binomial distribution"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("p",
                  "Proportion:",
                  min = 0,
                  max = 1,
                  value = 0.5),

      sliderInput("size",
                  "Size of samples:",
                  min = 1,
                  max = 500,
                  value = 200),

    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  R <- randomNumbers(n = 1, min = 0, max = 5000, col = 1)

  output$distPlot <- renderPlot({
    set.seed(R)

    ## 2000 samples from bernoulli trial each of size size with
    ## probability from input
    means <- tibble(a = colMeans(replicate(2000,
                          rbinom(n = input$size, size = 1, prob = input$p))))

    ggplot(means) +
      geom_density(aes(x = a)) +
      labs(x = "possible values of p", y = "count",
           title = "Plot of proportion from 2000 Bernoulli trials",
           subtitle = "Size of samples, and proportion from input") +
      lims(x = c(0, 1))
  })

}

# Run the application
shinyApp(ui = ui, server = server)
