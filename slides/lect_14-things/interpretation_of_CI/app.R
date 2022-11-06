#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/aa
#

library(shiny)
library(tidyverse)
library(random)


# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Sample from Normal distribution"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("m",
                  "Mean:",
                  min = 0,
                  max = 100,
                  value = 50),

      sliderInput("sd",
                  "Standard deviation:",
                  min = 0,
                  max = 40,
                  value = 5),

      sliderInput("size",
                  "Size of samples:",
                  min = 1,
                  max = 500,
                  value = 200),

      sliderInput("alpha",
                  "Level of confidence (1 - alpha):",
                  min = 0,
                  max = 1,
                  value = 0.5)

    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("mainPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  R <- randomNumbers(n = 1, min = 0, max = 5000, col = 1)


  output$mainPlot <- renderPlot({

    set.seed(R)

    ## 2000 samples from normal distribution with
    ## size, mean, and sd from input
    means <- tibble(a = colMeans(
      replicate(2000, rnorm(n = input$size,
                            mean = input$m,sd = input$sd))))


    mhat <- tibble(a = input$m)

    conf_level <- (1 - input$alpha)/2


    # Calculate the critical z-score
    t <- qt(conf_level, df = (input$size - 1))

    # Compute the CI
    CI <- tibble(bounds = means + c(-1,1)*t*input$sd*
                   sqrt(1/input$size))

    ### MAKE INDICATOR TO SEE IF MEAN INSIDE CI


    ggplot() +
      geom_vline(xintercept = CI$bounds, color = "blue") +
      geom_segment(intercept = CI$bounds, color = "blue") +
      labs(x = "Estimate (red dot) and CI (blue lines) for estimate", y = "",
           title = "Plot of CI for mean",
           subtitle = "Size of sample, mean, st dev, and alpha value from input") +
      lims(x = c(0, 100))
  })
}

# Run the application
shinyApp(ui = ui, server = server)


