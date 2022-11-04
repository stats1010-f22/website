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
  titlePanel("Sample from Bernoulli distribution"),

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

      sliderInput("alpha",
                  "Level of confidence (1 - alpha):",
                  min = 0,
                  max = 1,
                  value = 0.5)

    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("mainPlot"),
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  R <- randomNumbers(n = 1, min = 0, max = 5000, col = 1)



  output$mainPlot <- renderPlot({

    p_hat <- tibble(a = input$p)

    conf_level <- (1 - input$alpha)/2


    # Calculate the critical z-score
    z <- qnorm(conf_level)

    # Compute the CI
    CI <- tibble(bounds = p_hat$a + c(-1,1)*z*
                   sqrt(p_hat$a*(1-p_hat$a)/input$size))

    ggplot() +
      geom_point(data = p_hat, aes(x = a, y = 0), color = "red", size = 5) +
      geom_vline(xintercept = CI$bounds, color = "blue") +
      labs(x = "estimate and CI for estimate", y = "",
           title = "Plot of CI",
           subtitle = "Size of sample, proportion, and alpha value from input") +
      lims(x = c(0, 1))
  })


  output$distPlot <- renderPlot({
    set.seed(R)

    ## 2000 samples from bernoulli trial each of size size with
    ## probability from input

    means <- tibble(a = colMeans(replicate(2000,
                                           rbinom(n = input$size,
                                                  size = 1, prob = input$p))))
    ggplot(means) +
      geom_density(aes(x = a)) +
      labs(x = "possible values of p", y = "count",
           title = "Plot of mean from normal distribution",
           subtitle = "Size of samples, and proportion from input") +
      lims(x = c(0, 1))
  })

}

# Run the application
shinyApp(ui = ui, server = server)


