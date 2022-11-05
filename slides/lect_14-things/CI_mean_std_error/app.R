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
                  value = 200)

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

    ## 2000 samples from normal distribution with
    ## size, mean, and sd from input

    means <- tibble(a = colMeans(
      replicate(2000, rnorm(n = input$size,
                            mean = input$m,sd = input$sd))))

    ggplot(means) +
      geom_density(aes(x = a)) +
      labs(x = "possible mean values", y = "likeliness of possible mean values",
           title = "Plot of 2000 means from normal distribution",
           subtitle = "Size of samples, mean, and sd from input") +
      lims(x = c(0, 100))
  })

}

# Run the application
shinyApp(ui = ui, server = server)


