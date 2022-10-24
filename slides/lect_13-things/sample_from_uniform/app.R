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


d <- tibble(a = runif(5000, 0, 200))

main_plot <- ggplot(d) +
  geom_histogram(aes(x = a)) +
  labs(x = "value", y = "count",
       title = "Histogram of data")

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Sample from distribution"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("samples",
                  "Number of samples:",
                  min = 1,
                  max = 500,
                  value = 200),

      sliderInput("size",
                  "Size of samples:",
                  min = 1,
                  max = 500,
                  value = 200),

    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      plotOutput("main_plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  R <- randomNumbers(n = 1, min = 0, max = 5000, col = 1)

  output$distPlot <- renderPlot({
    set.seed(R)
    # generate bins based on input$bins from ui.R
    d <- tibble(a = runif(5000, 0, 200))

    no_sample <- input$samples
    size_sample <- input$size
    means <- tibble(a = colMeans(replicate(no_sample,
                                sample(x = unlist(d),
                                       size = size_sample))))

    ggplot(means) +
      geom_density(aes(x = a)) +
      labs(x = "means", y = "count",
           title = "Density plot of sample mean",
           subtitle = "Number of samples, and sample size from input") +
      lims(x = c(70, 130))


  })


  output$main_plot <- renderPlot({
    set.seed(R)
    d <- tibble(a = runif(5000, 0, 200))

    ggplot(d) +
      geom_histogram(aes(x = a)) +
      labs(x = "value", y = "count",
           title = "Histogram of data")
  })

}

# Run the application
shinyApp(ui = ui, server = server)
