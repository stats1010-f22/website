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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Chi-Squared distribution"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("df",
                        "Number of degrees of freedom:",
                        min = 1,
                        max = 50,
                        value = 5)
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

    ggplot() +
      geom_function(aes(colour = "chi-square, df from slider"), fun = dchisq,
                    args = list(df = input$df)) +
      lims(x = c(0, 50))
})
}

# Run the application
shinyApp(ui = ui, server = server)
