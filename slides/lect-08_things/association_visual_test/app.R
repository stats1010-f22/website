#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Association visual test"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(

            actionButton("go", "Go",
                         width = "150px",
                         style = "background-color:red")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("assoc_test_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    D <- eventReactive(input$go, {
      diamonds %>% # filtered data
        slice_sample(n = nrow(.)) %>% # random sample rows
        pull(carat) %>% # take out the variable carat
        bind_cols(., diamonds$price) # price in the same order and bound to carat in different order
    })

    output$assoc_test_plot <- renderPlot({
      D() %>%
        ggplot() + # into ggplot
        geom_point(aes(y = ...2, x = ...1)) + # using the new names
        labs(title = "Simulated association test",
             x = "Weight of diamond in carat",
             y = "Price of diamonds in US$")
    })

}

# Run the application
shinyApp(ui = ui, server = server)
