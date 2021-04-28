#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Typing Prediction"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            textInput(inputId = "text", 
                      h3("Type here Please:")),
            
            sliderInput(inputId = "number",
                        h3("Number of Words You Want:"),
                        min = 1,
                        max = 10,
                        value = 6)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            h3("Predicting words"),
            textOutput(outputId = "put")
            )

        )
    )
)
