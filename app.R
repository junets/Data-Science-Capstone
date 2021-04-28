#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
bigram <- readRDS(url("https://raw.githubusercontent.com/junets/Data-Science-Capstone/main/two.rds"))
trigram <- readRDS(url("https://raw.githubusercontent.com/junets/Data-Science-Capstone/main/three.rds"))
quardgram <- readRDS(url("https://raw.githubusercontent.com/junets/Data-Science-Capstone/main/four.rds"))
fivgram <- readRDS(url("https://raw.githubusercontent.com/junets/Data-Science-Capstone/main/five.rds"))
sixgram <- readRDS(url("https://raw.githubusercontent.com/junets/Data-Science-Capstone/main/six.rds"))
library(tm)
nextword <- function(x, n = 40){
    x <- tolower(x)
    x <- removeNumbers(x)
    x <- removePunctuation(x)
    x <- stripWhitespace(x)
    x <- unlist(strsplit(x, ' '))
    num <- length(x)
    
    two <- function(x){
        result2 <- subset(bigram, word1 == x[num])$word2[1:n]
        result2 <- result2[!is.na(result2)]
        if(length(result2) == 0){result2 <- NULL}
        result2
    }
    
    three <- function(x){
        result3 <- subset(trigram, word1 == x[num-1] &
                              word2 == x[num])$word3[1:n]
        result3 <- result3[!is.na(result3)]
        if(length(result3) == 0){result3 <- NULL}
        result3
    }
    four <- function(x){
        result4 <- subset(quardgram, word1 == x[num-2] &
                              word2 == x[num-1] &
                              word3 == x[num])$word4[1:n]
        result4 <- result4[!is.na(result4)]
        if(length(result4) == 0){result4 <- NULL}
        result4
    }
    
    five <- function(x){
        result5 <- subset(fivgram, word1 == x[num-3] &
                              word2 == x[num-2] &
                              word3 == x[num-1] &
                              word4 == x[num])$word4[1:n]
        result5 <- result5[!is.na(result5)]
        if(length(result5) == 0){result5 <- NULL}
        result5
    }
    
    six <- function(x){
        result6 <- subset(sixgram, word1 == x[num-4] &
                              word2 == x[num-3] &
                              word3 == x[num-2] &
                              word4 == x[num-1] &
                              word5 == x[num])$word4[1:n]
        result6 <- result6[!is.na(result6)]
        if(length(result6) == 0){result6 <- NULL}
        result6
    }
    
    if (num == 1){
        y <- c(two(x))
    } else if (num == 2){
        y <- c(two(x), three(x))
    } else if (num == 3){
        y <- c(four(x), three(x), two(x))
    } else if (num == 4){
        y <- c(five(x), four(x), three(x), two(x))
    } else {
        y <- c(six(x), five(x), four(x), three(x), two(x))}
    
    y <- unique(y)[1:n]
    return(y)
}
# Define UI for application that draws a histogram
ui <- fluidPage(
    
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
            
            verbatimTextOutput("prediction1"),
            verbatimTextOutput("prediction2"),
            verbatimTextOutput("prediction3"),
            verbatimTextOutput("prediction4"),
            verbatimTextOutput("prediction5"),
            verbatimTextOutput("prediction6"),
            verbatimTextOutput("prediction7"),
            verbatimTextOutput("prediction8"),
            verbatimTextOutput("prediction9"),
            verbatimTextOutput("prediction10")

            #textOutput(outputId = "put")
        )
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    result <- reactive(nextword(x = input$text, n = input$number))
    
    output$put <- renderText(result())
    observe({
        nus <- input$number
        if (nus == 1) {
            output$prediction1 <- renderText(result()[1])
            output$prediction2 <- NULL
            output$prediction3 <- NULL
            output$prediction4 <- NULL
            output$prediction5 <- NULL
            output$prediction6 <- NULL
            output$prediction7 <- NULL
            output$prediction8 <- NULL
            output$prediction9 <- NULL
            output$prediction10 <- NULL
        } else if (nus == 2) {
            output$prediction1 <- renderText(result()[1])
            output$prediction2 <- renderText(result()[2])
            output$prediction3 <- NULL
            output$prediction4 <- NULL
            output$prediction5 <- NULL
            output$prediction6 <- NULL
            output$prediction7 <- NULL
            output$prediction8 <- NULL
            output$prediction9 <- NULL
            output$prediction10 <- NULL
        } else if (nus == 3) {
            output$prediction1 <- renderText(result()[1])
            output$prediction2 <- renderText(result()[2])
            output$prediction3 <- renderText(result()[2])
            output$prediction4 <- NULL
            output$prediction5 <- NULL
            output$prediction6 <- NULL
            output$prediction7 <- NULL
            output$prediction8 <- NULL
            output$prediction9 <- NULL
            output$prediction10 <- NULL
        } else if (nus == 4) {
            output$prediction1 <- renderText(result()[1])
            output$prediction2 <- renderText(result()[2])
            output$prediction3 <- renderText(result()[2])
            output$prediction4 <- renderText(result()[4])
            output$prediction5 <- NULL
            output$prediction6 <- NULL
            output$prediction7 <- NULL
            output$prediction8 <- NULL
            output$prediction9 <- NULL
            output$prediction10 <- NULL
        } else if (nus == 5) {
            output$prediction1 <- renderText(result()[1])
            output$prediction2 <- renderText(result()[2])
            output$prediction3 <- renderText(result()[2])
            output$prediction4 <- renderText(result()[4])
            output$prediction5 <- renderText(result()[5])
            output$prediction6 <- NULL
            output$prediction7 <- NULL
            output$prediction8 <- NULL
            output$prediction9 <- NULL
            output$prediction10 <- NULL
        } else if (nus == 6) {
            output$prediction1 <- renderText(result()[1])
            output$prediction2 <- renderText(result()[2])
            output$prediction3 <- renderText(result()[2])
            output$prediction4 <- renderText(result()[4])
            output$prediction5 <- renderText(result()[5])
            output$prediction6 <- renderText(result()[6])
            output$prediction7 <- NULL
            output$prediction8 <- NULL
            output$prediction9 <- NULL
            output$prediction10 <- NULL
        } else if (nus == 7) {
            output$prediction1 <- renderText(result()[1])
            output$prediction2 <- renderText(result()[2])
            output$prediction3 <- renderText(result()[2])
            output$prediction4 <- renderText(result()[4])
            output$prediction5 <- renderText(result()[5])
            output$prediction6 <- renderText(result()[6])
            output$prediction7 <- renderText(result()[7])
            output$prediction8 <- NULL
            output$prediction9 <- NULL
            output$prediction10 <- NULL
        } else if (nus == 8) {
            output$prediction1 <- renderText(result()[1])
            output$prediction2 <- renderText(result()[2])
            output$prediction3 <- renderText(result()[2])
            output$prediction4 <- renderText(result()[4])
            output$prediction5 <- renderText(result()[5])
            output$prediction6 <- renderText(result()[6])
            output$prediction7 <- renderText(result()[7])
            output$prediction8 <- renderText(result()[8])
            output$prediction9 <- NULL
            output$prediction10 <- NULL
        } else if (nus == 9) {
            output$prediction1 <- renderText(result()[1])
            output$prediction2 <- renderText(result()[2])
            output$prediction3 <- renderText(result()[2])
            output$prediction4 <- renderText(result()[4])
            output$prediction5 <- renderText(result()[5])
            output$prediction6 <- renderText(result()[6])
            output$prediction7 <- renderText(result()[7])
            output$prediction8 <- renderText(result()[8])
            output$prediction9 <- renderText(result()[9])
            output$prediction10 <- NULL
        } else if (nus == 10) {
            output$prediction1 <- renderText(result()[1])
            output$prediction2 <- renderText(result()[2])
            output$prediction3 <- renderText(result()[2])
            output$prediction4 <- renderText(result()[4])
            output$prediction5 <- renderText(result()[5])
            output$prediction6 <- renderText(result()[6])
            output$prediction7 <- renderText(result()[7])
            output$prediction8 <- renderText(result()[8])
            output$prediction9 <- renderText(result()[9])
            output$prediction10 <- renderText(result()[10])
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
