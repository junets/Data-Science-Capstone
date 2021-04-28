#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    result <- reactive(nextword(x = input$text, n = input$number))
    
    output$put <- renderText(result())

})
