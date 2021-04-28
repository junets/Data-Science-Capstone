# for shiny
bigram <- read_rds(url("https://raw.githubusercontent.com/junets/Data-Science-Capstone/main/two.rds"))
trigram <- readRDS(url("https://raw.githubusercontent.com/junets/Data-Science-Capstone/main/three.rds"))
quardgram <- readRDS(url("https://raw.githubusercontent.com/junets/Data-Science-Capstone/main/four.rds"))
fivgram <- readRDS(url("https://raw.githubusercontent.com/junets/Data-Science-Capstone/main/five.rds"))
sixgram <- readRDS(url("https://raw.githubusercontent.com/junets/Data-Science-Capstone/main/six.rds"))

nextword <- function(input, n = 40){
    input <- tolower(input)
    input <- removeNumbers(input)
    input <- removePunctuation(input)
    input <- stripWhitespace(input)
    input <- unlist(strsplit(input, ' '))
    num <- length(input)
    
    two <- function(input){
        result2 <- subset(bigram, word1 == input[num])$word2[1:n]
        result2 <- result2[!is.na(result2)]
        if(length(result2) == 0){result2 <- NULL}
        result2
    }
    
    three <- function(input){
        result3 <- subset(trigram, word1 == input[num-1] &
                              word2 == input[num])$word3[1:n]
        result3 <- result3[!is.na(result3)]
        if(length(result3) == 0){result3 <- NULL}
        result3
    }
    four <- function(input){
        result4 <- subset(quardgram, word1 == input[num-2] &
                              word2 == input[num-1] &
                              word3 == input[num])$word4[1:n]
        result4 <- result4[!is.na(result4)]
        if(length(result4) == 0){result4 <- NULL}
        result4
    }
    
    five <- function(input){
        result5 <- subset(fivgram, word1 == input[num-3] &
                              word2 == input[num-2] &
                              word3 == input[num-1] &
                              word4 == input[num])$word4[1:n]
        result5 <- result5[!is.na(result5)]
        if(length(result5) == 0){result5 <- NULL}
        result5
    }
    
    six <- function(input){
        result6 <- subset(sixgram, word1 == input[num-4] &
                              word2 == input[num-3] &
                              word3 == input[num-2] &
                              word4 == input[num-1] &
                              word5 == input[num])$word4[1:n]
        result6 <- result6[!is.na(result6)]
        if(length(result6) == 0){result6 <- NULL}
        result6
    }
    
    if (num == 1){
        output <- c(two(input))
    } else if (num == 2){
        output <- c(two(input), three(input))
    } else if (num == 3){
        output <- c(four(input), three(input), two(input))
    } else if (num == 4){
        output <- c(five(input), four(input), three(input), two(input))
    } else {
        output <- c(six(input), five(input), four(input), three(input), two(input))}
    
    output <- unique(output)[1:n]
    return(output)
}

a = nextword("hello ladies and", 20)
data.frame(a)
a = as.data.frame(a)
a
