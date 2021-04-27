library(tidyverse)
# load the variables
# unigram <- readRDS('unigram.rds')
bigram <- readRDS('Bigram.rds')
trigram <- readRDS('Trigram.rds')
quardgram <- readRDS("quardgram.rds")
fivgram <- readRDS("fivgram.rds")
sixgram <- readRDS("sixgram.rds")
# select the ones with 50% or higher frequency
# unigram <- mutate(unigram, proportions = Freq/sum(Freq))
# unigram <- arrange(unigram, Freq)
# unigram <- mutate(unigram, cover = cumsum(proportions))
# unigram <- filter(unigram, cover >= 0.5)
# unigram <- arrange(unigram, desc(Freq))

bigram <- bigram %>%
    mutate(., proportions = Freq/sum(Freq)) %>%
    arrange(., Freq) %>%
    mutate(., cover = cumsum(proportions)) %>%
#    filter(., cover >= 0.5) %>%
    arrange(., desc(Freq))

trigram <- trigram %>%
    mutate(., proportions = Freq/sum(Freq)) %>%
    arrange(., Freq) %>%
    mutate(., cover = cumsum(proportions)) %>%
#    filter(., cover >= 0.5) %>%
    arrange(., desc(Freq))

quardgram <- quardgram %>%
    mutate(., proportions = Freq/sum(Freq)) %>%
    arrange(., Freq) %>%
    mutate(., cover = cumsum(proportions)) %>%
#    filter(., cover >= 0.5) %>%
    arrange(., desc(Freq))

fivgram <- fivgram %>%
    mutate(., proportions = Freq/sum(Freq)) %>%
    arrange(., Freq) %>%
    mutate(., cover = cumsum(proportions)) %>%
#    filter(., cover >= 0.5) %>%
    arrange(., desc(Freq))

sixgram <- sixgram %>%
    mutate(., proportions = Freq/sum(Freq)) %>%
    arrange(., Freq) %>%
    mutate(., cover = cumsum(proportions)) %>%
#    filter(., cover >= 0.5) %>%
    arrange(., desc(Freq))

nrow(quardgram)
nrow(fivgram)
nrow(sixgram)


# separate the phrases
library(tidyr)
library(stringr)
bigram <- separate(data = bigram, col = Bigram, 
                   into = c('word1', 'word2'), sep = ' ')
trigram <- separate(data = trigram, col = Trigram, 
                    into = c('word1', 'word2', 'word3'), sep = ' ')
quardgram <- separate(data = quardgram, col = quardgram, 
                     into = c('word1', 'word2', 'word3', 'word4'), sep = ' ')
fivgram <- separate(data = fivgram, col = fivgram, 
                    into = c('word1', 'word2', 'word3', 'word4', 'word5'), sep = ' ')
sixgram <- separate(data = sixgram, col = sixgram, 
                into = c('word1', 'word2', 'word3', 'word4', 'word5', 'word6'), sep = ' ')

bigram <- bigram[,1:2]
trigram <- trigram[,1:3]
quardgram <- quardgram[,1:4]
fivgram <- fivgram[,1:5]
sixgram <- sixgram[,1:6]

saveRDS(bigram, "two.rds")
saveRDS(trigram,"three.rds")
saveRDS(quardgram, "four.rds")
saveRDS(fivgram,"five.rds")
saveRDS(sixgram, "six.rds")

bigram <- readRDS('two.rds')
trigram <- readRDS("three.rds")
quardgram <- readRDS("four.rds")
fivgram <- readRDS("five.rds")
sixgram <- readRDS("six.rds")


library(tm)
nextword <- function(input){
    input <- tolower(input)
    input <- removeNumbers(input)
    input <- removePunctuation(input)
    input <- stripWhitespace(input)
    input <- unlist(strsplit(input, ' '))
    num <- length(input)
    
    two <- function(input){
        result2 <- subset(bigram, word1 == input[num])$word2[1:40]
        result2 <- result2[!is.na(result2)]
        if(length(result2) == 0){result2 <- NULL}
        result2
    }

    three <- function(input){
        result3 <- subset(trigram, word1 == input[num-1] &
                                  word2 == input[num])$word3[1:40]
        result3 <- result3[!is.na(result3)]
        if(length(result3) == 0){result3 <- NULL}
        result3
    }
    four <- function(input){
        result4 <- subset(quardgram, word1 == input[num-2] &
                             word2 == input[num-1] &
                             word3 == input[num])$word4[1:40]
        result4 <- result4[!is.na(result4)]
        if(length(result4) == 0){result4 <- NULL}
        result4
    }
    
    five <- function(input){
        result5 <- subset(fivgram, word1 == input[num-3] &
                             word2 == input[num-2] &
                             word3 == input[num-1] &
                             word4 == input[num])$word4[1:40]
        result5 <- result5[!is.na(result5)]
        if(length(result5) == 0){result5 <- NULL}
        result5
    }
    
    six <- function(input){
        result6 <- subset(sixgram, word1 == input[num-4] &
                             word2 == input[num-3] &
                             word3 == input[num-2] &
                             word4 == input[num-1] &
                             word5 == input[num])$word4[1:40]
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
    output <- unique(output)
    output
}

nextword("hello boys and")
