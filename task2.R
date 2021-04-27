# task 2

blogs_file   <- "./en_US/en_US.blogs.txt"
news_file    <- "./en_US/en_US.news.txt"
twitter_file <- "./en_US/en_US.twitter.txt"  

blogs_size   <- file.size(blogs_file) / (2^20)
news_size    <- file.size(news_file) / (2^20)
twitter_size <- file.size(twitter_file) / (2^20)

blogs   <- readLines(blogs_file)
news    <- readLines(news_file)
twitter <- readLines(twitter_file) 

blogs_lines   <- length(blogs)
news_lines    <- length(news)
twitter_lines <- length(twitter)
total_lines   <- blogs_lines + news_lines + twitter_lines

blogs_nchar   <- nchar(blogs)
news_nchar    <- nchar(news)
twitter_nchar <- nchar(twitter)

blogs_nchar_sum   <- sum(blogs_nchar)
news_nchar_sum    <- sum(news_nchar)
twitter_nchar_sum <- sum(twitter_nchar)

barplot(c(blogs_nchar_sum, news_nchar_sum, twitter_nchar_sum),
        names = c("blogs", "news", "twitter"),
        ylab =  " Total Number of Characters", xlab = "File Name",
        col = c('red', 'lightyellow', 'lightgreen'))
title("Comparing Total # of Chracters per file")

library(ngram)
blogs_words <- wordcount(blogs, sep = " ")
news_words  <- wordcount(news,  sep = " ")
twitter_words <- wordcount(news, sep = " ")


file_sum <- data.frame(f_names = c("blogs", "news", "twitter"),
                           f_size  = c(blogs_size, news_size, twitter_size),
                           f_lines = c(blogs_lines, news_lines, twitter_lines),
                           n_char =  c(blogs_nchar_sum, news_nchar_sum, twitter_nchar_sum),
                           n_words = c(blogs_words, news_words, twitter_words))
library(dplyr)
file_sum <- mutate(file_sum,
                       pct_n_char = round(n_char/sum(n_char), 2),
                       pct_lines = round(f_lines/sum(f_lines), 2),
                       pct_words = round(n_words/sum(n_words), 2))
library(knitr)
kable(file_sum)

# sampling
sample_pct = 0.01
set.seed(2021)

blogs_size   <- blogs_lines * sample_pct
news_size    <- news_lines * sample_pct
twitter_size <- twitter_lines * sample_pct

blogs   <- sample(blogs, blogs_size)
news    <- sample(news, news_size)
twitter <- sample(twitter, twitter_size)
texttogether    <- c(blogs, news, twitter)

# clean the memory
keep <- c("file_sum","blogs","news","twitter","texttogether")
kpindex = c()
for(i in keep) kpindex <- c(kpindex,which(i == ls()))
rm(list = ls()[-kpindex])
ls()
gc()


texttogether <- gsub("'", "'", texttogether)
texttogether <- gsub("â€™", "'", texttogether)
texttogether <- gsub("\'[sS]", "", texttogether)
texttogether <- gsub("[^A-z ]", "", texttogether)
texttogether <- gsub("[tT]he", "", texttogether)
# Transfer to lower words, remove Punctiation, Numbers whitespace
library(tm)
corpus <- Corpus(VectorSource(texttogether))
print(as.character(corpus[[1]]))



corpus <- corpus %>% 
    tm_map(tolower) %>%
    tm_map(PlainTextDocument) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(stripWhitespace)
    #tm_map(removeWords, stopwords("english"))

# remove profanity
library(rvest)
words <- read_html('https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en',encoding="UTF-8")
res <-words %>% html_nodes("p") %>% html_text()
profanity <- strsplit(res,'\n')[[1]]
corpus <- tm_map(corpus, removeWords, profanity)

## remove the urls
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
corpus <- tm_map(corpus, content_transformer(removeURL))
corpus <- gsub("http\\w+","", corpus)

## Tokenization
library(RWeka)
# unigram <- NGramTokenizer(corpus, Weka_control(min = 1, max = 1, delimiters = " \\r\\n\\t.,;:\"()?!"))
Bigram <- NGramTokenizer(corpus, Weka_control(min = 2, max = 2, delimiters = " \\r\\n\\t.,;:\"()?!"))
Trigram <- NGramTokenizer(corpus, Weka_control(min = 3, max = 3, delimiters = " \\r\\n\\t.,;:\"()?!"))

# unigram <- arrange(data.frame(table(unigram)),desc(Freq))
Bigram <- arrange(data.frame(table(Bigram)), desc(Freq))
Trigram <- arrange(data.frame(table(Trigram)),desc(Freq))

# Plot the grams to show the most popular words or phrases
library(ggplot2)
library(wordcloud)
ggplot(unigram[1:20,], aes(x=reorder(unigram,Freq),y=Freq)) + 
    geom_col(stat="Identity", fill="pink") +
    geom_text(aes(label=Freq), vjust=-0.20) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    coord_flip()
wordcloud(unigram$unigram, unigram$Freq, min.freq= 500, random.order=TRUE, rot.per=.25, colors = brewer.pal(6, 'Dark2'))

ggplot(Bigram[1:20,], aes(x=reorder(Bigram,Freq),y=Freq)) + 
    geom_col(stat="Identity", fill="pink") +
    geom_text(aes(label=Freq), vjust=-0.20) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    coord_flip()
wordcloud(Bigram$Bigram, Bigram$Freq, min.freq= 50, random.order=TRUE, rot.per=.25, colors = brewer.pal(6, 'Dark2'))

ggplot(Trigram[1:20,], aes(x=reorder(Trigram,Freq),y=Freq)) + 
    geom_col(stat="Identity", fill="pink") +
    geom_text(aes(label=Freq), vjust=-0.20) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    coord_flip()
wordcloud(Trigram$Trigram, Trigram$Freq, min.freq= 6, random.order=TRUE, rot.per=.25, colors = brewer.pal(6, 'Dark2'))

saveRDS(unigram, "unigram.rds")
saveRDS(Bigram,"Bigram.rds")
saveRDS(Trigram, "Trigram.rds")


# Add some more grams
quardgram <- NGramTokenizer(corpus, Weka_control(min = 4, max = 4, delimiters = " \\r\\n\\t.,;:\"()?!"))
fivgram <- NGramTokenizer(corpus, Weka_control(min = 5, max = 5, delimiters = " \\r\\n\\t.,;:\"()?!"))
sixgram <- NGramTokenizer(corpus, Weka_control(min = 6, max = 6, delimiters = " \\r\\n\\t.,;:\"()?!"))

quardgram <- arrange(data.frame(table(quardgram)),desc(Freq))
fivgram <- arrange(data.frame(table(fivgram)), desc(Freq))
sixgram <- arrange(data.frame(table(sixgram)),desc(Freq))
saveRDS(quardgram, "quardgram.rds")
saveRDS(fivgram,"fivgram.rds")
saveRDS(sixgram, "sixgram.rds")
