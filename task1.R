# task 1
'''
Tasks to accomplish

Tokenization - identifying appropriate tokens such as words, 
punctuation, and numbers. Writing a function that takes a file as input and returns a tokenized version of it.
Profanity filtering - removing profanity and other words you do not want to predict.
Tips, tricks, and hints

Loading the data in. This dataset is fairly large. 
We emphasize that you do not necessarily need to load the entire dataset in to build your algorithms (see point 2 below). 
At least initially, you might want to use a smaller subset of the data. 
Reading in chunks or lines using Rs readLines or scan functions can be useful. 
You can also loop over each line of text by embedding readLines within a for/while loop, but this may be slower than reading in large chunks at a time. 
Reading pieces of the file at a time will require the use of a file connection in R. 
For example, the following code could be used to read the first few lines of the English Twitter dataset:con <- file("en_US.twitter.txt", "r") readLines(con, 1) 
## Read the first line of text readLines(con, 1) ## Read the next line of text readLines(con, 5) 
## Read in the next 5 lines of text close(con) 
## It is important to close the connection when you are done. See the connections help page for more information.
Sampling. To reiterate, to build models you do not need to load in and use all of the data. Often relatively few randomly selected rows or chunks need to be included to get an accurate approximation to results that would be obtained using all the data. Remember your inference class and how a representative sample can be used to infer facts about a population. You might want to create a separate sub-sample dataset by reading in a random subset of the original data and writing it out to a separate file. That way, you can store the sample and not have to recreate it every time. You can use the rbinom function to "flip a biased coin" to determine whether you sample a line of text or not.
'''
blogs_file   <- "./en_US/en_US.blogs.txt"
news_file    <- "./en_US/en_US.news.txt"
twitter_file <- "./en_US/en_US.twitter.txt"  

blogs_size   <- file.size(blogs_file) / (2^20)
news_size    <- file.size(news_file) / (2^20)
twitter_size <- file.size(twitter_file) / (2^20)

blogs   <- readLines(blogs_file)
news    <- readLines(news_file)
twitter <- readLines(twitter_file) 
str(blogs)
blogs[1]
blogs_lines   <- length(blogs)
news_lines    <- length(news)
twitter_lines <- length(twitter)
total_lines   <- blogs_lines + news_lines + twitter_lines
nchar(blogs[1])
blogs_nchar   <- nchar(blogs)
news_nchar    <- nchar(news)
twitter_nchar <- nchar(twitter)

boxplot(blogs_nchar, news_nchar, twitter_nchar, log = "y",
        names = c("blogs", "news", "twitter"),
        ylab = "log(Number of Characters)", xlab = "File Name") 
title("Comparing Distributions of Chracters per Line")

blogs_nchar_sum   <- sum(blogs_nchar)
news_nchar_sum    <- sum(news_nchar)
twitter_nchar_sum <- sum(twitter_nchar)

library(ngram)
blogs_words <- wordcount(blogs, sep = " ")
news_words  <- wordcount(news,  sep = " ")
twitter_words <- wordcount(news, sep = " ")

library(dplyr)
repo_summary <- data.frame(f_names = c("blogs", "news", "twitter"),
                           f_size  = c(blogs_size, news_size, twitter_size),
                           f_lines = c(blogs_lines, news_lines, twitter_lines),
                           n_char =  c(blogs_nchar_sum, news_nchar_sum, twitter_nchar_sum),
                           n_words = c(blogs_words, news_words, twitter_words))
repo_summary <- repo_summary %>% mutate(pct_n_char = round(n_char/sum(n_char), 2))
repo_summary <- repo_summary %>% mutate(pct_lines = round(f_lines/sum(f_lines), 2))
repo_summary <- repo_summary %>% mutate(pct_words = round(n_words/sum(n_words), 2))
library(knitr)
kable(repo_summary)

# sampling
sample_pct = 0.05
set.seed(1001)
blogs_size   <- blogs_lines * sample_pct
news_size    <- news_lines * sample_pct
twitter_size <- twitter_lines * sample_pct

blogs_sample   <- sample(blogs, blogs_size)
news_sample    <- sample(news, news_size)
twitter_sample <- sample(twitter, twitter_size)
repo_sample    <- c(blogs_sample, news_sample, twitter_sample)

str(repo_sample)
class(repo_sample)

## transfer the data into corpus(a kind of dataset utilized in nlp)
library(tm)
clean_sample <- Corpus(VectorSource(repo_sample))
clean_sample[1]
as.character(clean_sample[1])

## remove the urls
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
clean_sample <- tm_map(clean_sample, content_transformer(removeURL))

## Remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
clean_sample <- tm_map(clean_sample, content_transformer(removeNumPunct))

## Transform sample to all lower case
clean_sample <- tm_map(clean_sample, content_transformer(tolower))

## Remove profanity
library(rvest)
words <- read_html('https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en',encoding="UTF-8")
res <-words %>% html_nodes("p") %>% html_text()
profanity <- strsplit(res,'\n')[[1]]
clean_sample <- tm_map(clean_sample, removeWords, profanity)

## Remove stopwords(like the words a, an, the)
clean_sample <- tm_map(clean_sample, removeWords, stopwords("english"))
clean_sample <- tm_map(clean_sample, removeWords, stopwords("SMART"))
print(as.character(clean_sample[[1]]))

## remove whitespace
clean_sample <- tm_map(clean_sample, stripWhitespace)
print(as.character(clean_sample[[1]]))

## Save clean corpus  
saveRDS(clean_sample, file = "./en_US/clean_sample.rds" )

# Initial Exploratory Data Analysis
## Convert to document term matrix
?DocumentTermMatrix
docterm_corpus <- DocumentTermMatrix(clean_sample)
class(docterm_corpus)
inspect(docterm_corpus)
colSums(as.matrix(docterm_corpus))

dim(docterm_corpus)
?removeSparseTerms
new_docterm_corpus <- removeSparseTerms(docterm_corpus,sparse = 0.999)
dim(new_docterm_corpus)

colS <- colSums(as.matrix(new_docterm_corpus))
length(colS)
library(data.table)
doc_features <- data.table(name = attributes(colS)$names, count = colS)

## Most frequent and least frequent words
doc_features[order(-count)][1:10] #top 10 most frequent words
doc_features[order(count)][1:10] #least 10 frequent words

library(ggplot2)
ggplot(doc_features[count>5000],aes(name, count)) +
    geom_bar(stat = "identity",fill='lightblue',color='black') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme_classic()
library(wordcloud)
wordcloud(names(colS), colS, min.freq = 500, 
          colors = brewer.pal(6, 'Dark2'), random.order = FALSE) 
wordcloud(names(colS), colS, min.freq = 2000, 
          colors = brewer.pal(6, 'Dark2'), random.order = FALSE) 

