# Task 0
'''
Tasks to accomplish

Obtaining the data - Can you download the data and load/manipulate it in R?
Familiarizing yourself with NLP and text mining - Learn about the basics of natural language processing and how it relates to the data science process you have learned in the Data Science Specialization.
Questions to consider

What do the data look like?
Where do the data come from?
Can you think of any other data sources that might help you in this project?
What are the common steps in natural language processing?
What are some common issues in the analysis of text data?
What is the relationship between NLP and the concepts you have learned in the Specialization?
'''
install.packages('tm')
library(tm)

# sample corpus
txt <- system.file("texts", "txt", package = "tm")
txt

ovid <- Corpus(DirSource(txt, encoding = "UTF-8"),
               readerControl = list(reader = readPlain,
                                    language = "lat",
                                    load = TRUE))
# examine matadata
ovid
ovid[[1]]
ovid[[2]]
ovid[[3]]
ovid[[4]]
ovid[[5]]
meta(ovid[[1]])
meta(ovid[[1]])$author
meta(ovid[[1]])$author <- "Publius Ovidius Naso"
meta(ovid[[1]])
ovid[[1]][1]
ovid[[1]][2]

ovid[1]
c(ovid[1:2], ovid[3:4])

length(ovid)
summary(ovid)

# Show predefined transformations
getTransformations()

ovid <- tm_map(ovid, FUN = removePunctuation)
ovid[[1]][1]

## Remove punctuation
ovid <- tm_map(ovid, FUN = removeNumbers)
ovid[[1]][1]

## Remove numbers
ovid <- tm_map(ovid, FUN = removeNumbers)
ovid[[1]][1]

## Change to all lower case
ovid <- tm_map(ovid, FUN = content_transformer(tolower))
ovid[[1]][1]

## Remove words
axe_words <- c("mater", "seu", "annis", "si")
ovid <- tm_map(ovid, FUN = removeWords, axe_words)
ovid[[1]][1]

## Remove whitespace
ovid <- tm_map(ovid, FUN = stripWhitespace)
ovid[[1]][1]

# Another example
reut21578 <- system.file("texts", "crude", package = "tm")
reuters <- VCorpus(DirSource(reut21578),
                   readerControl = list(reader = readReut21578XMLasPlain))
reuters[[1]][1]

## Eliminate Extra Whitespace
reuters <- tm_map(reuters, stripWhitespace)
reuters[[1]][1]
## Convert to Lower Case
reuters <- tm_map(reuters, content_transformer(tolower))
reuters[[1]][1]

## Stemming
library(SnowballC)
tm_map(reuters, stemDocument)

## Remove Stop Words
reuters <- tm_map(reuters, removeWords, stopwords("english"))
reuters[[1]][1]

## Creating Document-Term Matrices
dtm <- DocumentTermMatrix(reuters)
inspect(dtm[5:10, 740:743])
?DocumentTermMatrix
?inspect
## Operations on Document-Term Matrices
findFreqTerms(dtm, 5)

## Find Associations Between Words
findAssocs(dtm, "opec", 0.8)

## Remove Sparse Terms
inspect(removeSparseTerms(dtm, 0.4))

## Dictionary: Terms to Text Mine
inspect(DocumentTermMatrix(reuters,list(dictionary = c("prices", "crude", "oil"))))

## Session info
sessionInfo()
