blogs_file   <- "./en_US/en_US.blogs.txt"
news_file    <- "./en_US/en_US.news.txt"
twitter_file <- "./en_US/en_US.twitter.txt"

# file size of ...
file.size(blogs_file)/(2^20)

# The en_US.twitter.txt has how many lines of text?
twitter <-  readLines(twitter_file, skipNul = TRUE) 
length(twitter)

# What is the length of the longest line seen in any of the three en_US data sets?
blog_lines <- nchar(readLines(blogs_file, skipNul = TRUE))
max(blog_lines)

news_lines <- nchar(readLines(news_file, skipNul = TRUE))
max(news_lines)

twitter_lines <- nchar(readLines(twitter_file, skipNul = TRUE))
max(twitter_lines)

# In the en_US twitter data set, if you divide the number of lines where the word “love” (all lowercase) occurs by the number of lines the word “hate” (all lowercase) occurs, about what do you get?
lines_love <- grepl(".love.", readLines(twitter_file, skipNul = TRUE), 
                    ignore.case = FALSE, perl = TRUE)

n_love <- sum(lines_love)

lines_hate <- grepl(".hate.", readLines(twitter_file, skipNul= TRUE), 
                    ignore.case = FALSE, perl = TRUE)

n_hate <- sum(lines_hate)

n_love/n_hate

# The one tweet in the en_US twitter data set that matches the word “biostats” says what?
twitter[grep("biostats", twitter)]

# How many tweets have the exact characters “A computer once beat me at chess, but it was no match for me at kickboxing”. (I.e. the line matches those characters exactly.)
sum(grepl( "A computer once beat me at chess, but it was no match for me at kickboxing", twitter))
