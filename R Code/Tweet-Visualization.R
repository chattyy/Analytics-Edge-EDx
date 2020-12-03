tweets <- read.csv("/Users/eshanchatty/Downloads/tweets.csv", stringsAsFactors=FALSE)

#Next, perform the following pre-processing tasks (like we did in Unit 5), noting that we don't stem the words in the document or remove sparse terms:

#Clean up data. Excluding stemming because it will be easier to red and understand the word cloud if it includes full words.

library(tm)
library(SnowballC)
corpus=VCorpus(VectorSource(tweets$Tweet))
corpus=tm_map(corpus,tolower)
corpus=tm_map(corpus,PlainTextDocument)
corpus=tm_map(corpus,removePunctuation)
corpus<- tm_map(corpus, removeWords, stopwords("english"))
dtm <- DocumentTermMatrix(corpus)
allTweets <- as.data.frame(as.matrix(dtm))
str(allTweets)
library(wordcloud)
head(colnames(allTweets))
head(colSums(allTweets))
wordcloud(colnames(allTweets), colSums(allTweets))
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25))
# 1) Create a corpus using the Tweet variable
corpus <- Corpus(VectorSource(tweets$Tweet))
corpus[[1]]$content

# 2) Convert the corpus to lowercase (don't forget to type 
# "corpus = tm_map(corpus, PlainTextDocument)" in your R console right after 
# this step)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, PlainTextDocument)
corpus[[1]]$content

# 3) Remove punctuation from the corpus
corpus <- tm_map(corpus, removePunctuation)
corpus[[1]]$content

# 4) Remove all English-language stopwords
corpus <- tm_map(corpus, removeWords, c("apple", stopwords("english")))
#corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus[[1]]$content

# 5) Build a document-term matrix out of the corpus
dtm <- DocumentTermMatrix(corpus)
dtm
allTweets <- as.data.frame(as.matrix(dtm))

# Create a word cloud with the updated corpus. What is the most common word in 
# this new corpus (the largest word in the outputted word cloud)? The most 
# frequent word might not be printed if you got a warning about words being cut 
# off -- if this happened, be sure to follow the instructions in the previous 
# problem.
tweetWords <- colnames(allTweets)
tweetWordsFreq <- colSums(allTweets)
wordcloud(tweetWords, tweetWordsFreq)

# 1) Create a corpus using the Tweet variable
corpusNegative <- Corpus(VectorSource(negativeTweets$Tweet))
corpusNegative[[1]]$content

# 2) Convert the corpus to lowercase (don't forget to type 
# "corpus = tm_map(corpus, PlainTextDocument)" in your R console right after 
# this step)
corpusNegative <- tm_map(corpusNegative, content_transformer(tolower))
corpusNegative <- tm_map(corpusNegative, PlainTextDocument)
corpusNegative[[1]]$content

# 3) Remove punctuation from the corpus
corpusNegative <- tm_map(corpusNegative, removePunctuation)
corpusNegative[[1]]$content

# 4) Remove all English-language stopwords
corpusNegative <- tm_map(corpusNegative, 
                         removeWords, c("apple", stopwords("english")))
#corpusNegative <- tm_map(corpusNegative, removeWords, stopwords("english"))
corpusNegative[[1]]$content

# 5) Build a document-term matrix out of the corpus
dtmNegative <- DocumentTermMatrix(corpusNegative)
dtmNegative

# 6) Convert the document-term matrix to a data frame called allNegativeTweets
allNegativeTweets <- as.data.frame(as.matrix(dtmNegative))

str(allNegativeTweets)
negativeTweetWords <- colnames(allNegativeTweets)
negativeTweetWordsFreq <- colSums(allNegativeTweets)
wordcloud(negativeTweetWords, negativeTweetWordsFreq)

## Word Cloud C


# Problem 3.2 - Size and Color
# Only one word cloud was created without modifying parameters min.freq or 
# max.words. Which word cloud is this?
## Word Cloud A:


# Problem 3.3 - Size and Color
# Which word clouds were created with parameter random.order set to FALSE?
?wordcloud
## Word Cloud B, Word Cloud D


# Problem 3.4 - Size and Color
# Which word cloud was built with a non-default value for parameter rot.per?
wordcloud(negativeTweetWords, negativeTweetWordsFreq)
wordcloud(negativeTweetWords, negativeTweetWordsFreq, rot.per = .5)
wordcloud(negativeTweetWords, negativeTweetWordsFreq, rot.per = 1)
## Word Cloud A


# Problem 3.5 - Size and Color
# In Word Cloud C and Word Cloud D, we provided a color palette ranging from 
# light purple to dark purple as the parameter colors (you will learn how to 
# make such a color palette later in this assignment). For which word cloud was 
# the parameter random.color set to TRUE?
## Word Cloud D


# Problem 4.1 - Selecting a Color Palette
# The use of a palette of colors can often improve the overall effect of a 
# visualization. We can easily select our own colors when plotting; for 
# instance, we could pass c("red", "green", "blue") as the colors parameter to 
# wordcloud(). The RColorBrewer package, which is based on the ColorBrewer 
# project (colorbrewer.org), provides pre-selected palettes that can lead to 
# more visually appealing images. Though these palettes are designed 
# specifically for coloring maps, we can also use them in our word clouds and 
# other visualizations.

# Begin by installing and loading the "RColorBrewer" package. This package may 
# have already been installed and loaded when you installed and loaded the 
# "wordcloud" package, in which case you don't need to go through this 
# additional installation step. If you obtain errors (for instance, "Error: 
# lazy-load database 'P' is corrupt") after installing and loading the 
# RColorBrewer package and running some of the commands, try closing and 
# re-opening R.
require(RColorBrewer)
# The function brewer.pal() returns color palettes from the ColorBrewer project 
# when provided with appropriate parameters, and the function 
# display.brewer.all() displays the palettes we can choose from.
display.brewer.all()
?brewer.pal
brewer.pal(8, "Accent")
# Which color palette would be most appropriate for use in a word cloud for 
# which we want to use color to indicate word frequency?
## YlOrRd


# Problem 4.2 - Selecting a Color Palette
# Which RColorBrewer palette name would be most appropriate to use when 
# preparing an image for a document that must be in grayscale?
## Greys


# Problem 4.3 - Selecting a Color Palette
# In sequential palettes, sometimes there is an undesirably large contrast 
# between the lightest and darkest colors. You can see this effect when plotting
# a word cloud for allTweets with parameter colors=brewer.pal(9, "Blues"), which
# returns a sequential blue palette with 9 colors.
wordcloud(tweetWords, tweetWordsFreq, colors=brewer.pal(9, "Blues"))
# Which of the following commands addresses this issue by removing the first 4 
# elements of the 9-color palette of blue colors? Select all that apply.
wordcloud(tweetWords, tweetWordsFreq, colors=brewer.pal(9, "Blues"))
wordcloud(tweetWords, tweetWordsFreq, 
          colors=brewer.pal(9, "Blues")[c(-5, -6, -7, -8, -9)])
wordcloud(tweetWords, tweetWordsFreq, 
          colors=brewer.pal(9, "Blues")[c(-1, -2, -3, -4)])
wordcloud(tweetWords, tweetWordsFreq, 
          colors=brewer.pal(9, "Blues")[c(1, 2, 3, 4)])
wordcloud(tweetWords, tweetWordsFreq, 
          colors=brewer.pal(9, "Blues")[c(5, 6, 7, 8, 9)])
