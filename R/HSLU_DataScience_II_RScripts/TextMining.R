####################################################
# Some text mining experiments using the tm package
# 2018-06-06
# Author: Dr. Albert Blarer
# Demo HSLU
####################################################

library(data.table)
library(tm)

# Load the raw data, here tweets sampled from the public Twitter stream using the keyword "pegida"
tweets <- fread('data/TweetsPegida.csv')

# Transform the timestamp into a POSIXct format
class(tweets$createdAt)
tweets$createdAt <- as.POSIXct(tweets$createdAt, format('%Y-%m-%dT%H:%M:%SZ'), tz='CET')
class(tweets$createdAt)
# Filter the tweets of a specific day, here October 16, 2016, the 2nd birthday of Pegida
tweets20161016 = subset(tweets, as.Date(createdAt) %in% as.Date("2016-10-16"))

##################################################
# Basic NLP stuff using the text mining tm package
##################################################

# Build the text corpus with this sample
corpus = Corpus(VectorSource(as.vector(tweets20161016$text)))

# Summary views
class(corpus)
class(corpus[[1]])
head(summary(corpus))

# Explore documents resp. the corpus; entry 151 is just an example
inspect(corpus)
inspect(corpus[151])
as.character(corpus[[151]])

# Preprocess resp. transform the text
corpus <- tm_map(corpus, content_transformer(tolower))
inspect(corpus[151])
as.character(corpus[[151]])

corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[151])
as.character(corpus[[151]])

removeURL <- function(x) gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", x)
###############################################
# Regex explanation:
# (f|ht) match "f" or "ht"
# (tp) match "tp"
# (s?) optionally match "s" if it exists
# (://) match "://"
# (.*) match every character (everything) up to
# [.|/] a period or a forward-slash
# (.*) then everything after that
###############################################
corpus <- tm_map(corpus, removeURL)
inspect(corpus[151])
as.character(corpus[[151]])

corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[151])
as.character(corpus[[151]])

corpus <- tm_map(corpus, removeWords, stopwords("german"))
stopwords(kind="de")
inspect(corpus[151])
as.character(corpus[[151]])

corpus <- tm_map(corpus, removeWords, c("pegida", "rt"))
inspect(corpus[151])
as.character(corpus[[151]])

corpus <- tm_map(corpus, stripWhitespace)
inspect(corpus[151])
as.character(corpus[[151]])

# Stemm using the Snowball algorithm (a Porter stemmer)
library(SnowballC)
corpus <- tm_map(corpus, stemDocument, language = "german")
inspect(corpus[151])
as.character(corpus[[151]])

# Calculate the DocumentTermMatrix
dtm <- DocumentTermMatrix(corpus)   
dim(dtm)
inspect(dtm[1:3, 1:10])

# Explore the corpus by DTM
frequentTerms <- findFreqTerms(dtm, lowfreq=100)
frequentTerms
associateTerms <- findAssocs(dtm, "anniversary", corlimit=0.4)
associateTerms
associateTerms <- findAssocs(dtm, "geburtstag", corlimit=0.4)
associateTerms

# Get a sorted word frequency vector/data frame
tf <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
df.tf   <- data.frame(word=names(tf), tf=tf)

# Visualize word frequencies
library(ggplot2)
p <- ggplot(subset(df.tf, tf>200), aes(word, tf))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p
# Use an interactive visualization mode
library(plotly)
gg <- ggplotly(p)
gg

# Plot the term frequencies using a wordcloud.
library(wordcloud2)
wordcloud2(subset(df.tf,tf>100), color = "random-light")

# Calculate 'term frequency - inverse document frequency'
tfidf <- DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf))
dim(tfidf)
inspect(tfidf[1:20, 1:20])
tfidf <- sort(colSums(as.matrix(tfidf)), decreasing=TRUE)
df.tfidf <- data.frame(word=names(tfidf), tfidf=tfidf)

# Visualize tfidf
q <- ggplot(subset(df.tfidf, tfidf>100), aes(word, tfidf))
q <- q + geom_bar(stat="identity")
q <- q + theme(axis.text.y=element_text(angle=45, hjust=1)) + coord_flip()
q
# watch out here with the versions of ggplot2!
hh <- ggplotly(q)
hh

####################
# Cluster experiment
####################

tdm <- TermDocumentMatrix(corpus, control = list(wordLengths = c(1, Inf)))
tdm
# Remove sparse terms in the TDM
tdm.sparse <- removeSparseTerms(tdm, sparse = 0.95) 
tdm.sparse
m <- as.matrix(tdm.sparse)
# Cluster terms
distMatrix <- dist(scale(m))
fit <- hclust(distMatrix, method = "ward.D")
plot(fit)
# Cut the tree into k appropriate clusters
rect.hclust(fit, k = 4) 

################################################
# Topic model; LDA = Latent Dirichlet Allocation
################################################

library(topicmodels)
dtm <- as.DocumentTermMatrix(tdm)
# Find the sum of words in each document
rowTotals <- apply(dtm, 1, sum) 
dtm.new   <- dtm[rowTotals> 0, ]
lda <- LDA(dtm.new, k = 4) # find k topics
term <- terms(lda, 10) # first n terms of every topic 
term
# First topic identified for every document (tweet)
topic <- topics(lda, 1)
topics <- data.frame(date=as.POSIXct(tweets20161016$createdAt[1:length(topic)], topic))
qplot(date, ..count.., data=topics, geom="density", fill=term[topic], alpha=I(.5))

############################################
# Sentiment analysis; basic, lexical version
############################################

readAndflattenSentiWS <- function(filename) { 
  words = readLines(filename, encoding="UTF-8")
  words <- sub("\\|[A-Z]+\t[0-9.-]+\t?", ",", words)
  words <- unlist(strsplit(words, ","))
  words <- tolower(words)
  return(words)
}

pos.words <- readAndflattenSentiWS("data/SentiWS_v1.8c/SentiWS_v1.8c_Positive.txt")
neg.words <- readAndflattenSentiWS("data/SentiWS_v1.8c/SentiWS_v1.8c_Negative.txt")

# The sentiment analysis algorithm. Input is a vector of sentences.
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{ 
  require(plyr)
  require(stringr)
  scores = laply(sentences, function(sentence, pos.words, neg.words) 
  {
    # Clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # Convert to lower case:
    sentence = tolower(sentence)
    # Split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    # Compare the words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    # Note: match() returns the position of the matched term or NA
    # Transform to TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    # TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, 
  pos.words, neg.words, .progress=.progress )
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

# Test the function: score.sentiment(.,.,.)
sample = c("ich liebe dich. du bist wunderbar", "ich liebe dich nicht. du bist nicht wunderbar", "ich hasse dich. stirb!", "ich hasse dich nicht. stirb nicht!");
sample
test.sample <- score.sentiment(sample, pos.words, neg.words)
test.sample

# Now with our tweets
sample <- tweets20161016$text[1:1000]
sample
test.sample <- score.sentiment(sample, pos.words, neg.words)
res <- as.data.frame(test.sample)

