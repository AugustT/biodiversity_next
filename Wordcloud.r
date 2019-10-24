#install.packages(c('twitteR','tm','RCurl','methods'))

library(twitteR)
library(tm)
library(RCurl)
library(methods)
library(httr)

searchTerm='#biodiversitynext'
# Load credentials
setup_twitter_oauth(consumer_key = "XXX",
                    consumer_secret = "XXX",
                    access_token = 'XXX',
                    access_secret = 'XXX')

#Grab the tweets
rdmTweets <- searchTwitter(searchTerm, n = 10000)
#Use a handy helper function to put the tweets into a dataframe
tw.df=twListToDF(rdmTweets)
length(tw.df[,1])
##Note: there are some handy, basic Twitter related functions here:
##https://github.com/matteoredaelli/twitter-r-utils
#For example:
RemoveAtPeople <- function(tweet) {
  gsub("@\\w+", "", tweet)
}
#Then for example, remove @'d names
tweets <- as.vector(sapply(tw.df$text, RemoveAtPeople))

#remove search term
tweets<-tolower(tweets)
RemoveSearchTerm <- function(tweet) {
  tweet <- gsub(searchTerm, "", tweet)
  gsub("amp", "", tweet)
}
tweets <- as.vector(sapply(tweets, RemoveSearchTerm))

write.csv(tweets,  file = 'tweets.csv')

##Wordcloud - scripts available from various sources; I used:
#http://rdatamining.wordpress.com/2011/11/09/using-text-mining-to-find-out-what-rdatamining-tweets-are-about/
#Call with eg: tw.c=generateCorpus(tw.df$text)
generateCorpus= function(df,my.stopwords=c()){
  #Install the textmining library
  require(tm)
  #The following is cribbed and seems to do what it says on the can
  tw.corpus= Corpus(VectorSource(df))
  # remove punctuation
  tw.corpus = tm_map(tw.corpus, removePunctuation)
  #normalise case
  tw.corpus = tm_map(tw.corpus, tolower)
  # remove stopwords
  tw.corpus = tm_map(tw.corpus, removeWords, stopwords('english'))
  tw.corpus = tm_map(tw.corpus, removeWords, my.stopwords)

  tw.corpus
}

wordcloud.generate=function(corpus,min.freq=8){
  require(wordcloud)
  pal2 <- brewer.pal(8,"Set2")
  doc.m = tm::TermDocumentMatrix(corpus, control = list(minWordLength = 1))
  dm = as.matrix(doc.m)
  # calculate the frequency of words
  v = sort(rowSums(dm), decreasing=TRUE)
  d = data.frame(word=names(v), freq=v)
  #Generate the wordcloud
  wc=wordcloud(d$word, d$freq, min.freq=min.freq,colors=pal2,random.order=F)
  wc
}

print(wordcloud.generate(generateCorpus(tweets,'dev8d'),12))

##Generate an image file of the wordcloud
#png('test.png', width=1000,height=1000)
#wordcloud.generate(generateCorpus(tweets,'dev8d'),3)
#dev.off()