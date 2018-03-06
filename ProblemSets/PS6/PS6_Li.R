library(ggplot2)
library(wordcloud)
library(RColorBrewer)

messages <- read.csv("~/Desktop/Text_Analytics/spam.csv", stringsAsFactors =F,fileEncoding = "latin1")

names(messages) <- c("label","text")
levels(as.factor(messages$label))
messages$text<- as.character(messages$text)
str(messages)

# First visualize the distribution of text messages 
# create new feature to measure the legnth of each text
# create second df to not mess with my bag of words model 

spam<- messages
spam$TextLength <- nchar(spam$text)
ggplot(spam, aes(x = TextLength, fill = label)) +
  theme_bw() +
  geom_histogram(binwidth = 6) +
  labs(y = "Text Count", x = "Length of Text",
       title = "Distribution of Text Lengths by Spam vs Ham")

# Make bag of words model
# at first I tried using the quanteda package but it was having issues with word clouds
bag_model <- Corpus(VectorSource(messages$text))

bag_model <- tm_map(bag_model, tolower)
bag_model <- tm_map(bag_model, removeNumbers)
bag_model <- tm_map(bag_model, stemDocument)
bag_model <- tm_map(bag_model, removePunctuation)
bag_model <- tm_map(bag_model, removeWords, c(stopwords("english")))
bag_model <- tm_map(bag_model, stripWhitespace)


graphics.off()
wordcloud(bag_model, max.words=200,scale=c(3,1),colors=brewer.pal(6,"Dark2"))

# Convert bag_model of words to data frame
frequency <- DocumentTermMatrix(bag_model)

# Fing words that appear atleast 100 times
findFreqTerms(frequency, lowfrequency = 100)
sparseWords <- removeSparseTerms(frequency, 0.995)

# Organizing frequency of terms
frequency <- colSums(as.matrix(sparseWords))
order_words <- order(frequency)

# Create Wordcloud

set.seed(123)

# create a data frame for visualization
wordfreq <- data.frame(word = names(frequency), frequency = frequency)

#plot 5000 most frequent words
wordcloud(names(frequency), frequency, max.words = 5000, scale = c(6, .1), colors = brewer.pal(6, 'Set2'))

#Create word cloud based off of each catagory
ham_cloud<- which(messages$label=="spam")
spam_cloud<- which(messages$label=="ham")

wordcloud(bag_model[ham_cloud],min.frequency=60)
wordcloud(bag_model[spam_cloud],min.frequency=60)
