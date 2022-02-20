#install.packages('tm')

library(tm)

#install.packages('wordcloud')

library(wordcloud)

#install.packages('wordnet')

library(wordnet)

#setDict("C:/Program Files (x86)/WordNet/2.1/dict")

#install.packages('SnowballC')

library(SnowballC)
library(cluster)
library(topicmodels)
library(RColorBrewer)
library(lda)
library(RColorBrewer)
#library(RWeka)
library(data.table)
library(dplyr)
textdata <- read.csv("Rail_Equipment_Accident_Incident_Data.csv", header = TRUE, sep = ",", encoding = "UTF-8")
dim(textdata)
colnames(textdata)

textdata$Accident.Cause
textdata$Narrative
textdata$Accident.Type
textdata$Equipment.Type
textdata$Train.Number
textdata$Joint.Track.Type
textdata$Weather.Condition
textdata$Visibility

table(textdata[, "Accident.Cause"])
table(textdata[, "Narrative"])
table(textdata[, "Accident.Type"])
table(textdata[, "Equipment.Type"])
table(textdata[, "Train.Number"])
table(textdata[, "Joint.Track.Type"])
table(textdata[, "Weather.Condition"])
table(textdata[, "Visibility"])

#create corpus from vector
crps_docu1 <- Corpus(VectorSource(textdata$Accident.Cause))
crps_docu1
#inspect a particular document in corpus
writeLines(as.character(crps_docu1[[1]]))

crps_docu2 <- Corpus(VectorSource(textdata$Narrative))
crps_docu2
#inspect a particular document in corpus
writeLines(as.character(crps_docu2[[1]]))

crps_docu3 <- Corpus(VectorSource(textdata$Accident.Type))
crps_docu3
#inspect a particular document in corpus
writeLines(as.character(crps_docu3[[1]]))

crps_docu4 <- Corpus(VectorSource(textdata$Equipment.Type))
crps_docu4
#inspect a particular document in corpus
writeLines(as.character(crps_docu4[[1]]))

crps_docu5 <- Corpus(VectorSource(textdata$Train.Number))
crps_docu5
#inspect a particular document in corpus
writeLines(as.character(crps_docu5[[2]]))

crps_docu6 <- Corpus(VectorSource(textdata$Joint.Track.Type))
crps_docu6
#inspect a particular document in corpus
writeLines(as.character(crps_docu6[[1]]))

crps_docu7 <- Corpus(VectorSource(textdata$Weather.Condition))
crps_docu7
#inspect a particular document in corpus
writeLines(as.character(crps_docu7[[1]]))

crps_docu8 <- Corpus(VectorSource(textdata$Visibility))
crps_docu8
#inspect a particular document in corpus
writeLines(as.character(crps_docu8[[1]]))

#####################################################################
#Transform to lower case
crps_docu1 <-tm_map(crps_docu1,content_transformer(tolower))
#remove potentially problematic symbols
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
#Removing all the special characters..

crps_docu1 <- tm_map(crps_docu1, toSpace, "/")
crps_docu1 <- tm_map(crps_docu1, toSpace, "@")
crps_docu1 <- tm_map(crps_docu1, toSpace, "\\|")
crps_docu1 <- tm_map(crps_docu1, removeNumbers)
crps_docu1 <- tm_map(crps_docu1, toSpace, "-")
crps_docu1 <- tm_map(crps_docu1, toSpace, ":")
crps_docu1 <- tm_map(crps_docu1, toSpace, "'")
crps_docu1 <- tm_map(crps_docu1, toSpace, "'")
crps_docu1 <- tm_map(crps_docu1, toSpace, " -")
crps_docu1 <- tm_map(crps_docu1, toSpace, " ")
crps_docu1 <- tm_map(crps_docu1, removeNumbers)

# Remove english common stopwords
crps_docu1 <- tm_map(crps_docu1, removeWords, stopwords("english"))
# Remove punctuations
crps_docu1 <- tm_map(crps_docu1, removePunctuation)
# Eliminate extra white spaces
crps_docu1 <- tm_map(crps_docu1, stripWhitespace)
writeLines(as.character(crps_docu1[[1]]))

#Transform to lower case
crps_docu2 <-tm_map(crps_docu2,content_transformer(tolower))
#remove potentially problematic symbols
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
#Removing all the special characters..

crps_docu2 <- tm_map(crps_docu2, toSpace, "/")
crps_docu2 <- tm_map(crps_docu2, toSpace, "@")
crps_docu2 <- tm_map(crps_docu2, toSpace, "\\|")
crps_docu2 <- tm_map(crps_docu2, removeNumbers)
crps_docu2 <- tm_map(crps_docu2, toSpace, "-")
crps_docu2 <- tm_map(crps_docu2, toSpace, ":")
crps_docu2 <- tm_map(crps_docu2, toSpace, "'")
crps_docu2 <- tm_map(crps_docu2, toSpace, "'")
crps_docu2 <- tm_map(crps_docu2, toSpace, " -")
crps_docu2 <- tm_map(crps_docu2, toSpace, " ")

crps_docu2 <- tm_map(crps_docu2, removeNumbers)

# Remove english common stopwords
crps_docu2 <- tm_map(crps_docu2, removeWords, stopwords("english"))
#nonenonenonenonenonenonenonenonenonenonenonenonenonenonenon
#crps_docu2 <- tm_map(crps_docu2, removeWords, stopwords("nonenonenonenonenonenonenonenonenonenonenonenonenonenonenon"))
# Remove punctuations
crps_docu2 <- tm_map(crps_docu2, removePunctuation)

# Eliminate extra white spaces
crps_docu2 <- tm_map(crps_docu2, stripWhitespace)

writeLines(as.character(crps_docu2[[1]]))

#Transform to lower case
crps_docu3 <-tm_map(crps_docu3,content_transformer(tolower))
#remove potentially problematic symbols
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
#Removing all the special characters..

crps_docu3 <- tm_map(crps_docu3, toSpace, "/")
crps_docu3 <- tm_map(crps_docu3, toSpace, "@")
crps_docu3 <- tm_map(crps_docu3, toSpace, "\\|")
crps_docu3 <- tm_map(crps_docu3, removeNumbers)
crps_docu3 <- tm_map(crps_docu3, toSpace, "-")
crps_docu3 <- tm_map(crps_docu3, toSpace, ":")
crps_docu3 <- tm_map(crps_docu3, toSpace, "'")
crps_docu3 <- tm_map(crps_docu3, toSpace, "'")
crps_docu3 <- tm_map(crps_docu3, toSpace, " -")
crps_docu3 <- tm_map(crps_docu3, toSpace, " ")

crps_docu3 <- tm_map(crps_docu3, removeNumbers)

# Remove english common stopwords
crps_docu3 <- tm_map(crps_docu3, removeWords, stopwords("english"))

# Remove punctuations
crps_docu3 <- tm_map(crps_docu3, removePunctuation)

# Eliminate extra white spaces
crps_docu3 <- tm_map(crps_docu3, stripWhitespace)

writeLines(as.character(crps_docu3[[1]]))

#Transform to lower case
crps_docu4 <-tm_map(crps_docu4,content_transformer(tolower))
#remove potentially problematic symbols
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
#Removing all the special characters..

crps_docu4 <- tm_map(crps_docu4, toSpace, "/")
crps_docu4 <- tm_map(crps_docu4, toSpace, "@")
crps_docu4 <- tm_map(crps_docu4, toSpace, "\\|")
crps_docu4 <- tm_map(crps_docu4, removeNumbers)
crps_docu4 <- tm_map(crps_docu4, toSpace, "-")
crps_docu4 <- tm_map(crps_docu4, toSpace, ":")
crps_docu4 <- tm_map(crps_docu4, toSpace, "'")
crps_docu4 <- tm_map(crps_docu4, toSpace, "'")
crps_docu4 <- tm_map(crps_docu4, toSpace, " -")
crps_docu4 <- tm_map(crps_docu4, toSpace, " ")

crps_docu4 <- tm_map(crps_docu4, removeNumbers)

# Remove english common stopwords
crps_docu4 <- tm_map(crps_docu4, removeWords, stopwords("english"))

# Remove punctuations
crps_docu4 <- tm_map(crps_docu4, removePunctuation)

# Eliminate extra white spaces
crps_docu4 <- tm_map(crps_docu4, stripWhitespace)

writeLines(as.character(crps_docu4[[1]]))

#Transform to lower case
#crps_docu5 <-tm_map(crps_docu5,content_transformer(tolower))
#remove potentially problematic symbols
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
#Removing all the special characters..

crps_docu5 <- tm_map(crps_docu5, toSpace, "/")
crps_docu5 <- tm_map(crps_docu5, toSpace, "@")
crps_docu5 <- tm_map(crps_docu5, toSpace, "\\|")
#crps_docu5 <- tm_map(crps_docu5, removeNumbers)
crps_docu5 <- tm_map(crps_docu5, toSpace, "-")
crps_docu5 <- tm_map(crps_docu5, toSpace, ":")
crps_docu5 <- tm_map(crps_docu5, toSpace, "'")
crps_docu5 <- tm_map(crps_docu5, toSpace, "'")
crps_docu5 <- tm_map(crps_docu5, toSpace, " -")
crps_docu5 <- tm_map(crps_docu5, toSpace, " ")

#crps_docu5 <- tm_map(crps_docu5, removeNumbers)

# Remove english common stopwords
#crps_docu5 <- tm_map(crps_docu5, removeWords, stopwords("english"))

# Remove punctuations
crps_docu5 <- tm_map(crps_docu5, removePunctuation)

# Eliminate extra white spaces
crps_docu5 <- tm_map(crps_docu5, stripWhitespace)

writeLines(as.character(crps_docu5[[2]]))

#Transform to lower case
#crps_docu6 <-tm_map(crps_docu6,content_transformer(tolower))
#remove potentially problematic symbols
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
#Removing all the special characters..

crps_docu6 <- tm_map(crps_docu6, toSpace, "/")
crps_docu6 <- tm_map(crps_docu6, toSpace, "@")
crps_docu6 <- tm_map(crps_docu6, toSpace, "\\|")
crps_docu6 <- tm_map(crps_docu6, removeNumbers)
crps_docu6 <- tm_map(crps_docu6, toSpace, "-")
crps_docu6 <- tm_map(crps_docu6, toSpace, ":")
crps_docu6 <- tm_map(crps_docu6, toSpace, "'")
crps_docu6 <- tm_map(crps_docu6, toSpace, "'")
crps_docu6 <- tm_map(crps_docu6, toSpace, " -")
crps_docu6 <- tm_map(crps_docu6, toSpace, " ")

crps_docu6 <- tm_map(crps_docu6, removeNumbers)

# Remove english common stopwords
crps_docu6 <- tm_map(crps_docu6, removeWords, stopwords("english"))

# Remove punctuations
crps_docu6 <- tm_map(crps_docu6, removePunctuation)

# Eliminate extra white spaces
crps_docu6 <- tm_map(crps_docu6, stripWhitespace)

writeLines(as.character(crps_docu6[[1]]))

#Transform to lower case
crps_docu7 <-tm_map(crps_docu7,content_transformer(tolower))
#remove potentially problematic symbols
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
#Removing all the special characters..

crps_docu7 <- tm_map(crps_docu7, toSpace, "/")
crps_docu7 <- tm_map(crps_docu7, toSpace, "@")
crps_docu7 <- tm_map(crps_docu7, toSpace, "\\|")
crps_docu7 <- tm_map(crps_docu7, removeNumbers)
crps_docu7 <- tm_map(crps_docu7, toSpace, "-")
crps_docu7 <- tm_map(crps_docu7, toSpace, ":")
crps_docu7 <- tm_map(crps_docu7, toSpace, "'")
crps_docu7 <- tm_map(crps_docu7, toSpace, "'")
crps_docu7 <- tm_map(crps_docu7, toSpace, " -")
crps_docu7 <- tm_map(crps_docu7, toSpace, " ")

crps_docu7 <- tm_map(crps_docu7, removeNumbers)

# Remove english common stopwords
crps_docu7 <- tm_map(crps_docu7, removeWords, stopwords("english"))

# Remove punctuations
crps_docu7 <- tm_map(crps_docu7, removePunctuation)

# Eliminate extra white spaces
crps_docu7 <- tm_map(crps_docu7, stripWhitespace)

writeLines(as.character(crps_docu7[[1]]))

#Transform to lower case
crps_docu8 <-tm_map(crps_docu8,content_transformer(tolower))
#remove potentially problematic symbols
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
#Removing all the special characters..

crps_docu8 <- tm_map(crps_docu8, toSpace, "/")
crps_docu8 <- tm_map(crps_docu8, toSpace, "@")
crps_docu8 <- tm_map(crps_docu8, toSpace, "\\|")
crps_docu8 <- tm_map(crps_docu8, removeNumbers)
crps_docu8 <- tm_map(crps_docu8, toSpace, "-")
crps_docu8 <- tm_map(crps_docu8, toSpace, ":")
crps_docu8 <- tm_map(crps_docu8, toSpace, "'")
crps_docu8 <- tm_map(crps_docu8, toSpace, "'")
crps_docu8 <- tm_map(crps_docu8, toSpace, " -")
crps_docu8 <- tm_map(crps_docu8, toSpace, " ")

crps_docu8 <- tm_map(crps_docu8, removeNumbers)

# Remove english common stopwords
crps_docu8 <- tm_map(crps_docu8, removeWords, stopwords("english"))

# Remove punctuations
crps_docu8 <- tm_map(crps_docu8, removePunctuation)

# Eliminate extra white spaces
crps_docu8 <- tm_map(crps_docu8, stripWhitespace)

writeLines(as.character(crps_docu8[[1]]))

data %>%
  unnest_tokens(word, category) %>%
  group_by(word) %>%
  count()
#########################################################
#load library
library(SnowballC)
#Stem document
crps_docu1 <- tm_map(crps_docu1,stemDocument)
writeLines(as.character(crps_docu1[[1]]))

crps_docu2 <- tm_map(crps_docu2,stemDocument)
writeLines(as.character(crps_docu2[[1]]))

crps_docu3 <- tm_map(crps_docu3,stemDocument)
writeLines(as.character(crps_docu3[[1]]))

crps_docu4 <- tm_map(crps_docu4,stemDocument)
writeLines(as.character(crps_docu4[[1]]))

crps_docu5 <- tm_map(crps_docu5,stemDocument)
writeLines(as.character(crps_docu5[[2]]))

crps_docu6 <- tm_map(crps_docu6,stemDocument)
writeLines(as.character(crps_docu6[[1]]))

crps_docu7 <- tm_map(crps_docu7,stemDocument)
writeLines(as.character(crps_docu7[[1]]))

crps_docu8 <- tm_map(crps_docu8,stemDocument)
writeLines(as.character(crps_docu8[[1]]))

#Create document-term matrix
rail_dtm1 <- DocumentTermMatrix(crps_docu1)
rail_dtm2 <- DocumentTermMatrix(crps_docu2)
rail_dtm3 <- DocumentTermMatrix(crps_docu3)
rail_dtm4 <- DocumentTermMatrix(crps_docu4)
rail_dtm5 <- DocumentTermMatrix(crps_docu5)
rail_dtm6 <- DocumentTermMatrix(crps_docu6)
rail_dtm7 <- DocumentTermMatrix(crps_docu7)
rail_dtm8 <- DocumentTermMatrix(crps_docu8)

################################################
# plot
library(tidytext)
library(ggplot2) 

freq1 <- colSums(as.matrix(rail_dtm1))
Accident_Cause <- data.frame(word=names(freq1), freq=freq1)   
head(Accident_Cause)
#plot frequency higher than 10,000
plot_freq1 <- ggplot(subset(Accident_Cause, freq>10000), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))
plot_freq1
###################
freq22 <- sort(colSums(as.matrix(rail_dtm2)), decreasing = TRUE)
Narrative <- data.frame(word=names(freq22), freq=freq22)   
head(Narrative)
#library(tidytext)
library(ggplot2)  
#plot frequency higher than 10,000
plot_freq33 <- ggplot(subset(Narrative, freq>10000), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))
plot_freq33
###################
freq2 <- sort(colSums(as.matrix(rail_dtm3)), decreasing = TRUE)
Accident_type <- data.frame(word=names(freq2), freq=freq2)   
head(Accident_type)
#library(tidytext)
library(ggplot2)  
#plot frequency higher than 10,000
plot_freq3 <- ggplot(subset(Accident_type, freq>10000), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))
plot_freq3
##########
freq4 <- sort(colSums(as.matrix(rail_dtm4)), decreasing = TRUE)
Equipment_type <- data.frame(word=names(freq4), freq=freq4)   
head(Equipment_type)
library(tidytext)
library(ggplot2)  
#plot frequency higher than 10,000
plot_freq4 <- ggplot(subset(Equipment_type, freq>1000), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))
plot_freq4
############
freq6 <- sort(colSums(as.matrix(rail_dtm6)), decreasing = TRUE)
Track_type <- data.frame(word=names(freq6), freq=freq6)   
head(Track_type)
library(tidytext)
library(ggplot2)  
#plot frequency higher than 10,000
plot_freq6 <- ggplot(subset(Track_type, freq>1000), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))
plot_freq6
#########
freq7 <- sort(colSums(as.matrix(rail_dtm7)), decreasing = TRUE)
Weather_condition <- data.frame(word=names(freq7), freq=freq7)   
head(Weather_condition)
library(tidytext)
library(ggplot2)  
#plot frequency higher than 10,000
plot_freq7 <- ggplot(subset(Weather_condition, freq>400), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))
plot_freq7
############
freq8 <- sort(colSums(as.matrix(rail_dtm8)), decreasing = TRUE)
Visibility <- data.frame(word=names(freq8), freq=freq8)   
head(Visibility)
library(tidytext)
library(ggplot2)  
#plot frequency higher than 10,000
plot_freq8 <- ggplot(subset(Visibility, freq>1000), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))
plot_freq8
#############################
#boxplot

ggplot(diamonds, aes(x=color, y=price)) + geom_boxplot()
##############################################
freq2 <- colSums(as.matrix(rail_dtm2))
#length should be total number of terms
length(freq2)
#create sort order (descending)
freq_ord2 <- order(freq,decreasing=TRUE)
#List all terms in decreasing order of freq and write to disk
freq[head(freq_ord2)]
freq[tail(freq_ord2)] 
findFreqTerms(rail_dtm1,highfreq=10000)
write.csv(freq[freq_ord2],"word_freq.csv")

m <- as.matrix(rail_dtm)
write.csv(m, file="allwords.csv")
#######################################
# Accident Cause word cloud
wordcloud(crps_docu1
          , scale=c(5,0.5)     # Set min and max scale
          , max.words=100      # Set top n words
          , random.order=FALSE # Words in decreasing freq
          , rot.per=0.35       # % of vertical words
          , use.r.layout=FALSE # Use C++ collision detection
          , colors=brewer.pal(8, "Dark2"))

# Narrative word cloud

wordcloud(crps_docu2
          , scale=c(1,0.5)     # Set min and max scale
          , max.words=50      # Set top n words
          , random.order=FALSE # Words in decreasing freq
          , rot.per=0.35       # % of vertical words
          , use.r.layout=FALSE # Use C++ collision detection
          , colors=brewer.pal(8, "Dark2"))

#####################################

rowTotals1 <- apply(rail_dtm1 , 1, sum) #Find the sum of words in each Document
rail_dtm_new1 <- rail_dtm1[rowTotals1 > 0, ]

##################################
library(topicmodels)

#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
#Number of topics
k <- 5

#Run LDA using Gibbs sampling
ldaOut1 <-LDA(rail_dtm_new1,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

#write out results
#docs to topics
ldaOut.topics1 <- as.matrix(topics(ldaOut1))
write.csv(ldaOut.topics1,file=paste("LDAGibbs",k,"DocsToTopics.csv"))

#top  terms in each topic
ldaOut.terms1 <- as.matrix(terms(ldaOut1,10))
write.csv(ldaOut.terms1,file=paste("LDAGibbs",k,"TopicsToTerms1.csv"))

#probabilities associated with each topic assignment
topicProbabilities1 <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities1,file=paste("LDAGibbs",k,"TopicProbabilities1.csv"))

#Find relative importance of top 2 topics
topic1ToTopic2 <- lapply(1:nrow(rail_dtm_new),function(x)
  sort(topicProbabilities[x,])[k]/sort(topicProbabilities1[x,])[k-1])

#Find relative importance of second and third most important topics
topic2ToTopic3 <- lapply(1:nrow(rail_dtm_new1),function(x)
  sort(topicProbabilities1[x,])[k-1]/sort(topicProbabilities1[x,])[k-2])

#write to file
write.csv(topic1ToTopic2,file=paste("LDAGibbs",k,"Topic1ToTopic2.csv"))
write.csv(topic2ToTopic3,file=paste("LDAGibbs",k,"Topic2ToTopic3.csv"))

#################3
##################################################################

## do tfxidf
rail_dtm_tfxidf <- weightTfIdf(rail_dtm_new1)
inspect(rail_dtm_tfxidf)

## do document clustering

### k-means( uses Euclidean distance)
m_new <- as.matrix(rail_dtm_tfxidf)
rownames(m_new) <- 1:nrow(m_new)

#normalize the vectors so Euclidean makes sense
norm_eucl <- function(m_new) m_new/apply(m_new, MARGIN=1, FUN=function(x) sum(x^2)^.5)
m_norm <- norm_eucl(m_new)


### cluster into 3 clusters
cl <- kmeans(m_norm, 3)
cl

table(cl$cluster)

### show clusters using the first 2 principal components
plot(prcomp(m_norm)$x, col=cl$cl)

findFreqTerms(rail_dtm_new1[cl$cluster==1,], 50)
inspect(rail_dtm_new1[which(cl$cluster==1)])

########################################
## hierarchical clustering not working
#install.packages("proxy")

m  <- as.matrix(rail_dtm1)
# # # m <- m[1:2, 1:3]
distMatrix <- dist(scale(m))
groups <- hclust(distMatrix,method="ward.D")
plot(groups, cex=0.9, hang=-1)
rect.hclust(groups, k=5)

dtmss <- removeSparseTerms(rail_dtm1, 0.9) # This makes a matrix that is only 15% empty space, maximum.   
dtmss
library(proxy)
d <- dist(scale(dtmss))
hc <- hclust(d=d, method="ward.D")
plot(hc)

########## working cluster
library(cluster)
groups <- hclust(d, method="ward.D")
#plot dendogram, use hang to ensure that labels fall below tree
plot(groups, cex=.9, hang=-1, main="word cluster dendrogram")

rect.hclust(groups,3)
(groups1 <- cutree(groups, k=3))


freq.terms<-findFreqTerms(rail_dtm1, lowfreq=500)[1:25]
################################################
install.packages("BiocManager")
BiocManager::install("Rgraphviz")
biocLite("Rgraphviz")

library(graph)

library(tidyverse)

library(Rgraphviz)

library(ggplot2)

library(bnlearn)

plot(rail_dtm1, term = freq.terms, corThreshold = 0.1, weighting = T)
graphviz.plot(rail_dtm1, layout = "dot")


#######################################
