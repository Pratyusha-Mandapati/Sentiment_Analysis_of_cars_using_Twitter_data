# install package to connect through monodb
install.packages("rmongodb")

install.packages("tm")

install.packages("wordcloud")

library("tm")
library(rmongodb)
library("wordcloud")
library(stringr)
library(RColorBrewer)

# connect to MongoDB
mongo = mongo.create(host = "localhost")
mongo.is.connected(mongo)

mongo.get.databases(mongo)

mongo.get.database.collections(mongo, db = "tweetDB2") #"tweetDB" is where twitter data is stored

library(plyr)
## create the empty data frame
df1 = data.frame(stringsAsFactors = FALSE)

## create the namespace
DBNS = "tweetDB2.honda"

## create the cursor we will iterate over, basically a select * in SQL
cursor = mongo.find(mongo, DBNS)

## create the counter
i = 1

## iterate over the cursor
while (mongo.cursor.next(cursor)) {
  # iterate and grab the next record
  tmp = mongo.bson.to.list(mongo.cursor.value(cursor))
  # make it a dataframe
  tmp.df = as.data.frame(t(unlist(tmp)), stringsAsFactors = F)
  # bind to the master dataframe
  df1 = rbind.fill(df1, tmp.df)
}

dim(df1)
#Second data frame with ford as the collection
df2 = data.frame(stringsAsFactors = FALSE)

## create the namespace
DBNS_2 = "tweetDB2.ford"

## create the cursor we will iterate over, basically a select * in SQL
cursor = mongo.find(mongo, DBNS_2)

## create the counter
i = 1

## iterate over the cursor
while (mongo.cursor.next(cursor)) {
  # iterate and grab the next record
  tmp = mongo.bson.to.list(mongo.cursor.value(cursor))
  # make it a dataframe
  tmp.df = as.data.frame(t(unlist(tmp)), stringsAsFactors = F)
  # bind to the master dataframe
  df2 = rbind.fill(df2, tmp.df)
}

dim(df2)
#Third data frame with toyota as the collection
df3 = data.frame(stringsAsFactors = FALSE)

## create the namespace
DBNS_3 = "tweetDB2.toyota"

## create the cursor we will iterate over, basically a select * in SQL
cursor = mongo.find(mongo, DBNS_3)

## create the counter
i = 1

## iterate over the cursor
while (mongo.cursor.next(cursor)) {
  # iterate and grab the next record
  tmp = mongo.bson.to.list(mongo.cursor.value(cursor))
  # make it a dataframe
  tmp.df = as.data.frame(t(unlist(tmp)), stringsAsFactors = F)
  # bind to the master dataframe
  df3 = rbind.fill(df3, tmp.df)
}

dim(df3)

#Fourth data frame with hyundai as the collection
df4 = data.frame(stringsAsFactors = FALSE)

## create the namespace
DBNS_4 = "tweetDB2.hyundai"

## create the cursor we will iterate over, basically a select * in SQL
cursor = mongo.find(mongo, DBNS_4)

## create the counter
i = 1

## iterate over the cursor
while (mongo.cursor.next(cursor)) {
  # iterate and grab the next record
  tmp = mongo.bson.to.list(mongo.cursor.value(cursor))
  # make it a dataframe
  tmp.df = as.data.frame(t(unlist(tmp)), stringsAsFactors = F)
  # bind to the master dataframe
  df4 = rbind.fill(df4, tmp.df)
}

dim(df4)

# Combining the text from all the data frames
df1_txt <- df1$tweet_text
df2_txt <- df2$tweet_text
df3_txt <- df3$tweet_text
df4_txt <- df4$tweet_text

cars = c(df1_txt, df2_txt, df3_txt, df4_txt)
cars

nd = c(length(df1_txt), length(df2_txt), length(df3_txt), length(df4_txt))
nd

#Positive and Negative word files
pos = scan('C:/Users/navee_000/Desktop/CMPE 239/R files/positive-words.txt', what='character', comment.char=';')

neg = scan('C:/Users/navee_000/Desktop/CMPE 239/R files/negative-words.txt', what='character', comment.char=';')
# function score.sentiment
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of postive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  
  # create simple array of scores with laply
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   # remove punctuation
                   sentence = gsub("[[:punct:]]", "", sentence)
                   # remove control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   # remove digits?
                   sentence = gsub('\\d+', '', sentence)
                   
                   # define error handling function when trying tolower
                   tryTolower = function(x)
                   {
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply 
                   sentence = sapply(sentence, tryTolower)
                   
                   # split sentence into words with str_split (stringr package)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   
                   # final score
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress=.progress )
  
  # data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}
# apply function score.sentiment
scores = score.sentiment(cars, pos, neg, .progress='text')
scores

#naive bayesian classifier
library("e1071")
library("klaR")
#install.packages("caret")
library("caret")
x=scores[,-5]
y=scores$score
model = train(x,y,'nd',trControl=trainControl(method='cv',number=10))
model = train(x,y,'nd',trControl=trainControl(method='cv',number=10))
classifier <- naiveBayes(scores[,1:2], scores[,2])
conf <- predict(classifier, scores[,1:2])
str(conf)
conf
table(scores)
scores$sentiment

savesscores <- scores
str(scores)
scores_1 <- scores
scores_1$scores <- as.numeric(levels(conf)) [conf]
conf
str(scores_1)
scores_1$scores

# add variables to data frame
scores$car = factor(rep(c("toyota", "ford", "honda", "hyundai"), nd))
scores$very.pos = as.numeric(scores$score >= 2)
scores$very.neg = as.numeric(scores$score <= -2)

# how many very positives and very negatives
numpos = sum(scores$very.pos)
numneg = sum(scores$very.neg)

# global score
global_score = round( 100 * numpos / (numpos + numneg) )
# colors
cols = c("#7CAE00", "#00BFC4", "#F8766D", "#C77CFF")
names(cols) = c("toyota", "ford", "honda", "hyundai")


# boxplot
library(ggplot2)
library(extrafont)

# plot to get the scores of cars
p <- ggplot(scores, aes(x=car, y=score)) +
  geom_bar(aes(fill=car),stat="identity") +
  xlab("car") + ylab("score") +
  theme_bw() +
  theme(text=element_text(family="Garamond", size=14))
#table(scores$score)
p
# barplot of average score
meanscore = tapply(scores$score, scores$car, mean)
df = data.frame(car=names(meanscore), meanscore=meanscore)
df$cars <- reorder(df$car, df$meanscore)
p <- ggplot(df, aes(x=cars, y=meanscore)) + geom_bar(aes(fill=cars), stat="identity") +
  ggtitle("Avg sentiment score") +
  xlab("cars") + ylab("meanscore") +
  theme_bw() +
  theme(text=element_text(family="Garamond", size=14))
p

#Word cloud
library(tm)
#create corpus
cars.corpus <- Corpus(VectorSource(cars))
cars.corpus <- tm_map(cars.corpus, function(x)removeWords(x,stopwords()))
library(wordcloud)
#generate wordcloud
tdm <- TermDocumentMatrix(cars.corpus)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
pal <- brewer.pal(9, "Dark2")
pal <- pal[-(1:2)]
png("wordcloud1234.png", width=12, height=8, units="in", res=300)
wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=2,max.words=1000, random.order=T, rot.per=.15, colors=pal)
dev.off()

#Very positive and very negative
# barplot of average very positive
car_pos = ddply(scores, .(car), summarise, mean_pos=mean(very.pos))
car_pos$cars <- reorder(car_pos$car, car_pos$mean_pos)

p <- ggplot(car_pos, aes(y=mean_pos)) +
  geom_bar(data=car_pos, aes(x=cars, fill=cars),stat="identity") +
  ggtitle("Avg very positive sentiment score") +
  xlab("Cars") + ylab("mean_pos") +
  theme_bw() +
  theme(text=element_text(family="Garamond", size=14))
p
# barplot of average very negative
car_neg = ddply(scores, .(car), summarise, mean_neg=mean(very.neg))
car_neg$cars <- reorder(car_neg$car, car_neg$mean_neg)

p <- ggplot(car_neg, aes(y=mean_neg)) +
  geom_bar(data=car_neg, aes(x=cars, fill=cars),stat="identity") +
  ggtitle("Avg very negative sentiment score") +
  xlab("cars") + ylab("mean_neg") +
  theme_bw() +
  theme(text=element_text(family="Garamond", size=14))
p


#Shiny for Visualization
install.packages("shiny")
library(shiny)
runExample("02_text")
runExample("03_reactivity")
runApp("MyApp")