##################### setup environment  ######################

# setup libraries
if (!require(lattice)) {install.packages("lattice"); library(lattice)}
if (!require(NLP)) {install.packages("NLP"); library(NLP)}
if (!require(topicmodels)) {install.packages("topicmodels"); library(topicmodels)}
if (!require(tm)) {install.packages("tm"); library(tm)}
if (!require(slam)) {install.packages("slam"); library(slam)}

##################### input the data  ######################

## read in the data

library(readr)
yelp = read.csv("yelp_academic_dataset_review_train.csv", header = TRUE)


## make modifications to the dataset

# change data formats
yelp$business_id=as.factor(yelp$business_id)  # change from strings to factors -- easier to find common businesses
yelp$user_id=as.factor(yelp$user_id)  # change from strings to factors

## transform customer reviews into a tm structure that can be used for topic modeling

# first turn the reviews which are strings into words (or tokens)
creviews = VCorpus(VectorSource(yelp$text))
meta(creviews,"business_id")=yelp$business_id   # save the business_id
meta(creviews,"user_id")=yelp$user_id   # save the user_id
writeLines(as.character(creviews[1]))  # print specific review

# second let's modify the reviews (removal of certain words)
creviews = tm_map(creviews,stripWhitespace)  # remove extra whitespace
creviews = tm_map(creviews,content_transformer(tolower))  # convert all to lower case
creviews = tm_map(creviews,removeWords,stopwords("english"))  # remove common english words
creviews = tm_map(creviews,removeNumbers)  # remove numbers
creviews = tm_map(creviews,removePunctuation)  # remove symbols
writeLines(as.character(creviews[1]))  # print specific review after transforms

# third let's create a term-document matrix (e.g., count # of times word used)
tmcreviews = DocumentTermMatrix(creviews)
dim(tmcreviews)   # notice that we have one row for each user review, and one column for each word
inspect(tmcreviews[1:10,c("amazing","happy","indian","food")])  # print out word counts for the first 10 users for four specific words

# fourth let's only get the words that are common (alternative could use removeSparseTerms(tmreviews,0.2))
shorttermlist=findFreqTerms(tmcreviews,500)  # let's find words that are used 500 times or more
mcterms = tmcreviews[,shorttermlist]  # let's create another term matrix with just common words
dim(mcterms)   # notice now we have the same number of rows but only 1984 columns (just frequent words)
wordcount=apply(mcterms,1,sum)   # count the number words in reviews (the '1' tells R to sum each row or user)
summary(wordcount)  # summary of the users
dim(mcterms)   # notice that now we only have 109929 reviews


yelp.test = read.csv("yelp_academic_dataset_review_test.csv", header = TRUE)

yelp.test$business_id=as.factor(yelp.test$business_id)  
yelp.test$user_id=as.factor(yelp.test$user_id)

test.creviews = VCorpus(VectorSource(yelp.test$text))
meta(test.creviews,"business_id")=yelp.test$business_id   # save the business_id
meta(test.creviews,"user_id")=yelp.test$user_id   # save the user_id
writeLines(as.character(test.creviews[1]))  # print specific review

test.creviews = tm_map(test.creviews,stripWhitespace)  # remove extra whitespace
test.creviews = tm_map(test.creviews,content_transformer(tolower))  # convert all to lower case
test.creviews = tm_map(test.creviews,removeWords,stopwords("english"))  # remove common english words
test.creviews = tm_map(test.creviews,removeNumbers)  # remove numbers
test.creviews = tm_map(test.creviews,removePunctuation)  # remove symbols
writeLines(as.character(test.creviews[1])) 

test.tmcreviews = DocumentTermMatrix(test.creviews)
dim(test.tmcreviews)   # notice that we have one row for each user review, and one column for each word
inspect(test.tmcreviews[1:10,c("amazing","happy","indian","food")]) 

test.mcterms = test.tmcreviews[, shorttermlist]  # let's create another term matrix with just common words
dim(test.mcterms)   # notice now we have the same number of rows but only 1984 columns (just frequent words)
test.wordcount=apply(test.mcterms,1,sum)   # count the number words in reviews (the '1' tells R to sum each row or user)
summary(test.wordcount)  # summary of the users
dim(test.mcterms)  
test.mcterms = as.matrix(test.mcterms)

dim(mcterms)

