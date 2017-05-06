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
yelp.raw = read.csv("yelp_academic_dataset_review_train.csv", header = TRUE)
dim(yelp.raw)
set.seed(123)
rs = sample(1:116474, size = 116474/4, replace = F)
yelp = yelp.raw[-rs, ]
test = yelp.raw[rs, ]

## make modifications to the dataset

# copy of the data
#yelp = read.csv("data.csv")

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
wordcount.index = wordcount>10
mcterms = mcterms[wordcount.index,]   # let's only keep user reviews that have at least 10 words
dim(mcterms)   # notice that now we only have 109929 reviews
y = yelp$stars[wordcount.index]
mcterms = as.matrix(mcterms)
mcterms = as.data.frame(mcterms)
mcterms = cbind(mcterms, y)
dim(mcterms)

#shrink mcterms
rs1 = sample(1:79984, size = 79984/2, replace = F)
mcterms.shrink= mcterms[-rs1, ]
dim(mcterms.shrink)


#boosting
library(gbm)
gbmModel.adaboost.depth.two<-gbm(y~.,data=mcterms,distribution = "gaussian",
                                 interaction.depth=9,n.trees=100,shrinkage=.01,verbose=FALSE)

test$business_id=as.factor(test$business_id)  
test$user_id=as.factor(test$user_id)

test.creviews = VCorpus(VectorSource(test$text))
meta(test.creviews,"business_id")=test$business_id   # save the business_id
meta(test.creviews,"user_id")=test$user_id   # save the user_id
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
#test.wordcount=apply(test.mcterms,1,sum)   # count the number words in reviews (the '1' tells R to sum each row or user)
#summary(test.wordcount)  # summary of the users
#test.wordcount.index = test.wordcount>10
#test.mcterms = test.mcterms[test.wordcount.index,]   # let's only keep user reviews that have at least 10 words
#dim(test.mcterms)  
test.mcterms = as.data.frame(as.matrix(test.mcterms))

#test.business_id = test$business_id[test.wordcount.index]

gbm.Pred<-predict(gbmModel.adaboost.depth.two,newdata=test.mcterms,
                  type="response",n.trees=100)

###user.stars.regularize is a function to regularize individual user reviews to [-0.5, 6.5] range. 
#The range size can be adjusted later.
user.stars.regularize = function(x) {if(x >= 6.5){return(6.5)} else if (x <= -0.5) {return(-0.5)} else {return(x)}}
prediction.regularized = apply(as.matrix(gbm.Pred), user.stars.regularize, MARGIN = 1)

#business.test = read.csv("yelp_academic_dataset_business_test.csv", header = TRUE)

length(prediction.regularized)
#length(test.business_id)

error = abs(test$stars - prediction.regularized)

sum(error<=0.5)



