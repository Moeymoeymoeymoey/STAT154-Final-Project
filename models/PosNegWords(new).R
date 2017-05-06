library(tm)
library(stringr)
yelp = read.csv("yelp_academic_dataset_review_train.csv", header = TRUE)


# change data formats
yelp$business_id=as.factor(yelp$business_id)  # change from strings to factors -- easier to find common businesses
yelp$user_id=as.factor(yelp$user_id)  # change from strings to factors

## transform customer reviews into a tm structure that can be used for topic modeling

# first turn the reviews which are strings into words (or tokens)
creviews = VCorpus(VectorSource(yelp$text))
meta(creviews,"business_id")=yelp$business_id   # save the business_id
meta(creviews,"user_id")=yelp$user_id   # save the user_id

# second let's modify the reviews (removal of certain words)
creviews = tm_map(creviews,stripWhitespace)  # remove extra whitespace
creviews = tm_map(creviews,content_transformer(tolower))  # convert all to lower case
creviews = tm_map(creviews,removeWords,stopwords("english"))  # remove common english words
creviews = tm_map(creviews,removeNumbers)  # remove numbers
creviews = tm_map(creviews,removePunctuation)  # remove symbols

# third let's create a term-document matrix (e.g., count # of times word used)
tmcreviews = DocumentTermMatrix(creviews)
dim(tmcreviews)   # notice that we have one row for each user review, and one column for each word
inspect(tmcreviews[1:10,c("amazing","happy","indian","food")])  # print out word counts for the first 10 users for four specific words

# fourth let's only get the words that are common (alternative could use removeSparseTerms(tmreviews,0.2))
shorttermlist=findFreqTerms(tmcreviews,500)  # let's find words that are used 500 times or more
mcterms = tmcreviews[,shorttermlist]  # let's create another term matrix with just common words
dim(mcterms)   # notice now we have the same number of rows but only 1984 columns (just frequent words)
mcterms = as.matrix(mcterms)
wordcount=apply(mcterms,1,sum)   # count the number words in reviews (the '1' tells R to sum each row or user)
summary(wordcount)  # summary of the users

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

test.mcterms = as.matrix(test.mcterms)



######START HERE
###Must run codes to get mcterms and test.mcterms before running this script.
###Getting pos and neg words

set.seed(123)
#rs = sample(1:116474, size = 116474/4, replace = F)
TrainOrTest = yelp$business_id %in% business.train.business_id
mcterms.train = mcterms[TrainOrTest, ]
mcterms.test = mcterms[!TrainOrTest, ]
business.id.train = yelp$business_id[TrainOrTest]
business.id.test = yelp$business_id[!TrainOrTest]
stars.train = yelp$stars[TrainOrTest]
stars.test = yelp$stars[!TrainOrTest]

#allReviewstrain = mcterms.train
#allReviewstest = mtest[, 83:ncol(mtest)]


##subsetting
#4and5star
posReviewstrain = mcterms.train[(stars.train >= 4), ]
negReviewstrain = mcterms.train[(stars.train <= 2), ]

posReviewstrain = posReviewstrain[rowSums(posReviewstrain) != 0,]
negReviewstrain = negReviewstrain[rowSums(negReviewstrain) != 0,]

posFreqtrain = prop.table(as.matrix(posReviewstrain), margin = 1)
negFreqtrain = prop.table(as.matrix(negReviewstrain), margin = 1)

posColsumstrain = colSums(posFreqtrain)/nrow(posFreqtrain)
negColsumstrain = colSums(negFreqtrain)/nrow(negFreqtrain)

negWords = names(sort(posColsumstrain - negColsumstrain, decreasing = F))[1:100]
posWords = names(sort(posColsumstrain - negColsumstrain, decreasing = T))[1:100]
PosNegWords = c(posWords, negWords)

###Forming a new dataset
PosNegTrain.X = mcterms.train[,which(colnames(mcterms) %in% PosNegWords)]
PosNegTest.X = mcterms.test[,which(colnames(mcterms) %in% PosNegWords)]

PosNegTrain.y = stars.train
PosNegTest.y = stars.test

PosNegTrain.business.id = business.id.train
PosNegTest.business.id = business.id.test

summary(rowSums(PosNegTrain.X))

PosNegTrain.manywordsindex = rowSums(PosNegTrain.X) >= 9
#PosNegTrain.manywordsindex = sample(1:nrow(PosNegTrain.X), 0.5*nrow(PosNegTrain.X), replace = FALSE)

PosNegTrain.X = PosNegTrain.X[PosNegTrain.manywordsindex,]
PosNegTrain.y = PosNegTrain.y[PosNegTrain.manywordsindex]
PosNegTrain.business.id = PosNegTrain.business.id[PosNegTrain.manywordsindex]

PosNegTrain.data = as.data.frame(PosNegTrain.X)
PosNegTrain.data$stars = PosNegTrain.y
PosNegTest.data = as.data.frame(PosNegTest.X)


###Try boosting
library(gbm)
gbmModel.PosNeg<-gbm(stars~.,data=PosNegTrain.data, distribution = "tdist",
                                  interaction.depth=4, n.trees=300,shrinkage=.01,verbose=FALSE)

gbm.Pred<-predict(gbmModel.PosNeg, newdata=PosNegTest.data,
                  type="response",n.trees=300)

hist(gbm.Pred)

user.stars.regularize = function(x) {if(x >= 5){return(5)} else if (x <= 1) {return(1)} else {return(round(x))}}
gbm.Pred.regularized = apply(matrix(gbm.Pred,ncol=1), user.stars.regularize, MARGIN = 1)
gbmEPE = mean((gbm.Pred.regularized - PosNegTest.y)^2)


###Try random Forest
library(randomForest)
rfModel<-randomForest(PosNegTrain.X, as.factor(PosNegTrain.y), ntree=500)
rfPredictions<-predict(rfModel,newdata=PosNegTest.X)
rfPredictions = as.numeric(rfPredictions)

rfEPE = mean((rfPredictions - PosNegTest.y)^2)

##Average
#avgPredictions = (gbm.Pred + rfPredictions)/2
#avg.Pred.regularized = apply(matrix(avgPredictions,ncol=1), user.stars.regularize, MARGIN = 1)
#avgEPE = mean((avg.Pred.regularized - PosNegTest.y)^2)
#hist(avg.Pred.regularized)



###Try KNN

#####################
negWords = names(sort(posColsumstrain - negColsumstrain, decreasing = F))[1:15]
posWords = names(sort(posColsumstrain - negColsumstrain, decreasing = T))[1:15]
PosNegWords = c(posWords, negWords)

###Forming a new dataset
PosNegTrain.X = mcterms.train[,which(colnames(mcterms) %in% PosNegWords)]
PosNegTest.X = mcterms.test[,which(colnames(mcterms) %in% PosNegWords)]

PosNegTrain.y = stars.train
PosNegTest.y = stars.test

PosNegTrain.business.id = business.id.train
PosNegTest.business.id = business.id.test

summary(rowSums(PosNegTrain.X))

PosNegTrain.manywordsindex = rowSums(PosNegTrain.X) >= 0
#PosNegTrain.manywordsindex = sample(1:nrow(PosNegTrain.X), 0.5*nrow(PosNegTrain.X), replace = FALSE)

PosNegTrain.X = PosNegTrain.X[PosNegTrain.manywordsindex,]
PosNegTrain.y = PosNegTrain.y[PosNegTrain.manywordsindex]
PosNegTrain.business.id = PosNegTrain.business.id[PosNegTrain.manywordsindex]

PosNegTrain.data = data.frame(as.data.frame(PosNegTrain.X), stars = PosNegTrain.y)
PosNegTest.data = as.data.frame(PosNegTest.X)
#####################

#install.packages("FNN")
library(FNN)
knnModel.10 = knn.reg(PosNegTrain.X, test = PosNegTest.X, PosNegTrain.y, k = 10, algorithm = "brute")
knn.10.Predictions = knnModel.10$pred
knn.10.Pred.regularized = apply(matrix(knn.10.Predictions,ncol=1), user.stars.regularize, MARGIN = 1)
knn.10.EPE = mean((knn.10.Pred.regularized - PosNegTest.y)^2)



###Try LDA
library(MASS)
lda.model = lda(as.factor(stars)~., data = PosNegTrain.data)
lda.prediction<-predict(lda.model,PosNegTest.data)$class
lda.prediction = as.numeric(lda.prediction)
hist(lda.prediction)
lda.EPE = mean((lda.prediction - PosNegTest.y)^2)


###Try Bagging
library(ipred)
bagging.model = bagging(stars~., data = PosNegTrain.data, nbagg=50)
bagging.model.predictions = predict(bagging.model, newdata = PosNegTest.data)
hist(bagging.model.predictions)
bagging.model.predictions.regularized = apply(matrix(bagging.model.predictions,ncol=1), user.stars.regularize, MARGIN = 1)
bagging.EPE = mean((bagging.model.predictions.regularized - PosNegTest.y)^2)
###Lastly, output a dataframe with 2 columns, 1st column has train and test business id, 
###2nd column has train true review stars and test predicted review stars
#Random Forest
dfrf = data.frame(business_id = as.character(PosNegTest.business.id), 
                  stars =  rfPredictions)

##Do the same for training set
dfrf.train = data.frame(business_id = as.character(PosNegTrain.business.id),
                        stars = PosNegTrain.y)


#######We want to do it on the REAL test set
PosNegFinal.X = test.mcterms[,which(colnames(test.mcterms) %in% PosNegWords)]
PosNegFinal.business.id = yelp.test$business_id
rfPredictions.Final<-predict(rfModel,newdata=PosNegFinal.X)
rfPredictions.Final = as.numeric(rfPredictions.Final)
hist(rfPredictions.Final)

dfrf_Final = data.frame(business_id = as.character(PosNegFinal.business.id), 
                  stars =  rfPredictions.Final)
