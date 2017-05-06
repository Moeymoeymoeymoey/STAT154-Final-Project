# setup libraries
if (!require(lattice)) {install.packages("lattice"); library(lattice)}
if (!require(NLP)) {install.packages("NLP"); library(NLP)}
if (!require(topicmodels)) {install.packages("topicmodels"); library(topicmodels)}
if (!require(tm)) {install.packages("tm"); library(tm)}
if (!require(slam)) {install.packages("slam"); library(slam)}

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

load("+-wordstextmining.Rdata")

library(stringr)
train <- read.csv("yelp_academic_dataset_business_train.csv", header = TRUE)
test <- read.csv("yelp_academic_dataset_business_test.csv", header = TRUE)

stars = train[,14]
test <- test[,-2]
business <- rbind(train[,-14], test)

pitts_res_unique <- business
#binary variable indicating whether category or attribute is present
pitts_res_unique$Category.Bars <- as.integer(str_detect(pitts_res_unique$categories, "Bars"))
pitts_res_unique$Category.American__New_ <- as.integer(str_detect(pitts_res_unique$categories, "American (New)"))
pitts_res_unique$Category.Sandwiches <- as.integer(str_detect(pitts_res_unique$categories, "Sandwiches"))
pitts_res_unique$Category.Mexican <- as.integer(str_detect(pitts_res_unique$categories, "Mexican"))
pitts_res_unique$Category.Japanese <- as.integer(str_detect(pitts_res_unique$categories, "Japanese"))
pitts_res_unique$Category.Sushi_Bars <- as.integer(str_detect(pitts_res_unique$categories, "Sushi Bars"))
pitts_res_unique$Category.American__Traditional_ <- as.integer(str_detect(pitts_res_unique$categories, "American (Traditional)"))
pitts_res_unique$Category.Specialty_Food <- as.integer(str_detect(pitts_res_unique$categories, "Specialty Food"))
pitts_res_unique$Category.Local_Flavor <- as.integer(str_detect(pitts_res_unique$categories, "Local Flavor"))
pitts_res_unique$Category.Italian <- as.integer(str_detect(pitts_res_unique$categories, "Italian"))
pitts_res_unique$Category.Pizza <- as.integer(str_detect(pitts_res_unique$categories, "Pizza"))
pitts_res_unique$Category.Cafes <- as.integer(str_detect(pitts_res_unique$categories, "Cafes"))
pitts_res_unique$Category.Fast_Food <- as.integer(str_detect(pitts_res_unique$categories, "Fast Food"))
pitts_res_unique$Category.Breakfast___Brunch <- as.integer(str_detect(pitts_res_unique$categories, "Breakfast & Brunch"))
pitts_res_unique$Category.Donuts <- as.integer(str_detect(pitts_res_unique$categories, "Donuts"))
pitts_res_unique$Category.Coffee___Tea <- as.integer(str_detect(pitts_res_unique$categories, "Coffee & Tea"))
pitts_res_unique$Category.Seafood <- as.integer(str_detect(pitts_res_unique$categories, "Seafood"))
pitts_res_unique$Category.Bakeries <- as.integer(str_detect(pitts_res_unique$categories, "Bakeries"))
pitts_res_unique$Category.Barbeque <- as.integer(str_detect(pitts_res_unique$categories, "Barbeque"))
pitts_res_unique$Category.Vegan <- as.integer(str_detect(pitts_res_unique$categories, "Vegan"))
pitts_res_unique$Category.Indian <- as.integer(str_detect(pitts_res_unique$categories, "Indian"))
pitts_res_unique$Category.Chinese <- as.integer(str_detect(pitts_res_unique$categories, "Chinese"))
pitts_res_unique$Category.Vegetarian <- as.integer(str_detect(pitts_res_unique$categories, "Vegetarian"))
pitts_res_unique$Category.Latin_American <- as.integer(str_detect(pitts_res_unique$categories, "Latin American"))
pitts_res_unique$Category.Delis <- as.integer(str_detect(pitts_res_unique$categories, "Delis"))
pitts_res_unique$Category.Mediterranean <- as.integer(str_detect(pitts_res_unique$categories, "Mediterranean"))
pitts_res_unique$Category.Greek <- as.integer(str_detect(pitts_res_unique$categories, "Greek"))
pitts_res_unique$Category.Middle_Eastern <- as.integer(str_detect(pitts_res_unique$categories, "Middle Eastern"))
pitts_res_unique$Category.Desserts <- as.integer(str_detect(pitts_res_unique$categories, "Desserts"))
pitts_res_unique$Category.Food_Trucks <- as.integer(str_detect(pitts_res_unique$categories, "Food Trucks"))
pitts_res_unique$Category.Burgers <- as.integer(str_detect(pitts_res_unique$categories, "Burgers"))
pitts_res_unique$Category.Hot_Dogs <- as.integer(str_detect(pitts_res_unique$categories, "Hot Dogs"))
pitts_res_unique$Category.Steakhouses <- as.integer(str_detect(pitts_res_unique$categories, "Steakhouses"))
pitts_res_unique$Category.Soup <- as.integer(str_detect(pitts_res_unique$categories, "Soup"))
pitts_res_unique$Category.Noodles <- as.integer(str_detect(pitts_res_unique$categories, "Noodles"))
pitts_res_unique$Category.Buffets <- as.integer(str_detect(pitts_res_unique$categories, "Buffets"))
pitts_res_unique$Category.Thai <- as.integer(str_detect(pitts_res_unique$categories, "Thai"))
pitts_res_unique$Category.Cafeteria <- as.integer(str_detect(pitts_res_unique$categories, "Cafeteria"))
pitts_res_unique$Category.Diners <- as.integer(str_detect(pitts_res_unique$categories, "Diners"))
pitts_res_unique$Category.Korean <- as.integer(str_detect(pitts_res_unique$categories, "Korean"))
pitts_res_unique$Category.French <- as.integer(str_detect(pitts_res_unique$categories, "French"))
pitts_res_unique$Category.Tex_Mex <- as.integer(str_detect(pitts_res_unique$categories, "Tex-Mex"))
pitts_res_unique$Category.Caribbean <- as.integer(str_detect(pitts_res_unique$categories, "Caribbean"))
pitts_res_unique$Category.Beer <- as.integer(str_detect(pitts_res_unique$categories, "Beer"))
pitts_res_unique$Category.Ramen <- as.integer(str_detect(pitts_res_unique$categories, "Ramen"))
pitts_res_unique$Category.Vietnamese <- as.integer(str_detect(pitts_res_unique$categories, "Vietnamese"))
pitts_res_unique$Category.Spanish <- as.integer(str_detect(pitts_res_unique$categories, "Spanish"))
pitts_res_unique$Category.Taiwanese <- as.integer(str_detect(pitts_res_unique$categories, "Taiwanese"))
pitts_res_unique$Category.Fish___Chips <- as.integer(str_detect(pitts_res_unique$categories, "Fish & Chips"))
pitts_res_unique$Category.Tacos <- as.integer(str_detect(pitts_res_unique$categories, "Tacos"))
pitts_res_unique$Category.Hot_Pot <- as.integer(str_detect(pitts_res_unique$categories, "Hot Pot"))
pitts_res_unique$Category.Cantonese <- as.integer(str_detect(pitts_res_unique$categories, "Cantonese"))
pitts_res_unique$Attribute.BusinessAcceptsCreditCards__True <- as.integer(str_detect(pitts_res_unique$attributes, "BusinessAcceptsCreditCards: True"))
pitts_res_unique$Attribute.ByAppointmentOnly__True <- as.integer(str_detect(pitts_res_unique$attributes, "ByAppointmentOnly: True"))
pitts_res_unique$Attribute.DogsAllowed__True <- as.integer(str_detect(pitts_res_unique$attributes, "DogsAllowed: True"))
pitts_res_unique$Attribute.DriveThru__True <- as.integer(str_detect(pitts_res_unique$attributes, "DriveThru: True"))
pitts_res_unique$Attribute.GoodForDancing__True <- as.integer(str_detect(pitts_res_unique$attributes, "GoodForDancing: True"))
pitts_res_unique$Attribute.GoodForKids__True <- as.integer(str_detect(pitts_res_unique$attributes, "GoodForKids: True"))
pitts_res_unique$Attribute.HappyHour__True <- as.integer(str_detect(pitts_res_unique$attributes, "HappyHour: True"))
pitts_res_unique$Attribute.HasTV__True <- as.integer(str_detect(pitts_res_unique$attributes, "HasTV: True"))
pitts_res_unique$Attribute.Open24Hours__True <- as.integer(str_detect(pitts_res_unique$attributes, "Open24Hours: True"))
pitts_res_unique$Attribute.OutdoorSeating__True <- as.integer(str_detect(pitts_res_unique$attributes, "OutdoorSeating: True"))
pitts_res_unique$Attribute.RestaurantsAttire__casual <- as.integer(str_detect(pitts_res_unique$attributes, "RestaurantsAttire: casual"))
pitts_res_unique$Attribute.RestaurantsAttire__dressy <- as.integer(str_detect(pitts_res_unique$attributes, "RestaurantsAttire: dressy"))
pitts_res_unique$Attribute.RestaurantsAttire__formal <- as.integer(str_detect(pitts_res_unique$attributes, "RestaurantsAttire: formal"))
pitts_res_unique$Attribute.RestaurantsCounterService__True <- as.integer(str_detect(pitts_res_unique$attributes, "RestaurantsCounterService: True"))
pitts_res_unique$Attribute.RestaurantsDelivery__True <- as.integer(str_detect(pitts_res_unique$attributes, "RestaurantsDelivery: True"))
pitts_res_unique$Attribute.RestaurantsGoodForGroups__True <- as.integer(str_detect(pitts_res_unique$attributes, "RestaurantsGoodForGroups: True"))
pitts_res_unique$Attribute.RestaurantsPriceRange2__1 <- as.integer(str_detect(pitts_res_unique$attributes, "RestaurantsPriceRange2: 1"))
pitts_res_unique$Attribute.RestaurantsPriceRange2__2 <- as.integer(str_detect(pitts_res_unique$attributes, "RestaurantsPriceRange2: 2"))
pitts_res_unique$Attribute.RestaurantsPriceRange2__3 <- as.integer(str_detect(pitts_res_unique$attributes, "RestaurantsPriceRange2: 3"))
pitts_res_unique$Attribute.RestaurantsPriceRange2__4 <- as.integer(str_detect(pitts_res_unique$attributes, "RestaurantsPriceRange2: 4"))
pitts_res_unique$Attribute.RestaurantsReservations__True <- as.integer(str_detect(pitts_res_unique$attributes, "RestaurantsReservations: True"))
pitts_res_unique$Attribute.RestaurantsTableService__True <- as.integer(str_detect(pitts_res_unique$attributes, "RestaurantsTableService: True"))
pitts_res_unique$Attribute.RestaurantsTakeOut__True <- as.integer(str_detect(pitts_res_unique$attributes, "RestaurantsTakeOut: True"))
pitts_res_unique$Attribute.Smoking__yes <- as.integer(str_detect(pitts_res_unique$attributes, "Smoking: yes"))
pitts_res_unique$Attribute.WiFi__free <- as.integer(str_detect(pitts_res_unique$attributes, "WiFi: free"))
pitts_res_unique$Attribute.BusinessParking_street <- as.integer(str_detect(pitts_res_unique$attributes, "'street': True"))
pitts_res_unique$Attribute.Music_dj <- as.integer(str_detect(pitts_res_unique$attributes, "'dj': True"))

#Convert Neighborhood variables from character to factor
#colnames(pitts_res_unique)
pitts_res_unique$neighborhood <- as.factor(pitts_res_unique$neighborhood)
#levels(pitts_res_unique$neighborhood)

#Divide data into training and testing
pitts_res_unique.train <- pitts_res_unique[1:nrow(train), ]              #get training set
pitts_res_unique.test <- pitts_res_unique[-c(1:nrow(train)), ]    #get test set
pitts_res_unique.train$stars = stars

pitts_res_unique.train <- pitts_res_unique.train[,-c(1,2,4,5,6,7,8,9,11,12,13,14,1,16)]
pitts_res_unique.test <- pitts_res_unique.test[,-c(1,2,4,5,6,7,8,9,11,12,13,14,1,16)]


##Split train further into 70% train and 30% test
set.seed(123)
rindex = sample(1:nrow(pitts_res_unique.train), round(nrow(pitts_res_unique.train)*0.7), replace = FALSE)
pitts_res_unique.train.train = pitts_res_unique.train[rindex,]
pitts_res_unique.train.test = pitts_res_unique.train[-rindex,]

business.train.business_id = pitts_res_unique.train.train$business_id
business.test.business_id = pitts_res_unique.train.test$business_id


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

PosNegTrain.data = data.frame(as.data.frame(PosNegTrain.X))
PosNegTrain.data$stars = PosNegTrain.y
PosNegTest.data = as.data.frame(PosNegTest.X)




#load("+wwordsnewdataset.Rdata")
###Try Rpart
library(rpart)
library(rpart.plot)
modeltree <- rpart(stars~., data=PosNegTrain.data)
rpart.plot(modeltree)
rpartpredictions <- predict(modeltree,PosNegTest.data)
rpartPredictions = as.numeric(rpartpredictions)

rfEPE = mean((rpartPredictions - PosNegTest.y)^2)



rpPredictions = as.numeric(rpPredictions)

###Try random Forest
library(randomForest)
rfModel<-randomForest(PosNegTrain.X, as.factor(PosNegTrain.y), ntree=1000)
rfPredictions<-predict(rfModel,newdata=PosNegTest.X)
rfPredictions = as.numeric(rfPredictions)

rfEPE = mean((rfPredictions - PosNegTest.y)^2)




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

#get_review_scores_avg is a function that takes in a vector like any row of pitts_res_unique.train.train
#and also takes in a data frame like dfrf having two columns, one is business_id and the other is review stars
#it looks for first element of the vector which is a business id, and find the mean review score of this business id
get_review_scores_avg = function(x, dfany) {
  business_id = x[1]
  if (all(dfany[,1] != business_id)) {
    return(3.5)
  } else {
    return(mean(dfany[which(dfany[,1] == business_id),2]))
  }
}

pitts_res_unique.train.train.review_scores_avg = apply(pitts_res_unique.train.train, get_review_scores_avg, dfany = dfrf.train, MARGIN = 1)
pitts_res_unique.train.test.review_scores_avg = apply(pitts_res_unique.train.test, get_review_scores_avg, dfany = dfrf, MARGIN = 1)

pitts_res_unique.train.train.data = data.frame(review_scores_avg = pitts_res_unique.train.train.review_scores_avg,
                                               pitts_res_unique.train.train[,-1])
pitts_res_unique.train.train.business_id = pitts_res_unique.train.train$business_id
pitts_res_unique.train.test.data = data.frame(review_scores_avg = pitts_res_unique.train.test.review_scores_avg,
                                              pitts_res_unique.train.test[,-c(1, ncol(pitts_res_unique.train.test))])
pitts_res_unique.train.test.business_id = pitts_res_unique.train.test$business_id

#####Apply Models
###Try boosting
library(gbm)
gbmModel.BusinessScore<-gbm(stars~.,data=pitts_res_unique.train.train.data, distribution = "tdist",
                            interaction.depth=4, n.trees=100,shrinkage=.11,verbose=FALSE)

gbm.Pred.BusinessScore<-predict(gbmModel.BusinessScore, newdata=pitts_res_unique.train.test.data,
                                type="response",n.trees=100)

hist(gbm.Pred.BusinessScore)

business.score.regularize = function(x) {if (is.na(x)) {return (3.5)} else if(x >= 5){return(5)} else if (x <= 1) {return(1)} else {return(round(x*2)/2)}}
gbm.Pred.BusinessScore.regularized = apply(matrix(gbm.Pred.BusinessScore,ncol=1), business.score.regularize, MARGIN = 1)
gbmBusinessScoreEPE = mean((gbm.Pred.BusinessScore.regularized - pitts_res_unique.train.test$stars)^2)




###Final Prediction
pitts_res_unique.test.review_scores_avg = apply(pitts_res_unique.test, get_review_scores_avg, dfany = dfrf_Final, MARGIN = 1)
pitts_res_unique.test.data = data.frame(review_scores_avg =pitts_res_unique.test.review_scores_avg, pitts_res_unique.test[,-1])
gbm.Pred.Final<-predict(gbmModel.BusinessScore, newdata=pitts_res_unique.test.data,
                        type="response",n.trees=100)
hist(gbm.Pred.Final)
gbm.Pred.Final.regularized = apply(matrix(gbm.Pred.Final,ncol=1), business.score.regularize, MARGIN = 1)
hist(gbm.Pred.Final.regularized)

Final_Answer = data.frame(business_id = pitts_res_unique.test$business_id, stars = gbm.Pred.Final.regularized)

