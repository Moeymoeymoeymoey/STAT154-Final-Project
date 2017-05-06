###Business Score Prediction
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
rindex = sample(1:nrow(pitts_res_unique.train), round(nrow(pitts_res_unique.train)*0.7), replace = FALSE)
pitts_res_unique.train.train = pitts_res_unique.train[rindex,]
pitts_res_unique.train.test = pitts_res_unique.train[-rindex,]

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

pitts_res_unique.train.train.review_scores_avg = apply(pitts_res_unique.train.train, get_review_scores_avg, dfany = dfrf, MARGIN = 1)
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
