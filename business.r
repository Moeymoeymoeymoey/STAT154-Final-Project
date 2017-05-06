train <- read.csv("yelp_academic_dataset_business_train.csv")
test <- read.csv(yelp_academic_dataset_business_test.csv")

starts <- train[,14]
test <- test[,-2]
business <- rbind(train[,-14], test)

pitts_res_unique <- business

library(tibble)
library(dplyr)
library(tidyr)
library(stringr)
library(jsonlite)
library(rpart)
library(RWeka)
library(rpart.plot)
if (!require(tree)) {install.packages("tree"); library(tree)}
# setup environment (if you want to use fancy tree plots)
if (!require(rpart)) {install.packages("rpart"); library(rpart)}
if (!require(rattle)) {install.packages("rattle"); library(rattle)}
if (!require(rpart.plot)) {install.packages("rpart.plot"); library(rpart.plot)}
if (!require(RColorBrewer)) {install.packages("RColorBrewer"); library(RColorBrewer)}
if (!require(party)) {install.packages("party"); library(party)}
if (!require(partykit)) {install.packages("partykit"); library(partykit)}
# a better scatterplot matrix routine
if (!require(car)) {install.packages("car"); library(car)}
# better summary tables
if (!require(psych)) {install.packages("psych"); library(psych)}



#extracting unique category and attributes

categories_test_2 <- as.data.frame((unique(rapply(pitts_res_unique$categories,function(x)unique(x)))))

(unique(rapply(pitts_res_unique$attributes,function(x)unique(x))))
attributes_test_1 <- as.data.frame((unique(rapply(pitts_res_unique$attributes,function(x)unique(x)))))

#export to csv - see pitts_res adding columns with binary variable.xlsx for creation of binary script


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

