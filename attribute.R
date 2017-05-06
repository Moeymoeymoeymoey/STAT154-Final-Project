business.train = read.csv("yelp_academic_dataset_business_train.csv", header = TRUE)
library("qdapRegex")
attributes <- business.train$attributes
rm_all_except_letters <- rm_(pattern="[^ a-zA-Z]")
attributes.new <- rm_all_except_letters(attributes)
attri_Alcohol <- rm_default(attributes.new, pattern = S("@after_", "Alcohol", 1), extract=TRUE)
attri_amb_romantic <- rm_default(attributes.new, pattern = S("@after_", "romantic", 1), extract=TRUE)
attri_amb_intimate <- rm_default(attributes.new, pattern = S("@after_", "intimate", 1), extract=TRUE)
attri_amb_romantic <- rm_default(attributes.new, pattern = S("@after_", "classy", 1), extract=TRUE)
attri_amb_hipster <- rm_default(attributes.new, pattern = S("@after_", "hipster", 1), extract=TRUE)
attri_amb_divey <- rm_default(attributes.new, pattern = S("@after_", "divey", 1), extract=TRUE)
attri_amb_touristy <- rm_default(attributes.new, pattern = S("@after_", "touristy", 1), extract=TRUE)
attri_amb_trendy <- rm_default(attributes.new, pattern = S("@after_", "trendy", 1), extract=TRUE)
attri_amb_upscale <- rm_default(attributes.new, pattern = S("@after_", "upscale", 1), extract=TRUE)
attri_amb_casual <- rm_default(attributes.new, pattern = S("@after_", "casual", 1), extract=TRUE)
attri_BYOB <- rm_default(attributes.new, pattern = S("@after_", "BYOB", 1), extract=TRUE)
attri_BYOBCorkage <- rm_default(attributes.new, pattern = S("@after_", "BYOBCorkage", 1), extract=TRUE)
attri_BikeParking <- rm_default(attributes.new, pattern = S("@after_", "BikeParking", 1), extract=TRUE)
attri_BusinessAcceptsBitcoin <- rm_default(attributes.new, pattern = S("@after_", "BusinessAcceptsBitcoin", 1), extract=TRUE)
attri_BusinessAcceptsCreditCards <- rm_default(attributes.new, pattern = S("@after_", "BusinessAcceptsCreditCards", 1), extract=TRUE)
attri_BusiParking_garage <- rm_default(attributes.new, pattern = S("@after_", "garage", 1), extract=TRUE)
attri_BusiParking_street <- rm_default(attributes.new, pattern = S("@after_", "street", 1), extract=TRUE)
attri_BusiParking_validated <- rm_default(attributes.new, pattern = S("@after_", "validated", 1), extract=TRUE)
attri_BusiParking_lot <- rm_default(attributes.new, pattern = S("@after_", "lot", 1), extract=TRUE)
attri_BusiParking_valet <- rm_default(attributes.new, pattern = S("@after_", "valet", 1), extract=TRUE)
attri_ByAppointmentOnly <- rm_default(attributes.new, pattern = S("@after_", "ByAppointmentOnly", 1), extract=TRUE)
attri_Caters <- rm_default(attributes.new, pattern = S("@after_", "Caters", 1), extract=TRUE)
attri_Corkage <- rm_default(attributes.new, pattern = S("@after_", "Corkage", 1), extract=TRUE)
attri_DogsAllowed <- rm_default(attributes.new, pattern = S("@after_", "DogsAllowed", 1), extract=TRUE)
attri_GoodForKids <- rm_default(attributes.new, pattern = S("@after_", "GoodForKids", 1), extract=TRUE)
attri_GoodForMeal_dessert <- rm_default(attributes.new, pattern = S("@after_", "dessert", 1), extract=TRUE)
attri_GoodForMeal_latenight <- rm_default(attributes.new, pattern = S("@after_", "latenight", 1), extract=TRUE)
attri_GoodForMeal_lunch <- rm_default(attributes.new, pattern = S("@after_", "lunch", 1), extract=TRUE)
attri_GoodForMeal_dinner <- rm_default(attributes.new, pattern = S("@after_", "dinner", 1), extract=TRUE)
attri_GoodForMeal_breakfast <- rm_default(attributes.new, pattern = S("@after_", "breakfast", 1), extract=TRUE)
attri_GoodForMeal_brunch <- rm_default(attributes.new, pattern = S("@after_", "brunch", 1), extract=TRUE)
attri_HasTV <- rm_default(attributes.new, pattern = S("@after_", "HasTV", 1), extract=TRUE)
attri_NoiseLevel <- rm_default(attributes.new, pattern = S("@after_", "NoiseLevel", 1), extract=TRUE)
attri_Open24Hours <- rm_default(attributes.new, pattern = S("@after_", "Open24Hours", 1), extract=TRUE)
attri_OutdoorSeating <- rm_default(attributes.new, pattern = S("@after_", "OutdoorSeating", 1), extract=TRUE)
attri_RestaurantsAttire <- rm_default(attributes.new, pattern = S("@after_", "RestaurantsAttire", 1), extract=TRUE)
attri_RestaurantsCounterService <- rm_default(attributes.new, pattern = S("@after_", "RestaurantsCounterService", 1), extract=TRUE)
attri_RestaurantsDelivery <- rm_default(attributes.new, pattern = S("@after_", "RestaurantsDelivery", 1), extract=TRUE)
attri_RestaurantsGoodForGroups <- rm_default(attributes.new, pattern = S("@after_", "RestaurantsGoodForGroups", 1), extract=TRUE)
attri_RestaurantsPriceRange2 <- rm_default(attributes.new, pattern = S("@after_", "RestaurantsPriceRange2", 1), extract=TRUE)
attri_RestaurantsReservations <- rm_default(attributes.new, pattern = S("@after_", "RestaurantsReservations", 1), extract=TRUE)
attri_RestaurantsTableService <- rm_default(attributes.new, pattern = S("@after_", "RestaurantsTableService", 1), extract=TRUE)
attri_RestaurantsTakeOut <- rm_default(attributes.new, pattern = S("@after_", "RestaurantsTakeOut", 1), extract=TRUE)
attri_WheelchairAccessible <- rm_default(attributes.new, pattern = S("@after_", "WheelchairAccessible", 1), extract=TRUE)
attri_WiFi <- rm_default(attributes.new, pattern = S("@after_", "WiFi", 1), extract=TRUE)
attri_DietaryRestrictions_dairyfree <- rm_default(attributes.new, pattern = S("@after_", "dairyfree", 1), extract=TRUE)
attri_DietaryRestrictions_glutenfree <- rm_default(attributes.new, pattern = S("@after_", "glutenfree", 1), extract=TRUE)
attri_DietaryRestrictions_vegan <- rm_default(attributes.new, pattern = S("@after_", "vegan", 1), extract=TRUE)
attri_DietaryRestrictions_kosher <- rm_default(attributes.new, pattern = S("@after_", "kosher", 1), extract=TRUE)
attri_DietaryRestrictions_halal <- rm_default(attributes.new, pattern = S("@after_", "halal", 1), extract=TRUE)
attri_DietaryRestrictions_soyfree <- rm_default(attributes.new, pattern = S("@after_", "soyfree", 1), extract=TRUE)
attri_DietaryRestrictions_vegetarian <- rm_default(attributes.new, pattern = S("@after_", "vegetarian", 1), extract=TRUE)













