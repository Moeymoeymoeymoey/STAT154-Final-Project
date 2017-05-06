train <- read.csv("yelp_academic_dataset_business_train.csv")
test <- read.csv("yelp_academic_dataset_business_test.csv")

stars = train[,14]
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
pitts_res_unique.train$stars = stars



##

# setup libraries
if (!require(lattice)) {install.packages("lattice"); library(lattice)}
if (!require(NLP)) {install.packages("NLP"); library(NLP)}
if (!require(topicmodels)) {install.packages("topicmodels"); library(topicmodels)}
if (!require(tm)) {install.packages("tm"); library(tm)}
if (!require(slam)) {install.packages("slam"); library(slam)}

##################### input the data  ######################

## read in the data

library(readr)

yelp = read_csv("C:/Users/Administrator/Desktop/gg/yelp/yelp_academic_dataset_review_train.csv")


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
wordcount=apply(mcterms,1,sum)   # count the number words in reviews (the '1' tells R to sum each row or user)
summary(wordcount)  # summary of the users

yelp.test = read_csv("C:/Users/Administrator/Desktop/gg/yelp/yelp_academic_dataset_review_test.csv")

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
##


#load("testtrain.Rdata")

mcterms = as.matrix(mcterms)

mcdf = data.frame(business_id = yelp$business_id, mcterms)
#mcdf = data.frame(business_id = yelp$business_id, stars = yelp$stars, mcterms)

pitts_res_unique.train <- pitts_res_unique.train[,-c(1,2,4,5,6,7,8,9,11,12,13,14,1,16)]
pitts_res_unique.test <- pitts_res_unique.test[,-c(1,2,4,5,6,7,8,9,11,12,13,14,1,16)]


mtrain = merge(pitts_res_unique.train, mcdf, by = "business_id")

mcdftest = data.frame(business_id = yelp.test$business_id, test.mcterms)

mtest = merge(pitts_res_unique.test, mcdftest, by = "business_id")


#regression tree

#modeltree <- rpart(stars~ able+absolute+absolutely+accommodating+across+actual+actually+add+added+addition+additional+admit+adobada+affordable+afternoon+ago+agree+ahead+air+airport+alcohol+almond+almost+alone+along+already+alright+also+although+always+amazing+ambiance+ambience+american+americanized+amount+annoying+another+answer+anymore+anyone+anything+anyway+anyways+anywhere+apart+app+apparently+appetizer+appetizers+appreciate+area+arizona+around+arrive+arrived+art+asada+asian+aside+ask+asked+asking+ass+ate+atmosphere+attention+attentive+attitude+authentic+available+ave+average+avocado+avoid+away+awesome+awful+awhile+baby+back+bacon+bad+bag+baja+baked+bar+barbacoa+barely+barista+baristas+barrio+bartender+based+basic+basically+basil+bathroom+bbq+bean+beans+beat+beautiful+become+beef+beer+beers+behind+believe+bell+besides+best+better+beverage+beyond+big+bigger+biggest+bill+birthday+bit+bite+bites+bitter+black+blah+bland+blend+blended+blue+bomb+bonus+book+booth+boring+bother+bottle+bottom+bought+bowl+box+boy+boyfriend+bread+break.+breakfast+brew+brick+bright+bring+broccoli+bros+broth+brought+brown+bucks+buffalo+buffet+building+bunch+burger+burnt+burrito+burritos+burro+business+busy+butter+buy+cabbage+cafe+caffeine+cake+california+call+called+calling+calzone+came+can+cant+car+caramel+card+cards+care+carne+carnitas+carrots+carry+cars+case+cash+cashier+casino+casual+catch+cause+center+central+certain+certainly+ceviche+chai+chain+chains+chairs+chance+change+changed+charge+charged+charlotte+cheap+cheaper+check+checked+checking+cheese+cheesy+chef+chewy+chicago+chicken+chile+chili+chill+chimi+chimichanga+china+chinese+chip+chipotle+chips+chocolate+choice+choices+choose+chorizo+chose+chow+chunks+cilantro+cinnamon+city+classic+clean+clear+clearly+cleveland+close+closed+closer+closest+closing+club+coast+coconut+coffee+coffees+coke+cold+college+combination+combo+come+comes+comfortable+coming+company+compare+compared+complain+complaint+complaints+complete+completely+complimentary+con+concept+consider+considering+consistency+consistent+consistently+container+continue+convenient+conversation+cook+cooked+cookie+cooking+cooks+cool+corn+corner+correct+cosmo+cosmopolitan+cost+counter+couple+coupon+course+courteous+covered+coworker+coworkers+cozy+crab+crap+craving+crazy+cream+creamy+credit+crisp+crispy+crowd+crowded+crunchy+crust+cuisine+cup+cups+customer+customers+cut+cute+daily+damn+dark+date+daughter+day+days+deal+decent+decide+decided+decor+deep+definitely+del+delicious+delish+deliver+delivered+delivery+desert+despite+dessert+didnt+die+difference+different+difficult+dine+dining+dinner+dip+dirty+disappoint+disappointed+disappointing+disappointment+discovered+disgusting+dish+dishes+dog+dollar+dollars+done+dont+door+double+doubt+dough+downtown+dozen+dressing+dried+drink+drinking+drinks+drive+driver+drivethru+driving+drop+dropped+drove+drunk+dry+duck+due+dumplings+dutch+earlier+early+easily+east+easy+eat+eaten+eating+edible+efficient+egg+eggplant+eggs+either+else.+elsewhere+employee+employees+empty+enchilada+enchiladas+end+ended+english+enjoy+enjoyed+enjoying+enough+entire+entrance+entree+entrees+environment+especially+espresso+establishment+etc+even+evening+ever+every+everyday+everyone+everything+everywhere+exactly+excellent+except+exceptional+excited+expect+expectations+expected+expecting+expensive+experience+experienced+experiences+explain+explained+express+extra+extremely+eye+eyes+fabulous+face+fact+fair+fairly+fajita+fajitas+family+fan+fancy+fantastic+far+fare+fast+fat+fault+favor+favorite+favorites+feed+feel+feeling+feels+fell+felt+figure+figured+filibertos+fill+filled+filling+finally+find+finding+fine+finish+finished+fire+first+fish+five+fix+flat+flavor+flavored+flavorful+flavorless+flavors+flight+floor+flour+folks+food+foods+forever+forget+forgot+forward+found+fountain+four+free+french+frequent+fresh+freshly+friday+fried+friend+friendly+friends+fries+front+frozen+fruit+full+fun+fundido+funny+future+gallo+game+garlic+gave+gem+general+generally+generous+get+gets+getting+giant+girl+girlfriend+girls+give+given+gives+giving+glad+glass+glasses+gluten+god+goes+going+gone+gonna+good+goodness+gordo+got+gotta+gotten+gourmet+grab+grease+greasy+great+green+greeted+grew+grill+grilled+grimaldis+gross+ground+group+groupon+guac+guacamole+guess+guests+guy+guys+hair+half+hallway+hand+handed+hands+hang+hanging+happen+happened+happy+hard+hardly+hate+head+healthy+hear+heard+heart+heat+heaven+heavy+hell+help+helped+helpful+henderson+hey+hidden+high+higher+highly+hit+hold+hole+home+homemade+honest+honestly+honey+hope+hopefully+hoping+horchata+horrible+hostess+hot+hotel+hour+hours+house+however+hubby+huge+hungry+hurry+husband+hut+hype+ice+iced+idea+imagine+immediately+important+impressed+impressive+include+included+including+incredible+incredibly+inexpensive+ingredients+inside+instead+interesting+interior+issue+issues+italian+item+items+ive+jalapeno+job+johns+joint+joints+joke+juice+juicy+just+keep+kept+kick+kid+kids+kind+kinda+kinds+kitchen+knew+knots+know+known+knows+kung+lack+lacked+lacking+ladies+lady+large+larger+las+last+late+later+latte+lattes+lazy+least+leave+leaving+left+leftovers+legit+lemon+lengua+less+let+lettuce+level+life+light+like+liked+likely+likes+lime+limited+line+lines+list+literally+little+live+lived+living+loaded+lobster+local+located+location+locations+lol+long+longer+look+looked+looking+looks+los+lost+lot+lots+loud+love+loved+lovely+loves+low+luck+luckily+lucky+lunch+macayos+machaca+machine+made+madison+main+major+make+makes+making+mall+man+management+manager+mango+many+margarita+margaritas+market+matter+may+maybe+meal+meals+mean+means+meat+meatball+meatballs+meats+mediocre+medium+meet+meeting+meh+mein+melted+mention+mentioned+menu+menudo+menus+mess+messed+met+mex+mexican+mexico+middle+might+mild+mile+miles+milk+min+mind+mine+mini+mins+minute+minutes+miss+missing+mistake+mix+mixed+mocha+modern+moist+mole+mom+moment+monday+money+mongolian+month+months+mood+morning+mostly+mouth+move+moved+moving+mozzarella+much+multiple+mushroom+mushrooms+music+must+nachos+name+nasty+near+nearby+nearly+need+needed+needless+needs+negative+neighborhood+neither+never+new+next.+nice+nicely+night+nights+none+noodle+noodles+normal+normally+north+notch+note+nothing+notice+noticed+now+number+nyc+obviously+odd+offer+offered+offers+office+often+oil+oily+okay+old+olives+omg+one+ones+onion+onions+online+onto+open+opened+opening+opinion+opted+option+options+orange+order+ordered+ordering+orders+original+others+otherwise+outdoor+outside+outstanding+oven+overall+overcooked+overly+overpriced+owned+owner+owners+packed+paid+palace+pan+panda+pao+papa+paper+par+parents+park+parking+part+particular+particularly+party+pass+passed+past+pasta+pastor+pastries+patio+patrons+pay+paying+people+pepper+pepperoni+peppers+per+perfect+perfection+perfectly+perhaps+person+personal+personally+phenomenal+phoenix+phone+pick+picked+picky+pico+picture+pictures+pie+piece+pieces+pies+pineapple+pink+pittsburgh+pizza+pizzas+pizzeria+place+placed+places+plain+plan+plastic+plate+plates+platter+play+playing+pleasant+pleasantly+please+pleased+plenty+plus+point+polite+pollo+poor+pop+popular+pork+portion+portions+positive+possible+possibly+pot+potato+potatoes+pour+prefer+prepared+presentation+pretty+previous+price+priced+prices+pricey+pricing+probably+problem+process+product+prompt+promptly+provide+puffs+pull+pulled+purchase+put+putting+quality+quesadilla+quesadillas+queso+questions+quick+quickly+quiet+quite+ran+ranch+rate+rather+rating+read+reading+ready+real+realize+realized+really+reason+reasonable+reasonably+receipt+receive+received+recent+recently+recommend+recommendation+recommended+red+refill+refills+refreshing+refried+refund+register+regular+relatively+relleno+remember+remembered+reminded+reminds+request+reservation+rest+restaurant+restaurants+return+returned+returning+review+reviews+rice+rich+ricotta+ridiculous+right+rio+road+roast+roasted+robertos+rock+rocks+roll+rolled+rolls+room+round+rude+run+running+rush+sad+sadly+said+salad+salads+salsa+salsas+salt+salty+san+sandwich+sandwiches+sangria+sat+satisfied+saturday+sauce+sauces+sausage+save+saw+say+saying+says+school+scottsdale+seafood+search+seasoned+seasoning+seat+seated+seating+seats+second+secret+see+seeing+seem+seemed+seems+seen+selection+sell+sense+sent+separate+serious+seriously+serve+served+server+servers+serves+service+serving+sesame+set+several+shame+share+shared+shell+shop+shopping+shops+short+shot+show+showed+shredded+shrimp+sick+side+sides+sign+signs+similar+simple+simply+since+single+sister+sit+sitting+situation+six+size+sized+sizes+skip+slice+sliced+slices+slightly+slow+small+smaller+smell+smile+smooth+snack+soda+soft+soggy+solid+someone+something+sometimes+somewhat+somewhere+son+soon+sorry+sort+soup+sour+south+soy+space+spanish+speak+special+specials+specialty+spectacular+spend+spent+spice+spicy+spinach+split+spoke+spot+spots+spring+square+staff+stale+stand+standard+standards+standing+star+starbucks+stars+start+started+starving+state+stated+station+stay+stayed+staying+steak+steamed+step+stick+sticks+still+stomach+stood+stop+stopped+store+story+straight+strange+street+strip+strong+stuck+study+stuff+stuffed+style+sub+sucks+sugar+suggest+suggested+sum+summer+sunday+super+support+supposed+sure+surprise+surprised+surprisingly+sushi+sweet+system+table+tables+taco+tacos+take+taken+takeout+takes+taking+talk+talking+tamale+tamales+taquitos+taste+tasted+tasteless+tastes+tasting+tasty+tea+teas+tell+telling+tempe+ten+tend+tender+tequila+terrible+texture+thai+thank+thanks+thats+thick+thin+thing+things+think+thinking+third+though+thought+three+threw+throughout+throw+thru+time+times+tiny+tip+tired+today+tofu+together+told+tomato+tomatoes+ton+tongue+tonight+tons+took+top+topped+topping+toppings+torta+tortas+tortilla+tortillas+tostada+total+totally+touch+tough+town+traditional+trash+treat+treated+tried+trip+true+truly+trust+try+trying+tsos+tuesday+turn+turned+twice+two+type+types+typical+typically+understand+unfortunately+unique+unless+unlike+update+upon+upset+use+used+using+usual+usually+valley+value+vanilla+variety+various+vegan+vegas+vegetable+vegetables+vegetarian+veggie+veggies+verde+version+vibe+visit+visited+visiting+visits+wait+waited+waiter+waiters+waiting+waitress+walk+walked+walking+wall+walls+want+wanted+wanting+warm+waste+watch+watching+water+watery+way+weather+website+week+weekend+weekends+weeks+weird+welcome+welcoming+well+went+west+wet+whatever+whenever+whether+white+whole+wide+wife+wifi+will+willing+window+wine+wings+wish+within+without+wok+woman+won+wonder+wonderful+wonton+wontons+wood+word+words+work+worked+workers+working+works+world+worse+worst+worth+wow+wrapped+write+writing+wrong+yeah+year+years+yelp+yelpers+yes+yesterday+yet+york+young+yum+yummy+zero+review_count+Category.Bars+Category.American__New_+Category.Sandwiches+Category.Mexican+Category.Japanese+Category.Sushi_Bars+Category.American__Traditional_+Category.Specialty_Food+Category.Local_Flavor+Category.Italian+Category.Pizza+Category.Cafes+Category.Fast_Food+Category.Breakfast___Brunch+Category.Donuts+Category.Coffee___Tea+Category.Seafood+Category.Bakeries+Category.Barbeque+Category.Vegan+Category.Indian+Category.Chinese+Category.Vegetarian+Category.Latin_American+Category.Delis+Category.Mediterranean+Category.Greek+Category.Middle_Eastern+Category.Desserts+Category.Food_Trucks+Category.Burgers+Category.Hot_Dogs+Category.Steakhouses+Category.Soup+Category.Noodles+Category.Buffets+Category.Thai+Category.Cafeteria+Category.Diners+Category.Korean+Category.French+Category.Tex_Mex+Category.Caribbean+Category.Beer+Category.Ramen+Category.Vietnamese+Category.Spanish+Category.Taiwanese+Category.Fish___Chips+Category.Tacos+Category.Hot_Pot+Category.Cantonese+Attribute.BusinessAcceptsCreditCards__True+Attribute.ByAppointmentOnly__True+Attribute.DogsAllowed__True+Attribute.DriveThru__True+Attribute.GoodForDancing__True+Attribute.GoodForKids__True+Attribute.HappyHour__True+Attribute.HasTV__True+Attribute.Open24Hours__True+Attribute.OutdoorSeating__True+Attribute.RestaurantsAttire__casual+Attribute.RestaurantsAttire__dressy+Attribute.RestaurantsAttire__formal+Attribute.RestaurantsCounterService__True+Attribute.RestaurantsDelivery__True+Attribute.RestaurantsGoodForGroups__True+Attribute.RestaurantsPriceRange2__1+Attribute.RestaurantsPriceRange2__2+Attribute.RestaurantsPriceRange2__3+Attribute.RestaurantsPriceRange2__4+Attribute.RestaurantsReservations__True+Attribute.RestaurantsTableService__True+Attribute.RestaurantsTakeOut__True+Attribute.Smoking__yes+Attribute.WiFi__free+Attribute.BusinessParking_street+Attribute.Music_dj
#                  , data=mtrain)
modeltree <- rpart(stars.x~., data=mtrain[-c(1,3)])

predict(modeltree, mtest)







