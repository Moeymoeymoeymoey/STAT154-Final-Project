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


#Ridge & Lasso
library(glmnet)
mcterms.new = as.matrix(mcterms)
lam1 = 2^seq(-10,10)
#mod.ridge = cv.glmnet(mcterms.new[,1:1551], matrix(y,ncol=1), 
#                      intercept = FALSE, lambda = lam1)
#plot(mod.ridge, main = 'Ridge')
#best.lambda = mod.ridge$lambda.1se #0.0078125

mod.ridge = glmnet(mcterms.new[,1:1551], matrix(y,ncol=1),
                   intercept = FALSE, lambda = 0.0078125, alpha = 0)

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
test.wordcount.index = test.wordcount>10
test.mcterms = test.mcterms[test.wordcount.index,]   # let's only keep user reviews that have at least 10 words
dim(test.mcterms)  
test.mcterms = as.matrix(test.mcterms)

test.business_id = yelp.test$business_id[test.wordcount.index]

prediction = predict.glmnet(mod.ridge, newx = test.mcterms)

###user.stars.regularize is a function to regularize individual user reviews to [-0.5, 6.5] range. 
#The range size can be adjusted later.
user.stars.regularize = function(x) {if(x >= 6.5){return(6.5)} else if (x <= -0.5) {return(-0.5)} else {return(x)}}
prediction.regularized = apply(prediction, user.stars.regularize, MARGIN = 1)

business.test = read.csv("yelp_academic_dataset_business_test.csv", header = TRUE)

length(prediction.regularized)
length(test.business_id)

prediction.df = data.frame(test.business_id, prediction.regularized)
unique.id = as.character(unique(business.test$business_id))

score = NULL
for (i in 1:length(unique.id)) {
  score[i] = sum(prediction.df[test.business_id==unique.id[i],]$prediction.regularized)/nrow(prediction.df[test.business_id == unique.id[i],])
}

score.regularize = function(x) {if (is.na(x)) {return (3)} else if(x >= 5){return(5)} else if (x <= 1) {return(1)} else {return(round(x*2)/2)}}
score.regularized = apply(as.matrix(score), score.regularize, MARGIN = 1)
score.df = data.frame(unique.id, score.regularized)
colnames(score.df) = c("business_id", "stars")
rownames(score.df) = c()
#score.mat = cbind(unique.id, score.regularized)
#colnames(score.mat) = c("business_id", "stars")
#hist(score.regularized, main = "stars prediction")
write.csv(score.df, file = "predicted_stars.csv", row.names = F)

## transform restaurant reviews into a tm structure that can be used for topic modeling

# let's also create a set of reviews for just the restaurants
mypaste<-function(vec){paste(vec," ",collapse="")}  # turns a vector of strings into a single long string
rreviews=tapply(yelp$text,as.integer(yelp$business_id),mypaste)  # for each business_id combine the customer reviews into a single restaurant review
rreviews=as.vector(rreviews)  # turn this into a vector
rreviews = VCorpus(VectorSource(rreviews))  # create the corpus
yelpbusiness=data.frame(list(id=as.integer(yelp$business_id),business_id=as.character(yelp$business_id))) # create list with integer index and business_id name
yelpbusiness=unique(yelpbusiness)  # only save the unique ones
yelpbusiness=yelpbusiness[order(yelpbusiness$id),]  # sort by the id
yelpbusinessid=as.character(yelpbusiness$business_id) # only save the id (this is a sorted list)
meta(rreviews,"business_id")=yelpbusinessid  # add restaurant id
writeLines(as.character(rreviews[1]))  # print specific restaurant

# second let's modify the reviews (removal of certain words)
rreviews = tm_map(rreviews,stripWhitespace)  # remove extra whitespace
rreviews = tm_map(rreviews,content_transformer(tolower))  # convert all to lower case
rreviews = tm_map(rreviews,removeWords,stopwords("english"))  # remove common english words
rreviews = tm_map(rreviews,removeNumbers)  # remove numbers
rreviews = tm_map(rreviews,removePunctuation)  # remove symbols
writeLines(as.character(rreviews[1]))  # print specific review after transforms

# third let's create a term-document matrix (e.g., count # of times word used)
tmrreviews = DocumentTermMatrix(rreviews)
dim(tmrreviews)   # notice that we have one row for each user restaurant, and one column for each word
inspect(tmrreviews[1:10,c("amazing","happy","indian","food")])  # print out word counts for the first 10 users for four specific words

# fourth let's only get the words that are common (alternative could use removeSparseTerms(tmreviews,0.2))
shorttermlist=findFreqTerms(tmrreviews,500)  # let's find words that are used 500 times or more
mrterms = tmrreviews[,shorttermlist]  # let's create another term matrix with just common words
dim(mrterms)   # notice now we have the same number of rows but only 1984 columns (just frequent words)
wordcount=apply(mrterms,1,sum)   # count the number words in reviews (the '1' tells R to sum each row or user)
summary(wordcount)  # summary of the restaurants
mrterms = mrterms[wordcount>10,]   # let's only keep restaurant reviews that have at least 10 words
dim(mrterms)   # notice that now we still have 3196 restaurant reviews

lmterms=apply(mrterms,1,sum) 



# estimate kmeans with 10 topics
k=10
grpKmeans=kmeans(mrterms,centers=k)

# summarize the centroids
grpKcenter=t(grpKmeans$centers)
parallelplot(t(grpKcenter[c("indian","mexican","burger","great"),]))  # choose a few words

# print a table with the restaurants assigned to each cluster
for (i in 1:k) {
  print(paste("* * * Restaurants in Cluster #",i," * * *"))
  print(yelpbusiness[grpKmeans$cluster==i])
}



##################### estimate an LDA topic model using words  ######################

## our first step is to estimate the topic model using LDA

# setup the parameters for LDA control vector
#burnin=1000     # number of initial iterations to discard for Gibbs sampler (for slow processors use 500)
#iter=5000       # number of iterations to use for estimation  (for slow processors use 1000)
#thin=50         # only save every 50th iteration to save on storage
#seed=list(203,5,63,101,765)  # random number generator seeds
#nstart=5        # number of repeated random starts
#best=TRUE       # only return the model with maximum posterior likelihood

# estimate a series of LDA models (each run can take a few minutes depending upon your processor)
ClusterOUT = LDA(mrterms,35,method="Gibbs",control=list(nstart=nstart,seed=seed,best=best,burnin=burnin,iter=iter,thin=thin))


# probability of topic assignments (each user or restaurant has its own unique profile)
# rows are restaurants and columns are topics
ClustAssign = ClusterOUT@gamma   # this is a matrix with the row as the restaurant and column as the topic
dim(ClustAssign)  # check the dimension of the cluster (movies X topics)
head(ClustAssign,n=10)   # show the actual topic probabilities associated with the first 10 restaurants

# matrix with probabilities of each term per topic
ClustTopics = exp(ClusterOUT@beta)     # notice that we use "@" to access elements in the object and not "$" since this is an S4 object
colnames(ClustTopics)=colnames(mrterms) # the columns are the terms
dim(ClustTopics)                       # check dimensions of the topics
print(ClustTopics)                     # print out clusters (topics in rows and terms in columns)

# visualize the distribution of topics across the restaurants
boxplot(ClustAssign,xlab="Topic",ylab="Probability of Topic across Restaurants")




###stop here###