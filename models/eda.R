library(tm)
library(nnet)
library(e1071)
library(ggplot2)

business.train = read.csv("yelp_academic_dataset_business_train.csv", header = T)
business.review = read.csv("yelp_academic_dataset_review_train.csv", header = T)

qplot(data = business.train, stars, main = "Business Stars")
qplot(data = business.review, stars, main = "Review Stars")

text = as.vector(business.review$text)
lower_txt = tolower(text)
lower_txt = gsub("[[:punct:]]", " ", lower_txt)
split = strsplit(lower_txt, split = " ")
remove_empty_str = function (x) {
    x = x[x != ""]
    return(x)
}
txt_split = lapply(split, FUN = remove_empty_str)
word_count = NULL
for (i in 1:116474) {
  word_count[i] = length(txt_split[[i]])
}

business.review$review_length = word_count

qplot(data = business.review, review_length, main = "Review Length")


stopWords = c(stopwords("en"), "")

cleanReview = function(review, stop_words=stopWords){
  
  # Lowercase review 
  lower_txt = tolower(text)
  # Remove punctuation - (might want to keep !)
  lower_txt = gsub("[[:punct:]]", " ", lower_txt)
  # Tokenize review 
  tokens = strsplit(lower_txt, ' ')[[1]]
  # Remove stop words 
  clean_tokens = tokens[!(tokens %in% stopWords)]
  clean_review = paste(clean_tokens, collapse=' ')
  return(clean_review)
}

cleanCorpus = function(corpus){
  # You can also use this function instead of the first. 
  # Here you clean all the reviews at once using the 
  # 'tm' package. Again, a lot more you can add to this function...
  
  review_corpus = tm_map(corpus, content_transformer(tolower))
  review_corpus = tm_map(review_corpus, removeNumbers)
  review_corpus = tm_map(review_corpus, removePunctuation)
  review_corpus = tm_map(review_corpus, removeWords, c("the", "and", stopwords("english")))
  review_corpus =  tm_map(review_corpus, stripWhitespace)
}
