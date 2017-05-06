review.train = read.csv('yelp_academic_dataset_review_train.csv', header = TRUE)
business.reviews1 <- data.frame(review.train$text, stringsAsFactors = FALSE)
library(tm)
corpus <- Corpus(DataframeSource(business.reviews1))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
wordcloud(corpus, scale = c(3, 0.5), max.words = 50, min.freq = 5, random.order = , 
          rot.per = 0.35, use.r.layout = FALSE, colors = brewer.pal(8, "Dark2"))



