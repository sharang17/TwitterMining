library(e1071)

pos_tweets = rbind(c("I love this car", "positive"), c("This view is amazing", 
    "positive"), c("I feel great this morning", "positive"), c("I am so excited about the concert", 
    "positive"), c("He is my best friend", "positive"))


neg_tweets = rbind(c("I do not like this car", "negative"), c("This view is horrible", 
    "negative"), c("I feel tird this morning", "negative"), c("I am not looking forward to the concert", 
    "negative"), c("He is my enemy", "negative"))


test_tweets = rbind(c("feel happy this morning", "positive"), c("larry friend", 
    "positive"), c("not like that man", "negative"), c("house not great", "negative"), 
    c("your song annoying", "negative"))

tweets = rbind(pos_tweets, neg_tweets, test_tweets)

# native bayes
matrix = create_matrix(tweets[, 1], language = "english", removeStopwords = FALSE, 
    removeNumbers = TRUE, stemWords = FALSE, tm::weightTfIdf)
mat = as.matrix(matrix)
classifier = naiveBayes(mat[1:10, ], as.factor(tweets[1:10, 2]))
predicted = predict(classifier, mat[11:15, ])
predicted

table(tweets[11:15, 2], predicted)

# the other methods
container = create_container(matrix, as.numeric(as.factor(tweets[, 2])), trainSize = 1:10, 
    testSize = 11:15, virgin = FALSE)  #removeSparseTerms

models = train_models(container, algorithms = c("MAXENT", "SVM", "RF", "BAGGING", 
    "TREE"))

results = classify_models(container, models)

# accuracy
table(as.numeric(as.factor(tweets[11:15, 2])), results[, "FORESTS_LABEL"])