## Add library
library(jsonlite)
library(tm)
library(SnowballC)
library(ggplot2)
library(wordcloud)## Add library
library(qdap)
library(tm)
library(rattle)
library(RColorBrewer)
library(rpart.plot)
library(rpart)
library(caret)
library(ranger)
library(xgboost)
getwd()
setwd("~/Springboard/Yummly Stats")
train <- fromJSON("train.json", flatten = TRUE)
str(train)## Structure of datasframe train
class(train)
colnames(train)## Column names of dataframe train
list(train$cuisine)## Test list of Cuisine column of Train
unique(train$cuisine)
unique(head(train$ingredients))
#Create Corpus from tm packages
docs <- Corpus(DataframeSource(train))
str(docs)
##@#Stemming
#Inspect lines
writeLines(as.character(docs[[2000]]))

#@##Clean-Up
ingredients <- tm_map(docs, removeNumbers)
removeBrackets <- content_transformer(function(x){gsub(pattern = "\\(|\\)|,",replacement = " ",x)})
ingredients <- tm_map(ingredients,removeBrackets)
ingredients <- tm_map(ingredients,removePunctuation)
ingredients <- tm_map(ingredients, stemDocument)
ingredients <- tm_map(ingredients, stripWhitespace)

ingredientsMatrix <- DocumentTermMatrix(ingredients)
ingredientsMatrix

#Converting Corpus matrix into df
ingredientsDTM <- as.data.frame(as.matrix(ingredientsMatrix))
str(ingredientsDTM)

#adding DTM back to train data frame
train_new <- data.frame(train,ingredientsDTM) 
dim(train_new)

#Remove Sparse Terms
sparse <- removeSparseTerms(ingredientsMatrix, 0.99)
sparse

#Converting Corpus matrix into df
ingredientsMatrix_Sparse <- as.data.frame(as.matrix(sparse))
str(ingredientsMatrix_Sparse)

ingredientsMatrix_Sparse$cuisine <- as.factor(train$cuisine)
dim(ingredientsMatrix_Sparse)

#Spliting training set into two parts based on outcome: 80% and 20%
index <- createDataPartition(ingredientsMatrix_Sparse$cuisine, p=0.80, list=FALSE)
trainSet <- ingredientsMatrix_Sparse[ index,]
testSet <- ingredientsMatrix_Sparse[-index,]

#Using trainSet to run the model.
# Fit random_forest model on test: model
model_rf <- train(cuisine ~ .,
                  tuneLength = 1,data = trainSet, method = "ranger", 
                  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE) )
tree <- rpart(cuisine ~., data = trainSet, method = "class") 
fancyRpartPlot(tree)
pruned <-prune(tree, cp = 0.1) #View a pruned tree fancyRpartPlot(pruned) #Draw pruned
predict(model_rf, newdata = trainSet, type = "raw")
predict(model_rf, newdata = testSet, type = "raw")
#Fit Using other models- XgBoost
model_xgb <- train(cuisine ~ .,
                   tuneLength = 1,data = trainSet, method = "xgbTree", 
                   trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE) )
predict(model_xgb, newdata = trainSet, type = "raw")
predict(model_xgb, newdata = testSet, type = "raw")


#@$$ Now Testing on Kaggle Ymmly Test set #@$$
test <- fromJSON("test.json", flatten = TRUE)
str(test)## Structure of datasframe test
class(test)
colnames(test)## Column names of dataframe test
test$cuisine  <- 0   # Use the same value (0) for all rows
list(test$cuisine)## Add Test list of Cuisine column of test
unique(test$cuisine)
unique(head(test$ingredients))
#Create Corpus from tm packages
docs_test <- Corpus(DataframeSource(test))
str(docs_test)
##@#Stemming
#Inspect lines
writeLines(as.character(docs_test[[20]]))

#@##Clean-Up
ingredients_test <- tm_map(docs_test, removeNumbers)
removeBrackets <- content_transformer(function(x){gsub(pattern = "\\(|\\)|,",replacement = " ",x)})
ingredients_test <- tm_map(ingredients_test,removeBrackets)
ingredients_test <- tm_map(ingredients_test,removePunctuation)
ingredients_test <- tm_map(ingredients_test, stemDocument)
ingredients_test <- tm_map(ingredients_test, stripWhitespace)

ingredientsMatrix_test <- DocumentTermMatrix(ingredients_test)
dim(ingredientsMatrix_test)

#Converting Corpus matrix into df
ingredientsDTM_test <- as.data.frame(as.matrix(ingredientsMatrix_test))
dim(ingredientsDTM_test)

#adding DTM back to test data frame
test_new <- data.frame(test,ingredientsDTM_test) 
dim(test_new)

#Remove Sparse Terms
sparse_test <- removeSparseTerms(ingredientsMatrix_test, 0.99)
dim(sparse_test)

#Converting Corpus matrix into df
ingredientsMatrix_SparseTest <- as.data.frame(as.matrix(sparse_test))
dim(ingredientsMatrix_SparseTest)


#Using Test to run the model.
pred <- predict(model_rf, newdata =test_new , type = "prob") 
pred == test_new[ , last]  # where 'last' equals the index of 'y'


# Fit random_forest model on test: model
final_submission <-predict(model_rf, newdata = test_new, type =  "prob")