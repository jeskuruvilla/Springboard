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
getwd()
setwd("C:/Users/jesku/Documents/Springboard/Yummly Stats") ##Set Working Directory
train <- fromJSON("train.json", flatten = TRUE)
str(train)## Structure of datasframe train
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


# Fit random_forest model on train: model
model_rf <- train(cuisine ~ .,  
                  tuneLength = 1,data = ingredientsMatrix_Sparse, method = "ranger",
                  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)

tree <- rpart(cuisine ~., data = ingredientsMatrix_Sparse, method = "class")
fancyRpartPlot(tree)

pruned <-prune(tree, cp = 0.1) #View a pruned tree
fancyRpartPlot(pruned)  #Draw pruned

#@$$Now Testing models
test <- fromJSON("test.json", flatten = TRUE)
str(test)## Structure of datasframe test
class(test)
colnames(test)## Column names of dataframe test
test <- data.frame("cuisine") #Add test$cuisine
list(test$cuisine)## Test list of Cuisine column of test

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

#RandomForest Model 
model_rf
##Applying our model from Random Forest
predict(model_rf, newdata = test_new, type =  "prob")

#Test randomly predicted :943 -Mexican
writeLines(as.character(docs_test[[943]]))
writeLines(as.character(docs[[943]]))

#$FINAL SUBMISSION
write.csv(final_submission, file = "~/Springboard/Yummly Stats/jesyummly.csv",row.names = TRUE)