require(jsonlite) ##Using JSONLITE
## Add library
library(jsonlite)
library(tm)
library(SnowballC)
library(ggplot2)
library(wordcloud)## Add library
library(qdap)
library(tm)
library(rpart)
library(rattle)
library(caret)
library(rpart.plot)
library(RColorBrewer)

getwd()
setwd("C:/Users/jesku/Documents/Springboard/Yummly Stats") ##Set Working Directory
train <- fromJSON("train.json", flatten = TRUE)
str(train)## Structure of datasframe train
class(train)
colnames(train)## Column names of dataframe train
list(train$cuisine)## Test list of Cuisine column of Train
train$cuisine
unique(train$cuisine)
unique(head(train$ingredients))
train$ingredients
#Create Corpus from tm packages
docs <- Corpus(DataframeSource(train))
docs
docs
###inspect a particular document
writeLines(as.character(docs[[1]]))
##number of transformations - cleaning data- getTransformations() at the prompt
getTransformations()

#create the toSpace content transformer
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, "", x))})

##usethis content transformer to eliminate colons and hypens like so
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")
##Inspect random sections of corpus
writeLines(as.character(docs[[4]]))##looks good.

##Now apply the removePunctuation transformation
docs <- tm_map(docs, removePunctuation)
##Remove several  "non-standard" punctuation marks
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, "-")
##Inspect random sections of corpus
writeLines(as.character(docs[[4]]))

#Transform to lower case (need to wrap in content_transformer)
docs <- tm_map(docs,content_transformer(tolower))
#Strip digits (std transformation, so no need for content_transformer)
docs <- tm_map(docs, removeNumbers)
#remove stopwords using the standard list in tm
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <-removeWords(docs, "list","c")
#Strip whitespace (cosmetic?)
docs <- tm_map(docs, stripWhitespace)


##@#Stemming
#Inspect lines
writeLines(as.character(docs[[2000]]))
#Inspect lines again
writeLines(as.character(docs[[9]]))

#@#Do you want to do any Lemmation? Parts of speech(POS)? Not for now
###Replace errors like fused words with gsub or strreplace
##as eg:docs <- tm_map(docs, content_transformer(gsub), pattern = "wwwjohnshopkinsmedicineorg", replacement = "medicine")
docs <- tm_map(docs, content_transformer(gsub), pattern = "listc", replacement = "")
docs <- tm_map(docs, removeNumbers)
removeBrackets <- content_transformer(function(x){gsub(pattern = "\\(|\\)|,",replacement = " ",x)})
#Inspect lines again
writeLines(as.character(docs[[9]]))

##Replace stems to homogenize the words
#docs <- tm_map(docs,stemDocument)
str(docs)
#Inspect lines again
writeLines(as.character(docs[[11]]))

##@# Document Term Matrix (DTM)#create it - Stored in corpus
ingredientsMatirx <- DocumentTermMatrix(docs)
ingredientsMatirx

#Converting Corpus matrix into df
ingredientsDTM <- as.data.frame(as.matrix(ingredientsMatirx))
str(ingredientsDTM)

#adding DTM back to train data frame
train_new <- data.frame(train,ingredientsDTM) 

str(train_new)


sparse <- removeSparseTerms(ingredientsMatirx, 0.99)
sparse

#Converting Corpus matrix into df
ingredientsDTM_Sparse <- as.data.frame(as.matrix(sparse))
str(ingredientsDTM_Sparse)

ingredientsDTM_Sparse$cuisine <- as.factor(train$cuisine)
str(ingredientsDTM_Sparse)
dim(ingredientsDTM_Sparse)

#@#@splitting test and train
split <- round(nrow(ingredientsDTM_Sparse) * .70)
# Create train
ingredientsDTM_Sparsetrain <- ingredientsDTM_Sparse[1:split, ]
# Create test
ingredientsDTM_Sparsetest <- ingredientsDTM_Sparse[(split +1): nrow(ingredientsDTM_Sparse), ]
# Fit lm model on train: model
model <-lm(cuisine ~ ., data = ingredientsDTM_Sparsetrain)

# Predict on test: p
p <-predict(model, ingredientsDTM_Sparsetest)
head(p)
tail(p)


