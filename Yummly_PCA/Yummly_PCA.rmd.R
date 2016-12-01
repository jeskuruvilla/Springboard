require(jsonlite) ##Using JSONLITE
## Add library
library(jsonlite)
library(tm)
library(SnowballC)
library(ggplot2)
library(wordcloud)## Add library
getwd()
setwd("C:/Users/jesku/Documents/Springboard/Yummly Stats") ##Set Working Directory
train <- fromJSON("train.json", flatten = TRUE)
str(train)## Structure of datasframe train
class(train)
colnames(train)## Column names of dataframe train
list(train$cuisine)## Test list of Cuisine column of Train
unique(train$cuisine)
unique(head(train$ingredients))
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
writeLines(as.character(docs[[4]]))
##looks good, we can now apply the removePunctuation transformation
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
docs <- tm_map(docs, content_transformer(gsub), pattern = "listc", replacement = "list ")
#Inspect lines again
writeLines(as.character(docs[[9]]))
##Replace stems to homogenize the words
#docs <- tm_map(docs,stemDocument)
##@# Document Term Matrix (DTM)
#create it - Stored in corpus
dtm <- DocumentTermMatrix(docs)
dtm
str(dtm)
#Inspect specific regions of the Document Term Matrix
inspect(dtm[1:2,1000:1005])

##$#$$ Start Mining the Corpus(dtm)-starting point for quantitative text analysis.
#frequency of occurrence of each word in the corpus
freq <- colSums(as.matrix(dtm))
##Check dimension of freq equals the number of terms
length(freq)
#Sort freq in descending order of term count
ord <- order(freq,decreasing=TRUE)
#List the most and least frequently occurring terms
freq[head(ord)]
#% And list the most and least frequently occurring terms
freq[tail(ord)] 
##Certain words eliminated without loss of context
tdmr <-TermDocumentMatrix(docs, control=list(wordLengths=c(4, 20),bounds = list(global = c(3,27))))
#$#$ Inspect new data frame dtmr
str(tdmr)
#@#@ Calculate the cumulative frequencies of words across documents and sort as above
reqr <- colSums(as.matrix(tdmr))
freqr <- colSums(as.matrix(tdmr))
print(dim(dtmr))
print(dim(tdmr))
library(caret)
nzv <- nearZeroVar(tdmr, saveMetrics = TRUE)
print(paste('Range:',range(nzv$percentUnique)))
print(head(nzv))
library(caret)
nzv <- nearZeroVar(tdmr, saveMetrics = TRUE)
print(paste('Range:',range(nzv$percentUnique)))
print(head(nzv))
##@# Remove features with less than 0.1% variance:
print(paste('Column count before cutoff:',ncol(tdmr)))
##Dimensions of dataframe - nzv
dim(nzv[nzv$percentUnique > 0.1,])
##Work it- Cleaning up data
tdmr_nzv <- tdmr[c(rownames(nzv[nzv$percentUnique > 0.1,]))  ]
print(paste('Column count after cutoff:',ncol(tdmr_nzv)))
##$$ Evaluate without any PCA transformation:
dfEvaluate <- cbind(as.data.frame(sapply(tdmr_nzv, as.numeric)),
                    cluster=g_labels$V1)