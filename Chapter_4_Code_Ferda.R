
#Chapter 4 Probabilistic Learning - Classification using Naive Bayes
#May 16th, 2019 

setwd("~/Machine Learning with Application in R/Chapter04")

#Step 2 - exploring and preparing the data

#Imports the CSV data of SMS messages to try to use Bayes Classification to filer out spam messages
sms_raw = read.csv("sms_spam.csv",stringsAsFactors = FALSE)

#use the str() function to get some info on the data set

str(sms_raw)

#the type is currently set to a character vector. Since this ia a categorical variable, it would be better
#to convert it to a factor

sms_raw$type = factor(sms_raw$type)

#re-examine the data and see that it has been changed to a factor

str(sms_raw)

#how many are spam compared to ham?
table(sms_raw$type)

#installing a package for text mining
install.packages("tm")
library(tm)

#first step is creating a corpus (a collection of text documents)
#since we've already loaded the data as text, we will use the VectorSource() function to create a 
#source object which can then be supplied to VCorpus()
sms_corpus = VCorpus(VectorSource(sms_raw$text))

#***********************************BY SPECIFYING AN OPTIONAL readerControl parameter, the VCorpus()
#can be used to import text from sources such as PDF's and microsoft word files. to learn more
#use the vignette("tm") code

#use the print function to see what the corpus contains
print(sms_corpus)

#the corpus is essentially a list, so we can use list functions on it. 
#lets peep the inspect() to show a summary of the results
inspect(sms_corpus[1:2])

#to view the actual message as text, we must use the as.character() function
as.character(sms_corpus[[1]])

#to view multiple documents, well need to apply as.character() to several items in the sms_corpus object

lapply(sms_corpus[1:2],as.character)

#we need to clean the text data to standardize words and remove punctuation characters
#the tm_map() provides a method to apply a transformation to a tm corpus
#using the tolower() to make all the characteres lower case
#using the content_transformer function to treat tolower() as a transformatioon function that can be used to acces the corpus

sms_corpus_clean=tm_map(sms_corpus,content_transformer(tolower))

#to check to see how this worked lets check the first messafe in the og corpus and look at the new one too
as.character(sms_corpus[[1]])

as.character(sms_corpus_clean[[1]]) #all lower case!

#now lets strip all numbers from the text
sms_corpus_clean= tm_map(sms_corpus_clean,removeNumbers) #didnt use content_transformer() bc remove numbers is built into tm 

#our next step is to remove stop words (and, but, to, or) from the text. These are usually removed in text mining
#they appear frequently but do not provide much info for machine learning
#rather than define a list of stop words ourself, we will use the stopwords() built into tm

#to view stopwords used 
stopwords()
?stopwords()

#removing stop words
sms_corpus_clean = tm_map(sms_corpus_clean, removeWords,stopwords())

#removing punctuations
sms_corpus_clean=tm_map(sms_corpus_clean,removePunctuation)

#since this gets ride of all blank space, we need to specify our own replace function
replacePunctuation <- function(x){
  gsub("[[:punct:]]+"," ",x)
} #you must use <- here instead of =

#stemming taked the stem of the word and gets rid of the tense. learning, learns, learned all become learn
install.packages("SnowballC")

library(SnowballC)

wordStem(c("learn","learned","learning","learns"))

sms_corpus_clean = tm_map(sms_corpus_clean, stemDocument)

#now we can remove additional whitespace using the built in stripWhitespace() transformation
sms_corpus_clean = tm_map(sms_corpus_clean, stripWhitespace)

as.character(sms_corpus_clean[1:3])

#splitting test documents into words
#creating a DTM sparse matrix form a tm corpus:

sms_dtm= DocumentTermMatrix(sms_corpus_clean)

#to do this all in one command with no preprocessing
sms_dtm2 = DocumentTermMatrix(sms_corpus, control = list(
  tolower=TRUE,
  removeNumbers =TRUE,
  stopwords= TRUE,
  removePunctuation=TRUE,
  stemming = TRUE
))

#peeping game on the two
sms_dtm

sms_dtm2

#Data Preparation ----------- creating training and test datasets
#doing this so that once or spam classifier is build it can be trained on data it has not yet seen
#well divide the data into two portions: 75% for training and 25% for testing. since the messages
#are sorted in order, we can just take the first 4169 messages for training and leave the other 1,390
#for training
sms_dtm_train = sms_dtm[1:4169,]
sms_dtm_test = sms_dtm[4170:5559,]

#it is useful to save a pair of vectors with the laels for each of the rows in the training and testing matrices.
#These labels are not stores in the DTM, so we need to pull them from the original sms)raw data frame

sms_train_labels = sms_raw[1:4169, ]$type
sms_test_labels = sms_raw[4170:5559,]$type

#to confirm that the subsets are representative of the comlete set of SMS data, lets compare
#the proportion of spam in hte raining and test data frames

prop.table(table(sms_train_labels))

prop.table(table(sms_test_labels))

#this suggests that the spam messages were divided evenly between the two datasets

#visualizing text data - word clouds
#word clouds are used to visualize text data. The bigger the word, the more often it appears
install.packages("wordcloud")

library(wordcloud)

wordcloud(sms_corpus_clean,min.freq = 50, random.order = FALSE)
#creating a visualization comparing the clouds for SMS spam and ham
spam = subset(sms_raw, type =="spam")
ham = subset(sms_raw,type == "ham")

wordcloud(spam$text,max.words = 40, scale = c(3,0.5))
wordcloud(ham$text,max.words =40, scale = c(3,0.5))
#the scale parameter allows us to adjust the max and min font size for these

#Data preparation ----------------creating indicator features for frequent words
#to reduce the number of features, well eliminate any word that appears in less than five messages
#or in less than about 0.1 percent of records in the training data
findFreqTerms(sms_dtm_train,5)
sms_freq_words = findFreqTerms(sms_dtm_train,5)

str(sms_freq_words)

#filtering out DTM to include only the terms apppearing in the frequent word vector 
sms_dtm_freq_train = sms_dtm_train[,sms_freq_words]
sms_dtm_fre_test = sms_dtm_test[,sms_freq_words]

#naive bayes classifier is usually trained on data with categorical features. This poses a problem ,
#since the cells in the sparse matrix are numberic and mearsure the number of times a word appears in a message.
#we have to change this to catagorical
convert_counts <- function(x){
  x <- ifelse(x>0,"Yes", "No")
}

#now we need to apply the convert_counts function to each of the columns in out sparse matrix
#the margins parameter specifies either rows or columns

sms_train <- apply(sms_dtm_freq_train,MARGIN = 2, convert_counts)

sms_test <- apply(sms_dtm_fre_test,MARGIN =2, convert_counts)

#Step 3 ------ training a model on the data
install.packages("e1071")


library(e1071)

#building the classifier--- m <- naiveBayes(train,class,laplace=0)
#making preditions----- p<- predict(m,test,type ="class")

#to build our model we will use this command
sms_classifier = naiveBayes(sms_train,sms_train_labels)

#the sms_classifier varaible now contains a naiveayes classifier object that can be used to make prediction

#Step 4------------evaluating model performance

#to evaluate the SMS classifier, we need to test its predictions on the unseen messages in the test data. 
#recall these are stored in a matrix named sms_test, while the class labels are stored in a vector
#named sms_test_labels

#the predict function is used to make predictions
sms_test_pred <- predict(sms_classifier, sms_test)

#To compare predictions to true values, we will use the CrossTable() 
library(gmodels)

CrossTable(sms_test_pred, sms_test_labels,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted','actual'))

#length(sms_test_pred)
#length(sms_test_labels)

36/1390 
#classified incorrectly..... not bad

#Step 5 -----------improving model performance

#we did not classify our laplace estimator and just because something that only appeared once in a spam 
#message in the training data does not mean it cannot appear in a ham message in the test data

#specifying the laplace estimator as 1 will help alleviate this problem
sms_classifier2 <- naiveBayes(sms_train, sms_train_labels, laplace =1)

sms_test_pred2 <- predict(sms_classifier2,sms_test)

CrossTable(sms_test_pred2,sms_test_labels,
           prop.chisq = FALSE, prop.c =FALSE, prop.r = FALSE,
           dnn=c("predicted","actual"))
32/1390




#we have improved the model!!! we got rid of 2 false positives and 1 false negative! excellent
