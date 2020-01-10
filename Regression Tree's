#Machine Learning Chapter 5 Divide and Concour - Classification using decision Tree's and Rules
#May 25th 2019

#Using the C5.0 Decision Tree algorith. This usually has similar performance to black box and neural networks
#and support vector machines, but are much easier to understand and deploy
#trying to split data until subset is pure "any subset composed of only a single class"
#this method deals with entropy a concept that quantifies the randomness or disorder within a set of class
#Values. Sets with high entropy are very diverse and proide little information about other items that may also belong
#in the set.

#simple example: suppose we have a partition of data with two classes: red(60%) and white (40%)
#Entropy calculation would be as follows:
-.60*log2(.60) - 0.40*log2(.40)

#Visualizing the Entropy
curve(-x*log2(x) - (1-x),
      col="red",xlab="x",ylab = "Entropy",lwd=4)

#Setting Working Directory to location where data is saved
setwd("~/Machine Learning with Application in R/Chapter05")

#step 1----------------------Collecting Data
#Reading in
credit<-read.csv("credit.csv")

#Step 2---------------------exploring and preparing the data
#looking at an overview
str(credit)

#these variables are recorded as categorical
table(credit$checking_balance)
table(credit$savings_balance)
#DM stands for Deutsche Mark the money used before the adaption of the Euro in Germany

#These variables are recorded as numerical
summary(credit$months_loan_duration)

summary(credit$amount)

table(credit$default)

#Data preparation -----creating random training and test datasets
#training dataset for building the decision tree and test dataset to evaluate its performance
#we will use 90% of the data for training and 10% for testing

#*************************IMPORTANT DATA MUST BE IN RANDOM ORDER BEFORE BUILDING SETS!!!!!!
#Must train model on a random sample of the data

#set a seed value first so we can replicate the random sample later on
set.seed(123)
train_sample = sample(1000,900)

str(train_sample)

credit_train = credit[train_sample,]

#using the negative sign tells r to exclude this object in the set
credit_test = credit[-train_sample,]

#we know that 30% of the overall loans defaulted, if randomization was done correctly, we should have
#30% default rate in both the test and training set
prop.table(table(credit_train$default))

prop.table(table(credit_test$default))

#good to go baby!

#step 3--------------------------training a model on the data
install.packages("C50")

library(C50)

?C5.0Control

#C5 decision tree syntax
#m<- C5.0(train,class,trials = 1, costs =NULL)
#p<- predict(m,test,type="class")

credit_model <- C5.0(credit_train[-17], credit_train$default)

credit_model

#to see the decision tree we can call the summary() on the model
summary(credit_model)

#Step 4-------------evaluating model performance
#to apply our decision tree to the test data set we use the predict()
credit_pred<-predict(credit_model,credit_test)

library(gmodels)

CrossTable(credit_test$default,credit_pred,
           prop.chisq = FALSE, prop.c=FALSE,prop.r=FALSE,
           dnn=c('actual default','predicted default'))

#Step 5--------------improving model performance
#our models error rate is way to high to employ it for a real time application

#boosting the accuracy of decision trees

#to boost add the trials parameter indicating the number of separate decision trees to use in the boosted 
#team.This sets an upper limit  and will stop adding trees if it recognized that the additional trails
#do notseem to be improving the accuracy. 10 trials is the industry standard and reduces error rates on test data 
#by 25%.
credit_boost10 <-C5.0(credit_train[-17],credit_train$default,
                      trials=10)
credit_boost10
#the ouput shows that across 10 iterations, our tree size shrunk. if you would like, you can see all 10 tree's
#by typing summary(credit_boost10)
summary(credit_boost10)
34/900
#HUGE IMPROVEMENT!!!!!!!
#Now lets see if that improvement continues on to the test data
credit_boost_pred10<-predict(credit_boost10,credit_test)
CrossTable(credit_test$default,credit_boost_pred10,
           prop.chisq = FALSE,prop.c = FALSE,prop.r =FALSE,
           dnn=c('actual default','predicted default'))

#Large improvement, but still classifying 18% incorrectly
#could be because our training set is to small or maybe just a difficult problem to solve

#Making some mistakes costs more than others:
#the money that the bank would earn from a risky loan is far outweighed by the massive loss it would 
#incur if the money is not paid back at all. 

#we can design a penalty to incorperate this assumption into our classification decision tree
#first we must specify the dimensions
matrix_dimensions <-list(c("no","yes"),c("no","yes"))
names(matrix_dimensions)<- c("predicted","actual")
matrix_dimensions
#next we must assign the penalty for hte various types of errors by suppling four values to fill the matrix
#since r fills a mrtix by filin columns one by one from top to bottom, we need to supply the values in a specific order
#suppose we believe that a loan default costs the vank four times as much as a missed opportunity.

error_cost <- matrix(c(0,1,4,0),nrow=2,
                     dimnames =matrix_dimensions)
error_cost

#false negatives are penalized 4 times as much as false positive to reflect cost of a default

#lets apply this now
credit_cost<-C5.0(credit_train[-17],credit_train$default,
                  costs=error_cost)
credit_cost_pred=predict(credit_cost,credit_test)
CrossTable(credit_test$default,credit_cost_pred,
           prop.chisq = FALSE,prop.c = FALSE, prop.r=FALSE,
           dnn=c('actual default','predicted default'))
