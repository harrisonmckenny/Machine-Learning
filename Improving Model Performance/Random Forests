#Machine Learning with R
#Chapter 11 - Improving Model Performance
#July - STRABANE STRABANE
rm(list=ls())
#Attempting to increase model performance for the loan default classification data in chapter 5
#classification accuracy of 82% was misleading as the kappa stat was 28%. We trained on test data instead of creating a validation set
setwd("~/Machine Learning with Application in R/Chapter11")
credit <- read.csv("credit.csv")
library(caret)

set.seed(300)

m<- train(default ~., data=credit, method ="C5.0")

m

p<-predict(m,credit)

#Create a Confusion Matrix

table(p,credit)

#obtaining predicted class values and predicted class probabililties
head(predict(m,credit)) #class types

head(predict(m,credit,type="prob")) #probabilities


#---------------------------------------------------------Customizing the Tuning Process-------------------------
#control object
ctrl <- trainControl(method ="cv",number = 10,selectionFunction = "oneSE")
#grid of parameters to optimize
grid<- expand.grid(model="tree",
                   trials = c(1,5,10,15,20,25,30,35),
                   winnow=FALSE)
grid

set.seed(300)

m<-train(default~.,data=credit,method="C5.0",
         metric ="Kappa",
         trControl=ctrl,tuneGrid = grid)

m

#--------------------------------Improving model performance with meta-learning


#----------Bagging------

install.packages("ipred")
library(ipred)

set.seed(300)
mybag<-bagging(default ~.,data =credit,nbagg=25)

credit_pred<-predict(mybag,credit)
table(credit_pred,credit$default)

#Fits very well to the training data. lets see how this translates into future performance

install.packages("caret")
library(caret)
set.seed(300)
ctrl<-trainControl(method='cv',number=10)

train(default~.,data=credit,method="treebag",
      trControl =ctrl)

#---------------------------------------------------Boosting

set.seed(300)
install.packages("adabag")

library(adabag)

m_adaboost <-boosting(default~.,data=credit)


p_adaboost <- predict(m_adaboost,credit)


#object with information about the model
head(p_adaboost$class)

#confusion matrix can be found inside this object
p_adaboost$confusion #100% accuracy damn, titties #prob overfitting 

#assessment of accuracy on unscene data
set.seed(300)
adaboost_cv <- boosting.cv(default~.,data=credit)

#Results of Boosting
adaboost_cv$confusion


#Calculating the Kappa Stat
library(vcd)

Kappa(adaboost_cv$confusion)

#---------------------------------------------------------Random Forests
#Training Random Forests
install.packages("randomForest")
library(randomForest)

#Building the Classifier:
#m<-randomForest(rain,class,ntree =500,mtry=sqrt(p))

#Making predictions:
#p<- predict(m,test,type="response")

set.seed(300)
rf<- randomForest(default~., data=credit)

rf
install.packages('vcd')
library(vcd)

#computing the kappa stat for the random forest
Kappa(rf$confusion[1:2,1:2])

#Evaluating random forest performance in a simulated competition
install.packages('caret')
library(caret)
install.packages('ggplot2')
install.packages("ggplot2")

library(ggplot2)
install.packages('tidyverse')
library(tidyverse)
ctrl<-trainControl(method="repeatedcv",
                   number=10,repeats =10,
                   selectionFunction="best",
                   savePredictions =TRUE,
                   classProbs =TRUE,
                   summaryFunction = twoClassSUmmary)
