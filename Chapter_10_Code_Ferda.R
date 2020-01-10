#Machine Learning with R
#Chapter 10 - Evaluating Model Performance
setwd("M:/Machine Learning/9781788295864_Code/Chapter10")



#--------------------------------Measuring performance for classification
install.packages('caret')
library(caret)

sms_results<-read.csv('sms_results.csv')

head(sms_results)

head(subset(sms_results,prob_spam>0.4 & prob_spam <0.6))

head(subset(sms_results,actual_type !=predict_type))

table(sms_results$actual_type,sms_results$predict_type)

install.packages('gmodels')
library(gmodels)

install.packages('e1071', dependencies=TRUE)
library(e1071)

# ---------------------------------------The Kappa Statistic
CrossTable(sms_results$actual_type,sms_results$predict_type)

confusionMatrix(sms_results$predict_type,
                sms_results$actual_type,positive = 'spam')

install.packages("vcd")
library(vcd)

Kappa(table(sms_results$actual_type,sms_results$predict_type))

install.packages('irr')
library(irr)

kappa2(sms_results[1:2])


#----------------------------------------Sensitivity and Specificity
# These measure range from 0 to 1
sensitivity(sms_results$predict_type,sms_results$actual_type,
            positive="spam")

sensitivity(sms_results$predict_type,sms_results$actual_type,
            positive="ham")

posPredValue(sms_results$predict_type, sms_results$actual_type,
             positive = "spam")

sensitivity(sms_results$predict_type,sms_results$actual_type,
            positive="spam")

#-------------------------------------------The F-measure (same as the F - score)
# the measure that combines precision and recal into a single number
# f<- (2 * 152)/ (prec + rec)



#----------------------------------------Visualizing performance tradeoffs with ROC curves
install.packages('pROC')

library(pROC)

sms_roc <- roc(as.numeric(sms_results$prob_spam),as.numeric(sms_results$actual_type)) #cant get this to work

?roc

plot(sms_roc, main= "ROC curve for SMS spam filter", col="blue",lwd=2,legacy.axes = TRUE)

#-------------------------------New example

sms_results_knn <- read.csv('sms_results_knn.csv')
sms_roc_knn <- roc(sms_results$actual_type,
                   sms_results_knn$p_spam)
plot(sms_roc_knn, col="red",lwd=2, add = TRUE)

sms_roc_knn

plot(sms_roc_knn, col='red',lwd=2, add=TRUE)

auc(sms_roc)

auc(sms_roc_knn)



#--------------------------------------------Estimating Future Performance
credit<-read.csv("credit.csv")

#dividing data into even partitions

random_ids <- order(runif(1000))

credit_train <- credit[random_ids[1:500],]
credit_validate <- credit[random_ids[501:750],]
credit_test <- credit[random_ids[751:1000],]

#-------------------------Stratified Random Sampling

in_train <- createDataPartition(credit$default,p=0.75,
                                list =FALSE)
credit_train <- credit[in_train,]
credit_test <- credit[-in_train,]


#K Fold cross Validation
#why 10? trade off between computational intensity and more robust results. This is industry standard!
#Leave One Out Cross Fold Validation is very computationally expensive

folds <- createFolds(credit$default,k=10)

credit01_test <-credit[folds$Fold01,]
credit01_train <- credit[-folds$Fold01,]
                         
                         
str(folds)

install.packages('C50')
library(C50)

set.seed(123)

folds<- createFolds(credit$default,k=10)

cv_results <- lapply(folds, function (x){
  credit_train <-credit[-x,]
  credit_test <- credit[x,]
  credit_model <- C5.0(default ~.,data=credit_train)
  credit_pred <- predict(credit_model,credit_test)
  credit_actual <- credit_test$default
  kappa<- kappa2(data.frame(credit_actual,credit_pred))$value
  return(kappa)
  
})


str(cv_results)

#no we must take the average of the fold kappa stats, however we cannot mean() because they are in a list

#to get them out of a list format use the unlist()

mean(unlist(cv_results))

