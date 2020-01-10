#Machine Learning with applications in R
#Chapter 7: Black Box Methods - Neural Networks and Support Vector Machines
#06/30/19

#Neural Networks
#Example - modeling the strength of concrete with ANNs

#Step 1: Collecting Data
#Utilizing data on the compressive strength of concrete donated to UCI Macine Learning Repository

rm(list=ls())
setwd("~/Machine Learning with Application in R/Chapter07")

concrete <- read.csv("concrete.csv")
#Step 2: exploring and preparing the data
str(concrete)

#looks like input data is not scaled to a narrow range around zero. WE NEED TO NORMALIZE DATA FIRST!!

#we defined our own normalize function in chapter 3 that we will use here
normalize<-function(x){
  return((x - min(x))/(max(x)-min(x)))
}

#we can apply this function to every column in the data using lappy()

concrete_norm<- as.data.frame(lapply(concrete,normalize))

#to confirm that this worked we can see the min and max stregnth are now zero and one
summary(concrete_norm$strength)

#compared to the OG
summary(concrete$strength)

#any transformation will have to be applied in reverse after the neural network is done to get actual values out!!!!!

concrete_train <-concrete_norm[1:773,]
concrete_test <- concrete_norm[774:1030,]

#Step 3 - training a model on the data
install.packages("neuralnet")
library(neuralnet)

#syntax info:
#building the model:

#m<- neuralnet(taraget ~ predictors, data =mydata, hidden =1, act.fct = "logistic")

#making predictions:

#p<- compute(m,test)

concrete_model<-neuralnet(strength ~ cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data = concrete_train)
                           
plot(concrete_model)

#Step 4 - evaluating model performance
#to generate predictions on the test dataset, we can use the compute() function as follows:

model_results <- compute(concrete_model, concrete_test[1:8])

#the compute function works differently that the predict function. it returns a list with two componenets: 
#$neurons which stores the neurons for each layer in the network, and $net.result, which stores the
#the predicted values. We want $net.result

predicted_strength<-model_results$net.result

#we can look at correlation between the between the actual and predicted values to see if the model 
#is useful gauge of concrete strength

cor(predicted_strength,concrete_test$strength)

#Step 5 - improving model performance
concrete_model2<-neuralnet(strength ~ cement + slag+ ash + water + superplastic + coarseagg
                           +fineagg + age, data = concrete_train, hidden = 5)

plot(concrete_model2)

model_results2<- compute(concrete_model2,concrete_test[1:8])

predicted_strength2<-model_results2$net.result

cor(predicted_strength2,concrete_test$strength)
#cor went way up!!!

#experimenting with a different activation function
#soft plus function: log(1+e^x)
softplus<- function(x){
  log(1+exp(x))
}

set.seed(12345)
concrete_model3 <-neuralnet(strength ~ cement +slag +ash + water + superplastic + coarseagg + fineagg
                            + age, data = concrete_train, hidden = c(5,5), act.fct =softplus)
plot(concrete_model3)

model_results3<- compute(concrete_model3,concrete_test[1:8])
predicted_strength3<-model_results3$net.result
cor(predicted_strength3,concrete_test$strength)

strengths <-data.frame(
  actual =concrete$strength[774:1030],
  pred = predicted_strength3
)

head(strengths, n=3)

cor(strengths$pred,strengths$actual)

#choice of normalized or unnormalized data does not affect our computed performance stat
#If we were to compe a different performance metric, such as the absolute difference between the predicted 
#and actual values, the coice of scale would matter quite a bit

unnormalize<-function(x){
  return((x * (max(concrete$strength))-
            min(concrete$strength)) + min(concrete$strength))
}
#now we can see that the new predictions are on a similar scale to the original concrete strength values. 
#this allows us to compute a meaningful absolute error value. Additinoally the cor should remain the same

strengths$pred_new<-unnormalize(strengths$pred)
strengths$error<- strengths$pred_new - strengths$actual

head(strengths, n=3)

cor(strengths$pred_new,strengths$actual)

#--------------------------------------Support Vector Machines-----------------------

#Step 1 Collecting Data

letters <- read.csv("letterdata.csv")
str(letters)
View(letters)

#Step 2 exploring and preparing out data
#The data has already been randomized so we can just divide it into training and test data right now

letters_train <-letters[1:16000,]
letters_test <- letters[16001:20000,]

#Step 3 - training a model on the data
#syntax:
#Building the model:
#m<- ksvm(target ~predictors, data =mydata, kernel ="rbfdot",C=1)

#Making predictions:
#p<-predict(m, test, type ="response")

install.packages("kernlab")
library(kernlab)

letter_classifier <- ksvm(letter ~.,data=letters_train,
                          kernel = "vanilladot")
letter_classifier

#Step 4- evaluating model performance
#the predict() function allows us to use the letter classification model to make predictions on the testing dataset
letter_predictions <-predict(letter_classifier,letters_test)

#because no type parameter was specified we got the "response" as default. This provides a vector
#containing a predicted letter for each row of values in the testing data

head(letter_predictions)

#to examine how well our classifier performed we need to compare the predicted letter to the true letter in the
#testing dataset. we can do this using the table() 

table(letter_predictions,letters_test$letter)

agreement <- letter_predictions == letters_test$letter

table(agreement)
#correctly classified 3357/4000

prop.table(table(agreement))

#83% success rate!

#Step 5 ------------------------------Improving the Model------------------------

#Changing the SVM kernel function
letter_classifier_rbf <- ksvm(letter ~.,data = letters_train,
                              kernel="rbfdot")
letter_predictions_rbf <- predict(letter_classifier_rbf,
                                  letters_test)
agreement_rbf <-letter_predictions_rbf == letters_test$letter

table(agreement_rbf)

prop.table(table(agreement_rbf))

#93% success rate!!

cost_values <- c(1, seq(from =5, to =40, by = 5))

accuracy_values<- sapply(cost_values, function(x){
  set.seed(12345)
  m<- ksvm(letter ~., data = letters_train, kernel = "rbfdot", C=x)
  
  pred <- predict(m, letters_test)
  agree <- ifelse(pred == letters_test$letter,1,0)
  accuracy <- sum(agree)/ nrow(letters_test)
  return (accuracy)
})

plot(cost_values,accuracy_values,type = "b")
