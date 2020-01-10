#Chapter 6 Forecasting Numeric Data - Regression Methods
#Harrison McKenny
#6/08/2019



#Example predicting medical expenses using linear regression
#step 1 collecting data

insurance = read.csv("insurance.csv",stringsAsFactors = TRUE)

#check whats up with the data
str(insurance)

#step 2 -------------------------------Exploring and preparing the data

#prior to building a regression model it is helpful to check for normality
#lets look at some summary stats
summary(insurance$expenses)

#mean value is larger than the median which implies that the data for the dependent variable is skewed right

#getting a visual
hist(insurance$expenses)

#regressions require data to be numeric. We have variables with levels "male" or "female" smoker 'yes' or 'no' etc.
#we know that the region variable has 4 levels but we need to look at the distribution
table(insurance$region)

#data is divided nearly evenly across regions

#Exploring relationships among features- the correlation matrix
#before fitting a regression model to data, it can be useful to determine ho the independent variables 
#relate to the dependent variable an eachother. A correlation matrix provides a quick overview of this

#Creating a correlation matrix for the four numeric variables in the insurance data frame,use the cor command
cor(insurance[c("age","bmi","children","expenses")])

#visualizing relationships among features - the scatterplot matrix
pairs(insurance[c("age","bmi","children","expenses")])

#inhancing the correlation matrix
install.packages("psych")
library(psych)
pairs.panels(insurance[c("age","bmi","children","expenses")])

#the values above the diagonal are the correlation matrix.
#the oval shape in each correlation scatterplot is a correlation ellipse. it provides a visualization
#of correlation strength. The more it is stretched, the stronger the correlation. 
#the curve drawn on the scatterplot is called a loess curve. It indicates the general relationship 
#between the x axis and the yaxis variables. 
#a useful example is the curve for age and children is an upside down u, peaking around middle age.
#this means that the oldest and youngest people ini the sample have fewer children on the insurance plan
#than those around middle age. Because this trend is non linear, this finding could not have been 
#inferred from correlations alone. 

#step 3 - training a model on the data
#creating initial model
ins_model = lm(expenses ~ age + children + bmi + sex + smoker +region, data = insurance)

#beta values
ins_model

#step 4 --------------------- evaluating model performance


summary(ins_model)

#Step 5 ----------------------improving model performance

#Model specification - adding nonlinear relationships
insurance$age2 =insurance$age^2
#now when we create another model, we will add age and age2 to allow the model to separate the linear
#and nonlinear impact of age on medical expenses

#transformation ------- converting a numeric variable to a binary indicator
#we can create a binary indicator for BMI being over 30 or not. This might improve signal in the variable
insurance$bmi30= ifelse(insurance$bmi >= 30,1,0)

#model specification - adding interaction effects

#putting it all together - an improved regression model

ins_model2 = lm(expenses~age + age2 + children +bmi +sex+bmi30*smoker+region,data=insurance)

summary(ins_model2)

#Making predictions with a regression model

#saving predictions as a new vector named pred in the insurance data frame
insurance$pred = predict(ins_model2,insurance)

#computing corr between the pred and actual cost of insurance
cor(insurance$pred, insurance$expenses)

# it can be useful to examine this finding as a scatterplot. 
plot(insurance$pred,insurance$expenses)
abline(a=0,b=1,col="red",lwd = 3, lty =2)

#predicting the expense of people looking to enroll
#30 year old male, overweight, with 2 kids, non-smoker, living in the northeast 
predict(ins_model2,
        data.frame(age=30,age2=30^2,children =2,
                   bmi=30,sex="male",bmi30=1,
                   smoker = "no", region = "northeast"))
#using this model the insurance company would have to set a prices to about 6,000 per year in order
#to break even for this demographic group.

#to compare to an otherwise similar female
predict(ins_model2,
        data.frame(age=30,age2=30^2,children =2,
                   bmi=30,sex="female",bmi30=1,
                   smoker = "no", region = "northeast"))

#males forecasted to cost 496 less a year, all things equal

#looking at the cost of kids
predict(ins_model2,
        data.frame(age=30,age2=30^2,children =0,
                   bmi=30,sex="female",bmi30=1,
                   smoker = "no", region = "northeast"))
# a child costs 678 additionally a year in insurance costs; this is the coefficient value in the regression

#************************************ Understanding regression trees and model trees


#Regression trees use standard deviation reduction (SDR) to determine splitting of data on different features

#-----Example - estimating the quality of wines with regression trees and model trees

#Step 1 collecting data
wine=read.csv("whitewines.csv")

#step 2 exploring and preparing the data
str(wine)

hist(wine$quality)
#looks somewhat normalish

#examine summary to check for outliers or other potential problems
summary(wine)

#split our data up into training and testing sets
wine_train=wine[1:3750,]
wine_test=wine[3751:4898,]

#step 3 - training a model on the data
install.packages("rpart")
library(rpart)

#Building the model:
#m <- rpart(dv~iv, data = mydata)
# dv = dependant variable; iv = independant variable
#making predictions:
#p<-predict(m,test,type="vector")
#m is a model trained by rpart()
#test is the test set (must have same features as the training set)
#type specifies the type of prediction to return, either "vector", "class" or "prob" for predicted
#numeric, classses sor class probabilities respectively

m.rpart<-rpart(quality~.,data=wine_train)

#for info about the tree just type the name
m.rpart

summary(m.rpart) #this gives you MSE/a more detailed summary

#visualizing decision trees
install.packages("rpart.plot")
library(rpart.plot)

rpart.plot(m.rpart,digits=3)

rpart.plot(m.rpart,digits = 4, fallen.leaves =TRUE,
           type=3,extra =101)
#Step 4 - evaluating model performance

#to use the regression tree model to make predictions on the test data, we use the predict(),
#this returns the estimated numeric value for the outcome variale, which we'll save in the vector p.rpart

p.rpart=predict(m.rpart,wine_test)

summary(p.rpart)

summary(wine_test$quality)

#since the min and max's between the predicted values and the actual data we know the model is not
#correctly identifying the extreme cases

cor(p.rpart,wine_test$quality)

#measuring the performance with MAE
MAE = function (actual, predicted){
  mean(abs(actual-predicted))
}

MAE(p.rpart,wine_test$quality)

#how much better is this model than just predicting the mean value for every wine
mean(wine_train$quality)
MAE(5.87,wine_test$quality)

#not much better

#Step 5 --------------improving model performance
install.packages("Cubist")
library(Cubist)

#Building the model:
#m<-cusbist(train,class)
#train is a data frame or matrix containing training data
#class is a factor vector with the class for each row in the training data

#Making predictions:
#p<-predict(m,test)
#m is a model trained by the cubist()
#test is a data frame containing test data with the same features as the training data used to build the model

m.cubist<-cubist(x=wine_train[-12],y=wine_train$quality)
#get basic info from typing the name of the object
m.cubist

#here we see the model generated 25 rules to model the wine quality.
#to examine some of these rules, we can apply the summary()
summary(m.cubist)

#lets check how well this new model predicts values
p.cubist= predict(m.cubist,wine_test)
summary(p.cubist)
#better than the original
cor(p.cubist,wine_test$quality)

MAE(wine_test$quality,p.cubist)
