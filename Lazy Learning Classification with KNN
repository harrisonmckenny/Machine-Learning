setwd("~/Machine Learning with Application in R")

#---------------------------Chapter 3 Lazy Learning Classification Using Nearest Neighbors


#*******In General KNN classifiers are well suited for classification tasks where relationships among
#the features and the target classes are umerous, complicated, or otherwise extremely difficult to
#understand, yet the items of similar class type tend to be fairly homogeneous

#One common practice is to begin with k equal to the square root of the number of training examples.
# for example in the food case in the book there are 15 example ingredients so we would start with
# k = 4

#must transform features to a standard range of values prior to applying the K-NN algorithm

setwd("~/Machine Learning with Application in R/Chapter03")

wbcd = read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)

str(wbcd)

#playing with what i learned from the last chapter
table(as.character(wbcd$diagnosis))

#REGARDLESS OF MACHINE LEARNING METHOD, ID VARIABLES SHOULD ALWAYS BE EXCULUDED!
#A MODEL THAT INCLUDES AN ID COLUMN WILL ALMOST DEFINITELY SUFFER FROM OVERFITTING!!
# IT WILL ALSO GENERALIZE POORLY TO FUTURE DATA

#dropping the id column from the data
wbcd=wbcd[-1]

#many R machine learning classifiers require the target feature to be coded as a factor, so we need to
# recode the diagnosis variable.

wbcd$diagnosis = factor(wbcd$diagnosis, levels =c("B","M"), 
                        labels = c("Benign", "Malignant"))

#Looking at dependent variable with a table function again
round(prop.table(table(wbcd$diagnosis))*100,digits = 1)

#looking at the other 30 feature variables in the data
summary(wbcd[c("radius_mean","area_mean","smoothness_mean")])

#*******************^^^^^^Looking at this we see that the distance calculations will be problematic
#since all of the variables are not on the same scale

###--Transformation - normalizing numeric data
#to normalize these features we need to create a normalization function in R.
#this function takes a vector x of numeic values and for each value in x subtracts the min x value
#and divides by the range of x values. Lastly, the resulting vector is returned

normalize= function(x){
  return ((x-min(x))/ (max(x)-min(x)))
}

#testing the function
normalize(c(1,2,3,4,5))

normalize(c(10,20,30,40,50))

#results are the same despite a factor of 10 difference, so function works!

#we can now apply this function to the 30 variables in our data set
#the lapply function takes a list and applies a specified function to each list element. we can use 
#this to apply normalize to each feature in the data frame
#the final step is to convert the list returned by lapply() to a data frame

wbcd_n= as.data.frame(lapply(wbcd[2:31],normalize))

#this command applies the normalize function to columns 2-31 *remember 1 is the ID column so we dont use it
#look at the summary statistics to confirm the data has been normalized
summary(wbcd_n$area_mean)

#this variable originally ranged from 143.5 to 2501 now ranges from 0 to 1

#Because all of the observations are labeled with benign or malignant and we dont have data to test our
#KNN algorithm on, we can simulate this scenario by dividing our data into two portions: a training
#set and a testing set

wbcd_train = wbcd_n[1:469,]
wbcd_test = wbcd_n[470:569,]

#remember data is extracted using row, column syntax!

#DATA MUST BE ORDERED RANDOMLY TO DO THIS, NOT CHRONOLOGICALLY OR IN GROUPS OF SIMILAR VALUES!!!!!!
#in those cases random sampling would be needed

#When you construct a normalized training and test dataset, we excluded the target variable diagnosis
#For training the k-NN model, we will need to store these class labels in factor vectors, split between
#the training and test datasets

wbcd_train_labels = wbcd[1:469, 1] #diagnosis for the training set
wbcd_test_labels = wbcd [470:569,1] #diagnosis for the test set

#step 3 - training a model on the data
#The class package provides a standard implementation of the knn algorithm. The function will identify
#The k nearest neighbors, using Euclidean distance, where k is a user specified number. The test instance is classified by taking a"vote"
#among the nearest neighbors

install.packages("class")

library(class)

#there are other more detailed knn fuctions in other r packages that provide more sophisticated implementations.
# p = knn(train, test, class, k) knn classification syntax

#we will use 21 as k as it is close to the square root of 469. With two-category outcome, using an odd
# number eliminates the chance of ending with a tie vote

wbcd_test_pred = knn(train =wbcd_train, test = wbcd_test, 
                     cl = wbcd_train_labels, k = 21)
#the knn function returns a factor vector of predicted labels for each of the examples in the wbcd_test dataset

#-------Step 4 evaluating model performance
#we can evaluate how wll the predicted classses in the wbcd test pred vector match the actual values in the wbcd test labels vector

library("gmodels")
#we can use the CrossTable() function specifying prop.chisq = FALSE will remove unnecessary chi square values from the output
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)
37/39
#------------------Step 5 improving model performance
#first lets try an alternative method for rescaling our numeric features
#second we will try several differeent k values

#----Transformation - Z-score standardization
#although normalization is commmonly used for knn classification, lets try z score as it may be better in this case

wbcd_z = as.data.frame(scale(wbcd[-1]))
#this rescales all the features except diagnosis
summary(wbcd_z$area_mean)

#the mean should always be zero and the range should be fairly compact greater than 3 or less than -3 
#are hella rare values doggy

#following all the steps to implement knn 
#1
wbcd_train =wbcd_z[1:469,]
#2
wbcd_test=wbcd_z[470:569,]
#3
wbcd_test_labels=wbcd[1:469,1]
#4
wbcd_test_labels=wbcd[470:569,1] 
#5
wbcd_test_pred = knn(train = wbcd_train, test= wbcd_test,
                     cl = wbcd_train_labels,k=21)

CrossTable(x = wbcd_test_labels,y = wbcd_test_pred,
           prop.chisq = FALSE)

#This has more false negatives making it a worse approach

#---Testing alternative values of k
#Play around witht the first way of classification and changing k values (in the book they use values)

#from 1-27 and it looks like 21 is the best


