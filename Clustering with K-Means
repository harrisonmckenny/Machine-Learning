#Machine Learning with R
#Chapter 9 - Finding Groups of Data - Clustering with K-means
#07/06/2019


#Step 1 - Collecting Data
#we will be using a dataset representing a random sample of 30,000 US high school student who had 
#profiles on a social network service in 2006

#Clearing global enviroment
rm(list=ls())

#Setting working directory
setwd("~/Machine Learning with Application in R/Chapter09")
#Importing data
teens<-read.csv("snsdata.csv")

str(teens)

View(teens)

#Step - 2 exploring and preparing the data

#--------------------------ALWAYS CHECK DATA FOR MISSING VALUES, N/A's NAN's, weird quartile metrics etc-----------
#It looks like we are missing data for the gender variable as N/A's are present

table(teens$gender)
#it excludes n/a's by default
table(teens$gender, useNA="ifany")

summary(teens$age) #some N/A's in age aswell

#quartile metrics look out of character for highschool aged kids
teens$age<- ifelse(teens$age >= 13 & teens$age <20,teens$age,NA) #getting rid of incorrect outliers

#recheck the range now
summary(teens$age)

#numbers make sense now, but we've created a large amount of missing data now

#Data Preparation --- dummy coding missing values

teens$female <- ifelse(teens$gender =="F" &
                         !is.na(teens$gender),1,0)

teens$no_gender<- ifelse(is.na(teens$gender),1,0)

#checking our binary variables
table(teens$gender,useNA = "ifany")

table(teens$female,useNA = "ifany")

table(teens$no_gender, useNA= "ifany")

#Data preparation - inputing the missing values

mean(teens$age) #NA's through off mathmatical calculation

mean(teens$age, na.rm=TRUE) #removes NA's so we can calculate mean

#we can use the aggregate() to
aggregate(data=teens,age~gradyear,mean,na.rm=TRUE)

#the output is a dataframe, it would be hella work to merge back with the og data
#instead we can use the ave() that returns a vector with the group means repeated such that the result
#is of equal length to the original vector:

ave_age <- ave(teens$age,teens$gradyear,FUN=
                 function(x) mean(x,na.rm =TRUE)) #understand  what it is doing here
?ave #dont really get the function and the applications it could be used for

#to impute these means onto the missing values, we need one more ifelse() call to use the ave_age value
#only if the original age value was NA:

teens$age <- ifelse(is.na(teens$age), ave_age,teens$age)

summary(teens$age)

#Step 3 - training a model on the data
#Finding clusters:
#myclusters <- kmeans(mydata,k)

interests <- teens[5:40]

interests_z <- as.data.frame(lapply(interests, scale))

summary(interests$basketball) #yikes

summary(interests_z$basketball) #standardized and nicer

set.seed(2345)
teen_clusters <- kmeans(interests_z,5)


#Step 4 - evaluating model performance

#lets look at the size of the groups in which observations are falling in to
teen_clusters$size

teen_clusters$centers #loking at the coordinates of the cluster centriods


#Step - improving model performance

#adding the cluster assignment back to every profile in the data set

teens$cluster <- teen_clusters$cluster

teens[1:5,c("cluster","gender","age","friends")]

aggregate(data=teens,age~cluster,mean) #average age is not that different across cluster groups

aggregate(data=teens, female~cluster,mean) #big differences considering base data is 74% female

aggregate(data=teens, friends~cluster,mean) #large differences in the mean value of friends


