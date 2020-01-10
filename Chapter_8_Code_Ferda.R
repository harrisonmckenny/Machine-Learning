#Machine Learning with R
#Chapter 8 - Finding Patterns - Market Basket Analysis Using Association Rules
#July 4th 2019

#Example - identifying frequently purchased groceries with association rules

#Step 1 - collectiong data
#using grocery data from one month of operations. data containes 9835 transactions.

#Step - 2 exploring and preparing the dta
rm(list=ls())
library(arules)

setwd("~/Machine Learning with Application in R/Chapter08")
groceries <- read.transactions("groceries.csv",sep=",")

#install.packages("arules")
#library(arules)

summary(groceries)

#to view the sparse matrix use the inspect() and vector operators
inspect(groceries[1:5])

#to check out the frequency which certain items occur in the date use the itemFrequency()
itemFrequency(groceries[,1:3])
#items in the sparse matrix are sorted alphabetically. abrasive cleaners and artificial sweeteners are
#found in about 0.3 percent of the transactions
#must times the values by 100 to get percent****

#visualizing item support - item frequency plots

itemFrequencyPlot(groceries,support = 0.1) #10 percent support. means it shows up in atlest 10% of transactions
#the support argument specifies the freqency which something must show in the data to be included in the graph

#if you would rather limit the graph to a certain number of items:
itemFrequencyPlot(groceries,topN=20) #top 20 grocery items

#to get a birds eye view of the entire sparse matrix using the image() function. 
#remember it is usually best to limit to a subset of the entire matrix when doing this
image(groceries[1:5]) #diagram for the first five transactions in the matrix. Still 169 possible items here

#you can combine this with the sample function to look at a randomly generated selection of 100 transactions

image(sample(groceries,100))

#Step 3 - training a model on the data
#the apriori() in the arules package

#finding association rules:
#myrules<-apriori(data=mydata,parameter=list(support=0.1,confidence =0.8, minlen =1))

#examining association rules:
#inspect(myrules)

apriori(groceries) #using default settings of support =0.1 and confidence =0.8
#need to widen our search

groceryrules <- apriori(groceries, parameter = list(support =
                                                      0.006, confidence =0.25, minlen=2))
groceryrules # we now have a list of 463 rules. Lets see if any are useful

#step 4 - evaluating model performance

summary(groceryrules) # obataining a high level of overview of the association rules

inspect(groceryrules[1:3])

#Step 5 - improving model performance

#sorting to find the best 5 rules using different measures (lift,support,confidence etc.)
inspect(sort(groceryrules, by ="lift")[1:5])

berryrules <- subset(groceryrules,items %in% "berries")

inspect(berryrules)

#saving association rules to a file or data frame
#saving to a csv:
write(groceryrules, file ="groceryrules.csv",
      sep=",", quote =TRUE, row.names =FALSE)

#writing to a dataframe:
groceryrules_df <-as(groceryrules,"data.frame")

#this creates a data frame with the rules in factor format, and numeric vectors for support, confidence and lift
str(groceryrules_df)

