#Harrison Hugh McKenny
#Machine Learning with Applications in R
#April 28th 2019

#Setting Working Directory
setwd("~/Machine Learning with Application in R")

#-----Page 23 has a table that lists general types of machine learing algorithms to learning tasks---


#Algorithm: An unambiguous specification of how to solve a class of problems 

#----------------------------------Chapter 1 Introducing Machine Learning
#Mostly identifying successful application of algorithms and where applications were not successful

#Downloading the RWeka package
#Provides a collection of functions that give R access to the machine learning algorithms in the Java-based 
#---Weka software package by Ian H. Witten and Eibe Frank
install.packages("RWeka")
library(RWeka)

#----------------------------------Chapter 2 Managing and Understanding Data

#####Vectors
#Example 1 constructing a set of vectore containnig data on 3 medical patients
#Each vector is of a different type
subject_name= c("John Doe","Jane Doe", "Steve Graves")
temperature = c(98.1,98.6,101.4)
flu_status = c(FALSE,FALSE,TRUE)

#To obtain the temperature value for patient Jane Doe, the second patient:
temperature[2]

#ranges can be obtained using the :
temperature[2:3]

#items can be excluded using negative numbers
temperature[-2]

#using logical exclusions
temperature[c(TRUE,TRUE,FALSE)]

######Factors
#To create a factors from a character vector, simple apply the factor() function
gender=factor(c("MALE","FEMALE","MALE"))
gender

#Specifying potential levels not in the data that could show up elsewhere in a factor function
blood=factor(c("O","AB","A"),
             levels = c("A","B","AB","O"))
blood

#Factors with levels that are ordered "ordinal"
symptoms = factor(c("SEVERE","MILD","MODERATE"),
                  levels= c("MILD","MODERATE","SEVERE"),
                  ordered =TRUE)
#now levels are ordered and will include < < < arguments to show order
symptoms

#we can test whether each patients symptoms are more severe than moderate
symptoms>"MODERATE"

######Lists -> they are like vectors but allow us to store different data types together
#to get all info for subject 1 you would have to do this
subject_name[1]

temperature[1]

flu_status[1]

gender[1]
       
blood[1]

symptoms[1]

#a list allows us to access all of this information all at once

subject1=list(fullnames = subject_name[1],
              temperature = temperature[1],
              flu_status = flu_status[1],
              gender = gender[1],
              blood = blood[1],
              symptoms = symptoms[1])
subject1

subject1[2]

#lists retain order so all its components can be accessed using numeric positions i.e.
subject1[2]

#if the list component is a vector to return the numerical position of the vector that you want use double brackets
subject1[[2]]

#it is often better to access a list component by name than by numerical position
subject1$temperature

#obtaining several list items by specifying a vector of names
subject1[c('temperature','flu_status')]
pt_data=data.frame(subject_name,temperature,flu_status,gender,blood,symptoms,
                   stringsAsFactors = FALSE) #this line is important!! as R will automatically convert
#every character vector to a factor if we dont!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#peeping the dataframe
pt_data

#to extract vectors out of the dataframe "Columns"
pt_data$subject_name

#extracting multiple vectors out of dataframe
pt_data[c('temperature','flu_status')]

#can also do this by using numeric row commands, but using names doesnt helps keep everything sorted 
#when the order changes i.e. if youre using different dataframes for machine learning when automating

#to extract specific values in a dataframe, methods like with vectors are used, however because 
#dataframes are a matrix you must specify row and column. ROWS ARE FIRST, COLUMNS ARE SECOND!!!!
pt_data[1,2]

#multiple values from different rows/columns
pt_data[c(1,3),c(2,4)]

#to refer to every row or column leave the row or column blank
#extract all data from the first row
pt_data[1,]

#extract all data from the first column
pt_data[,1]

#to extract everything:
pt_data[,]

#columns are better accessed by name rather than position and negative signs can be used to exclude rows or coluns of data.
pt_data[c(1,3),c('temperature','gender')]

#this is equivalent to:
pt_data[-2,c(-1,-3,-5,-6)]

#to create new columns in an already existing data frame (perhaps as a function of existing columns)
pt_data$temp_c = (pt_data$temperature - 32)*(5/9)

#comparing celcius to farenheit temperature
pt_data[c("temperature","temp_c")]


########### Matrices and arrays
#like vectors, R matrices can contain only one type of data, although they are most often used for 
#mathematical operations and therefore typically store only numbers

#creating matrices
m = matrix(c(1,2,3,4),nrow=2)

m

#this is loaded in column-major order, to overide this, use the argument byrow=TRUE 

m = matrix(c(1,2,3,4),nrow=2, byrow=TRUE)

m

#more values to the matrix
m = matrix(c(1,2,3,4,5,6),nrow=2)

m

#requesting that the matrix has 2 columns
m = matrix(c(1,2,3,4,5,6),ncol=2)

m

#extracting data from matrices
m[1,]

m[,1]

#ARRAYS are multidimentional tables of data. Where matrix has rows and columns of values, an array has
#rows columns, and a number of additional layers of values. Although we will occasionally use matrices
#in later chapters, the use of arryas is unnecessary within the scope of this book.

################### MANAGING DATA WITH R

#Saving, loading and removing R data structures

#suppose you have 3 objects and you would like to save them to a permanent file. Regardless of if they
#are vectors, factors, lists or data frames they can be saved to a file named mydata.RData like this
save(x,y,z, file ="mydata.Rdata")

#The load function can recreate any data structures that have been saved to an .RData file.
#to load the mydata.RData file created in the preceding code, simply type:
load("mydata.RData")
#this will recreate x, y, and z data structures in you R enviroment
#be careful, this will overwrite things you have in your r script with the same name as things in the data
#file that you are loading!!!

#the ls() fuction returns a vector of all data structures currently in memory
ls()

#the rm function removes things from memory 
rm(m,subject1)

#to remove everything from memory you can reference the ls() functions
#SHAFRAN LOL
rm(list = ls())

#IMPORTING AND SAVING DATA FROM CSV FILES

#Importing CSV if we had the patient data saved as a CSV
pt_data = read.csv('pt_data.csv',stringsAsFactors = FALSE)

#if the CSV file does not include a header in file
mydata= read.csv("mydata.csv", stringsAsFactors = FALSE, header = FALSE)

#to save a CSV file use the write.csv() function. If you dataframe is named pt_data do this
write.csv(pt_data, file = "pt_data.csv",row.names = FALSE)

#EXPLORING AND UNDERSTANDING DATA

#must set working directory:
setwd("~/Machine Learning with Application in R/Chapter02")

usedcars=read.csv("usedcars.csv",stringsAsFactors = FALSE)

#first setp is aski9ng how the dataset is organized if youre lucky your source will 
#provide a data dictionary

#the str() function provides a method for displaying the structure of R objects, such as data frames
#vectors, or lists. it can be used to provide the vasic outline for our data dictionary
str(usedcars)

#**********************************GREAT COMMAND YOU LEARN A LOT ABOUT THE DATAFRAME USING THIS!!!!

#Exploring numeric variables
#summary statistics using the summary() command to display some common summary statistics
summary(usedcars$year)

#we can see that the year column is the year of the car and not year of the advertisement as we
#know that these cars were listed for sale in 2012

summary(usedcars[c("price","mileage")])

#measures of centeral tendency

#mean
(36000 +44000+56000)/3
#or using mean() function
mean(c(36000,44000,56000))

#looking at summaray price and milage statistics we see that the mean price is 12000ish and milage is
#44000ish. keeping this in mind going forward, we may hypothesize that the data set contains info on
#cars that are economy class

#we can also look at the median or the value that occurs at the midpoint of an ordered list of values
median(c(36000,44000,56000))

#mean is highly sensitive to outliers and we must keep this in mind. Sometimes median is much better

#the mean value of milage on cars is much higher than the median... causing us to suspect that there
#are some high milage used cars in the dataframe and we should be aware of this as we move forward

#MEASURING THE SPREAD

#the five number summary is a set of five statistics that roughly depict the spread of a features values. All five of the 
#stats are included in the output of the summary() function.

#the span between the min and max value is known as the range. in R the range() function returns both min
#and max values

range(usedcars$price)

#combining range with diff allows you to compute the range(used cars$price)

diff(range(usedcars$price))

18192

#looking at the interquartile range is of particular interest because it is the middle 50% of data

IQR(usedcars$price)

#quantile funcion provides a tool for indetifying quantiles fo a set of values. By defaul the quantile function 
#returns the five number summary
quantile(usedcars$price)

#by specifying an additional probs parameter using a vetor denotin cut points, we can obtain arbitrary quantiles
#first and 99th percentiles
quantile(usedcars$price,probs = c(.01,.99))

#the seq() function generates a sequence of evenly-spaced values. This makes it easy to obtain other slices
#other of data, such as the quintiles below:
quantile(usedcars$price,seq(from = 0, to=1, by = 0.2))

#--------visualizing numeric variables  boxplots
#displays center and spread of a numeric variable in a format that allows you to quickly obtain a
#sense of range and skew of a variable 

boxplot(usedcars$price,main = "Boxtplot of Used Car Prices", 
        ylab = "Price ($)")
    
boxplot(usedcars$mileage,main = "Boxplot of Used Car Mileage", ylab = "Odometer (mi.)" )    

#------------------------Visualizing numeric variables Histograms
#another way to visualize the spread of a numeric variable. 
hist(usedcars$price,main = "Histogram of Used Car Prices", 
     xlab= "Price ($)")

hist(usedcars$mileage, main = "Histogram of Used Car Mileage", 
     xlab="odometer (mi.)")

#heights indicate the "count" or frequency of values falling within each of the equal-width bins
#partitioning the values. The vertical lines that separate the bars, as laveled on the horizontal 
#axis, indicate the start and end oints of the range of values falliing within the bin

#-----------------------Measuring spread - variance and standard deviation
#wow this is hella basic......
#spread of a normal distribution is defined by its standard deviation
#to calculate, we must first obtain the variance: the average of the squared differences between each
#value and the mean value.
#standard deviation is the square root of the variance

var(usedcars$price)

sd(usedcars$price)

var(usedcars$mileage)

sd(usedcars$mileage)

#68,95 99.7 rule states that 68%,95% and 99.7% of values fall within one two and 3 st deviations of the mean respectively

#---------------------------------Exploring categorical variables
#the used cars dataset has 3 catagorical variables: model, color, transmission. R has stored
#these as character vectors rather than factor type vectors because we used the strins As Factors=FALSE
#parameter when loading the data. We could also treat year as a categorical vector
#categorical data is usually examined using tables rather than summary statistics.

#one-way table: single categorical variable
table(usedcars$year)

table(usedcars$model)

table(usedcars$color)

#R can also perform the calculation of table proportions directly, by using the prop.table command
#on the table produced by the table() function

model_table= table(usedcars$model)

prop.table(model_table)

#the results of this can be combined with other R functions to transform the output

#display results as percentages with a single decimal point

color_table=table(usedcars$color)
color_pct = prop.table(color_table) *100
round(color_pct, digits = 1)

#Measuring central tendency ---------------- The MODE


#mode: the most often occuring value
#typically used for catagorical data
#a variable with one mode is unimodal while a variable with 2 modes is bimodal, multimodal = 3 or more
#****You cannot use the mode() function to get the mode, as R uses this to otain the type of variable
#(as in numeric, list, etc.) rather than the statistical mode. Instead you must look at the table()
#function for the catagory with the greatest amount of values

#--------------------------Visualizing relationships - scatterplots
#scatterplot: a diagram that visualies a bivariate relationship between numeric features.

#to use the plot() fuction we need to specify x and y vectors conaining the values used to position the dots
#on the figure. Convention dictates that the y variable is the one that is presumed to depend on the other 
#and is there for known as the dependant variable

plot(x= usedcars$mileage, y = usedcars$price, 
     main = "scatterplot of Price vs. Mileage",
     xlab= "Used Car Odometer (mi.)",
     ylab = "Used Car Price ($)")
#The relationship we observe between car prices and mileage is known as a negative association 
#because it forms a pattern of dots in a line sloping downward. A positive association would appear
#to form a line sloping upward. A flat line, or a seemingly random scattering of dots is evidence that
#the two variables are not associated at all

#the strength of a linear association betwenn two variable is measured by a statistic known as correlation

#*********************************************IMPORTANT***********************************
#not all associations form straight lines. Sometimes tht dots form a u shape or v shape, while sometimes the
#pattern seems to be weaker or stronger for increasing values of the x or y variable. such patterns
#imply that the relationship between the two variables is not linear. 

#Examining relationships - two way cross- tabulations
#to examine a relationship between two nominal variables a two-way cross tabulation is used
#allows you to examine how the values of one variable vary by the values of another .
#the format is a table in which the rowsare the levels of one variable, while the columns are the levels of another
#Counts in each of the tables cells indicate the number of values falling into the particular row and column combination

#to see if there is a relationship between model and color we will examine a crosstab.

install.packages("gmodels")

library(gmodels)

usedcars$conservative = usedcars$color %in% c("Black","Gray","Silver","White")

#the %in% operator returns TRUE or FALSE for each value in the cector on the left hand side of the operator. 
table(usedcars$conservative)

#now lets look at a cross tabulation to see how the proportion of conservativley colored cars varies
#by model. Since we're assuming that the model of car dictates the choice of color, we'll treat the 
#conservative color indicator as the dependent variable

CrossTable(x = usedcars$model, y = usedcars$conservative)
