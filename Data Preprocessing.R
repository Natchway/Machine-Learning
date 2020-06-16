#DATA PREPROCESSING


#1.Importing Data Set.

dataset = read.csv("Data.csv")




#2.Missing Data


#We will replace the missing values by the column means.
#In the ifelse statement we put three parameters;The if condition,the value to be returned if the condition is true,the value want to return if the condition is not true. 

dataset$Age = ifelse(is.na(dataset$Age),ave(dataset$Age,FUN = function(x) mean(x,na.rm = TRUE)),dataset$Age)
dataset$Salary = ifelse(is.na(dataset$Salary),ave(dataset$Salary,FUN = function(x) mean(x,na.rm = TRUE)),dataset$Salary)




#3.Encoding Categorical Data.


#We will be using the factor function to convert categorical data to numerical data

dataset$Country = factor(dataset$Country,levels = c("France","Spain","Germany"),labels = c(1,2,3))
dataset$Purchased = factor(dataset$Purchased,levels = c("No","Yes"),labels = c(0,1))




#4.Splitting dataset into Training Set and Test Set.


#Training Set is for machine learning model and Test set on which we test the performance of model. We will create a machine learning model and will test it over the test set.Performance on both sets should be more or less same.

#Installing Library

install.packages("caTools")

#Importing Library

library(caTools)

set.seed(123)

#SplitRatio=0.8 means we divide the dataset into 80:20 = training set:test set.
#We use Purchased as dependent variable,that's why we use dataset$Purchased.

split =  sample.split(dataset$Purchased,SplitRatio = 0.8)
split
training_set = subset(dataset,split == TRUE)
test_set = subset(dataset,split == FALSE)




#5.Feature Scaling.


#We need to put the variables on the same scale others the variables with bigger digits will dominate euclidean distance in machine learning equation. So we need to scale the data,so that no variable is dominated by other. We can use Standardization or Normalization methods. 
#By Standardization we mean, (x-mean)/sd.
#By Normalization we mean, (x-min)/range.

training_set = scale(training_set)
test_set = scale(test_set)


#If we run the two lines of code, we'll get error, as our training_set and test_set are not numeric. Because Country and Purchased was change into factors, where factors are not numeric. So we need to exclude the factor columns and the use scaling.

training_set[,2:3] = scale(training_set[,2:3])
test_set[,2:3] = scale(test_set[,2:3])




#6.Data Preprocessing Template.




