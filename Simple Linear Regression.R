#SIMPLE LINEAR REGRESSION




#1.Importing the dataset.

dataset = read.csv("Salary_Data.csv")
#dataset = dataset[,2:3]

#2.Splitting the dataset into the Training set and Test set.

#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Salary, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


#3.Feature Scaling.
#training_set[,2:3]=scale(training_set[,2:3])
#test_set[,2:3]=scale(test_set[,2:3])



#4.Fitting Simple Linear Regression to the Training Set.


regressor = lm(formula = Salary ~ YearsExperience,data=training_set)
summary(regressor)



#5.Predicting Test Set Results.


#Here we are interested in predicting the new Salaries corresponding to the YearsExperience, 
#So we pass the YearsExperience through the model and predict the Salary of the test_set
y_predict = predict(regressor,new_data = test_set)
y_predict




#6.Visualizing the Training Set results



library(ggplot2)
#Here we find the points vs the line diagram. We put YearsExperience of training_set in x axis 
#and put training_set salary and predicted salary on the y axis.


ggplot()+
  geom_point(aes(x = training_set$YearsExperience,y = training_set$Salary),colour = 'red')+ 
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor,new_data = training_set)),colour = 'blue')+
  ggtitle("Salary vs Experience")+ 
  xlab("Years of Experience")+ ylab("Salary")




#6.Visualizing the Test Set results




library(ggplot2)
#Here we find the points vs the line diagram. We put YearsExperience of test_set in x axis 
#and put training_set salary and predicted salary on the y axis.

ggplot()+
  geom_point(aes(x = test_set$YearsExperience,y = test_set$Salary),colour = 'red')+ 
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor,new_data = training_set)),colour = 'blue')+
  ggtitle("Salary vs Experience")+ 
  xlab("Years of Experience")+ ylab("Salary")
