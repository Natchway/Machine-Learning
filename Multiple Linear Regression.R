#MULTIPLE LINEAR REGRESSION

#1.Importing the dataset.

dataset = read.csv('50_Startups.csv')



#2.Encoding categorical data.

dataset$State = factor(dataset$State,
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1, 2, 3))



#3.Splitting the dataset into the Training set and Test set.

# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


#4.Feature Scaling.

# training_set = scale(training_set)
# test_set = scale(test_set)


#5.Fitting Multiple Linear Regression to the Training set.

regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
               data = training_set)

# Instead of writing the all the independent variables we can use '.'

regressor = lm(formula = Profit ~ .,
               data = training_set)


#6.Predicting the Test set results.

y_pred = predict(regressor, newdata = test_set)