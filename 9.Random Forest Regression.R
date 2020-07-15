# RANDOM FOREST REGRESSION


#1.Importing the dataset.

dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]

#2.Splitting the dataset into the Training set and Test set
# # install.packages('caTools')
# library(caTools)
# set.seed(123)
# split = sample.split(dataset$Salary, SplitRatio = 2/3)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)



#3.Feature Scaling.
# training_set = scale(training_set)
# test_set = scale(test_set)



#4.Fitting Random Forest Regression to the dataset

install.packages('randomForest')
library(randomForest)
set.seed(1234)
regressor = randomForest(x = dataset[-2],
                         y = dataset$Salary,
                         ntree = 500) #Increase the ntree value for better result
# Here x is a matrix and y is a vector, so we use array setup while using what
# columns to use and use the dollar sign to extract the vector column from the 
# dataset



#5.Predicting a new result with Random Forest Regression

y_pred = predict(regressor, data.frame(Level = 6.5))



#6.Visualising the Random Forest Regression results (higher resolution)

install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.001)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Random Forest Regression)') +
  xlab('Level') +
  ylab('Salary')
