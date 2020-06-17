# SVR

#1.Importing the dataset.
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]




#2.Splitting the dataset into the Training set and Test set.
# install.packages('caTools')
# library(caTools)
# set.seed(123)
# split = sample.split(dataset$Salary, SplitRatio = 2/3)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)




#3.Feature Scaling.
# training_set = scale(training_set)
# test_set = scale(test_set)




#4.Fitting SVR to the dataset.
install.packages('e1071')
library(e1071)
dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3
# We will add more levels as third degree polynomial was not fitted accurately. 
dataset$Level4 = dataset$Level^4
regressor = svm(formula = Salary ~ .,
                data = dataset,
                type = 'eps-regression',#This type argument will specify what type of model we are using
                kernel = 'radial') #We are using Gaussian Kernel




#5.Predicting a new result.
y_pred = predict(regressor, data.frame(Level = 6.5))



#6.Visualising the SVR results (for higher resolution and smoother curve).
# install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid,
                                                                        Level2 = x_grid^2,
                                                                        Level3 = x_grid^3,
                                                                        Level4 = x_grid^4))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (SVR)') + xlab('Level') + ylab('Salary')
