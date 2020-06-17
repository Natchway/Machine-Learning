#POLYNOMIAL REGRESSION MODEL

#1.Importing the dataset.


dataset = read.csv('Position_Salaries.csv')

# Here machine is polynomial regression model is itself a model and learning is related 
# to the fact that this polynomial regression model will learn the correlation between 
# level and salary to predict new salary of a given level.Suppose we want to predict
# the salary of level 6.5

# From the data set we see that the 1st column=Resources will not be used as it is 
# encoded in the 2nd column, we'll just use the 2nd and the 3rd column from the dataset. 

dataset = dataset[2:3]



#2.Splitting the dataset into the Training set and Test set.



# We do  not need to create training_set and test_set
# install.packages('caTools')
# library(caTools)
# set.seed(123)
# split = sample.split(dataset$Salary, SplitRatio = 2/3)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)




#3.Feature Scaling.



# We do not need to apply feature scaling as the data set is very small.
# training_set = scale(training_set)
# test_set = scale(test_set)




#4.Fitting Linear Regression to the dataset.



lin_reg = lm(formula = Salary ~ .,
             data = dataset)

# Here we use "." to use all the independent variables except the Salary.
# As we have not created two separate datasets, we will use the whole dataset.




#5.Fitting Polynomial Regression to the dataset.



# We are going to build the polynomial terms,

dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3
# We will add more levels as third degree polynomial was not fitted accurately. 
dataset$Level4 = dataset$Level^4
dataset$Level5 = dataset$Level^5
poly_reg = lm(formula = Salary ~ .,data = dataset)




#6.Visualizing the Linear Regression results.



# install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(lin_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Linear Regression)') +
  xlab('Level') +
  ylab('Salary')

# One has to be very careful, here the dataset we use has been edited and three extra 
# columns have been added. So we can run this graphical part before running the part 
# where we just fit a polynomial regression model, as the dataset changes there due 
# to adding three levels.




#7.Visualising the Polynomial Regression results.



# install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(poly_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Polynomial Regression)') +
  xlab('Level') +
  ylab('Salary')





#8.Visualising the Regression Model results (for higher resolution and smoother curve).



# install.packages('ggplot2')
library(ggplot2)
#x_grid is a sequence,not a data frame.
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(poly_reg,
                                        newdata = data.frame(Level = x_grid,
                                                             Level2 = x_grid^2,
                                                             Level3 = x_grid^3,
                                                             Level4 = x_grid^4,
                                                             Level5 = x_grid^5
                                                             ))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Polynomial Regression)') + xlab('Level') + ylab('Salary')
  



#9.Predicting a new result with Linear Regression.

# We create a new data frame only containing the level 6.5
y_pred = predict(lin_reg, data.frame(Level = 6.5))




#10.Predicting a new result with Polynomial Regression.


predict(poly_reg, data.frame(Level = 6.5,Level2 = 6.5^2,Level3 = 6.5^3,
                             Level4 = 6.5^4,Level5 = 6.5^5))
