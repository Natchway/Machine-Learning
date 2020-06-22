# Decision Tree Regression

#1.Importing the dataset.

dataset = read.csv('Position_Salaries.csv')
dataset=dataset[,2:3]


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



#4.Fitting Decision Tree Regression to the dataset.

install.packages('rpart')
library(rpart)
regressor = rpart(formula = Salary ~ .,
                  data = dataset)
# Here we have no split in data regarding the regressor variables so we don't have 
# several means regarding the different partions. So we will add another parameter
# in the rpart. If we view the graph there is only one straight line, but after the
# extra parameter we get several splits or lines which is regarding the different
# spliting in the data.

regressor = rpart(formula = Salary ~ .,
                  data = dataset, control = rpart.control(minsplit = 1))


#5.Predicting a new result with Decision Tree Regression.


y_pred = predict(regressor, data.frame(Level = 6.5))



#6.Visualising the Decision Tree Regression results (higher resolution).


# install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.0001)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Decision Tree Regression)') +
  xlab('Level') +
  ylab('Salary')



#7.Plotting the tree.

plot(regressor)
text(regressor)