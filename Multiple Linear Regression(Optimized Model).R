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


#6.Optimized Model.

# We will use the backward elimination to get the best model. We will fit the full
# model and then check which regressor has the maximum p value, then compare it 
# with the significance level. If p-value>level of significance then we remove the 
# variable

summary(regressor)

# Now we check for the conditions and eliminate to get the best result by using 
# a foor loop we can check for all the variables at a once.

backwardElimination <- function(x, sl) {
  #Here in x we will input the dataset we want to work on
  numVars = length(x)
  #Here we are including Y and all the regressor variables in numVars
  for (i in c(1:numVars)){
    regressor = lm(formula = Profit ~ ., data = x)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    #coef gives the coefficients of the model.
    #Here from the summary of regressor we want to extract the rows of 
    #all variables and p-value column. So we extract it using [,]. Then 
    #we s=use the max to get the max of those p-values.
    if (maxVar > sl){
      #Now we check if max value is greater than or not respect to level of
      #significnce
      j = which(maxVar == coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
      #By the which function we the array position and then we remove that
      #variable by it's array position
      x = x[, -j]
    }
    numVars = numVars - 1
  }
  return(summary(regressor))
  #When the if loop stops we get the summary of the final result
}


# WE set the level of significance

SL = 0.05

#Now we pass the arguments throught the function backwardElimination

backwardElimination(training_set, SL)
summary(regressor)
