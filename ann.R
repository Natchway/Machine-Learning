# ARTIFICIAL NEURAL NETWORK



#1.Importing the dataset.
dataset = read.csv('Churn_Modelling.csv')
# Here we see that column 4 to column 14 are important to the dependent variable.
dataset = dataset[4:14]




#2.Encoding the categorical variables as factors.
# Here we convert into factor and then convert them into numeric, so we get numeric factors. So we use the as.numeric after converting the factors.
dataset$Geography = as.numeric(factor(dataset$Geography,
                                      levels = c('France', 'Spain', 'Germany'),
                                      labels = c(1, 2, 3)))
dataset$Gender = as.numeric(factor(dataset$Gender,
                                   levels = c('Female', 'Male'),
                                   labels = c(1, 2)))




#3.Splitting the dataset into the Training set and Test set.
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Exited, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)




#4.Feature Scaling.
training_set[-11] = scale(training_set[-11])
test_set[-11] = scale(test_set[-11])




#5.Fitting ANN to the Training set.
install.packages('h2o')
library(h2o)
h2o.init(nthreads = -1, max_mem_size = '2g', ip = "127.0.0.1", port = 54321) #Connecting to the server
# Here we need to convert the training set to a h2o frame, so we use as.h2o to convert it.
model = h2o.deeplearning(y = 'Exited',
                         training_frame = as.h2o(training_set),
                         activation = 'Rectifier',
                         hidden = c(5,5),
                         epochs = 100,
                         train_samples_per_iteration = -2)

# Predicting the Test set results
y_pred = h2o.predict(model, newdata = as.h2o(test_set[-11]))
y_pred = (y_pred > 0.5)
y_pred = as.vector(y_pred)

# Making the Confusion Matrix
cm = table(test_set[, 11], y_pred)

# h2o.shutdown()