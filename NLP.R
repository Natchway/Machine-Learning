# NATURAL LANGUAGE PROCESSING



#1.Importing the dataset.
# If we use read.csv then .tsv file will not open, so we use the read.delim for the tab separated file
# We use quote='' to ignore the quotes in the file as we set quote=nothing
# We use stringAsFactors=F, as in NLP we dont use factors as factors is a single entity, whereas we want the each words separately in a string
dataset_original = read.delim('Restaurant_Reviews.tsv', quote = '', stringsAsFactors = FALSE)




#2.Cleaning the texts.
# install.packages('tm')
# install.packages('SnowballC')
library(tm)
library(SnowballC) #package used for stopwords()


#We will clean the reviews and store it in corpus
corpus = VCorpus(VectorSource(dataset_original$Review))
#To acess the first review
as.character(corpus[[1]])


#We convert all the reviews to lowercases, as we can have same words with upper case and lower case
#content_transformer transforms to lower case and tm_map halps to change for all.
corpus = tm_map(corpus, content_transformer(tolower))
#To acess the first review
as.character(corpus[[1]])


#Now we will remove Numbers by tm_map function
corpus = tm_map(corpus, removeNumbers)
#To acess the 900th review
as.character(corpus[[900]])



#Now we will remove Punctuations by tm_map function
corpus = tm_map(corpus, removePunctuation)
#To acess the 1st review
as.character(corpus[[1]])



#Now we will remove the non relevant words using stopword() by tm_map function
corpus = tm_map(corpus, removeWords, stopwords())
#To acess the 560th review
as.character(corpus[[560]])



#Now we will consider only the root words, as LOVE,LOVED,LOVING all have same root word LOVE.
#We will use stemming for any texts, reviews,html etc
corpus = tm_map(corpus, stemDocument)
#To acess the 1st review,we see here the root of LOVED is LOVE
as.character(corpus[[1]])



#Now we will remove the extra space after removing the above things(numbers,stemwords etc..)
corpus = tm_map(corpus, stripWhitespace)
#To acess the 560th review
as.character(corpus[[560]])


# Here the independent variables are the words corresponding to each review and the dependent variable is the outcome(Liked or not)


#3.Creating the Bag of Words model.
# Here will create columns for each word available from our reviews, each cell will provide review corresponding to each row and one word corresponding to each word,thus number of times the word appeared in the review
# We create a spars matrix as many entries will be zero.
# We created the corposes to create a matrix with minimum number of words.
dtm = DocumentTermMatrix(corpus)

#Now we will filter the non frequent words, if we want to keep 99.9% of the words are more frequent words we use 0.999
dtm = removeSparseTerms(dtm, 0.999)

# Here the independent variables are in a matrix, so we'll convert it into data frame
dataset = as.data.frame(as.matrix(dtm))

# Here we add the dependent column "Liked" to the data frame
dataset$Liked = dataset_original$Liked





#6.Encoding the target feature as factor.
dataset$Liked = factor(dataset$Liked, levels = c(0, 1))



#7.Splitting the dataset into the Training set and Test set.
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Liked, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)




#8.Fitting Random Forest Classification to the Training set.
# install.packages('randomForest')
library(randomForest)
classifier = randomForest(x = training_set[-692],
                          y = training_set$Liked,
                          ntree = 10)



#9.Predicting the Test set results.
y_pred = predict(classifier, newdata = test_set[-692])


#10.Making the Confusion Matrix.
cm <- table(test_set[, 692], y_pred)
cm
