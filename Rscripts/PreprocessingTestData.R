# apply the same transformation (used in preprocessing training data) to the testing data

library(tibble)

# import data
test.data = as_tibble(read.csv('/Users/lena/Documents/Titanic-Kaggle-Challenge/Data/test.csv', stringsAsFactors = FALSE, na.strings=c('NA','NaN', '')))

# convert to factor
factor.cols = c('Sex', 'Ticket', 'Cabin', 'Embarked') # list all potential factor columns
test.data[factor.cols] = lapply(test.data[factor.cols], factor)

