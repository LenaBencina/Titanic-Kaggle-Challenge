# libraries
library(tibble)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# FEATURES INFORMATION
# PassengerId
# Survived
# Pclass = Ticket class i.e. A proxy for socio-economic status (SES) (1/2/3)
# Name 
# Sex
# Age
# SibSp = # of siblings / spouses aboard
# Parch = # of parents / children aboard
# Ticket = Ticket number
# Fare = Passenger fare
# Cabin = Cabin number
# Embarked = Port of Embarkation (C/Q/S)


### import data
train.data = as_tibble(read.csv('/Users/lena/Documents/Machine Learning/titanic/Data/train.csv', stringsAsFactors = FALSE, na.strings=c('NA','NaN', '')))

# check number od observations
n.train = nrow(train.data)
n.train

# check features and their types
head(train.data)

# change types of some columns to factor
factor.cols = c('Survived', 'Sex', 'Ticket', 'Cabin', 'Embarked') # list all potential factor columns
train.data[factor.cols] = lapply(train.data[factor.cols], factor)

### check missing values in %
sort(sapply(train.data, function(x){sum(is.na(x))}))/n.train*100

# 1. remove Cabin feature due to high amount of missing values
train.data$Cabin = NULL

# 2. either remove or set to 'other' -> because there are only 2 observations, we will remove it 
table(train.data$Embarked, exclude = FALSE)
train.data = train.data[!is.na(train.data$Embarked),]

# 3. lets analyse the Age feature
ggplot(train.data, aes(x=Age)) + geom_histogram(binwidth=1, colour='black')

# lets predict missing age values from other variables
#...we will come back to this...


### feature engineering:
# name feature is not so useful.. lets try to extract the name title
train.data = train.data %>% 
             mutate(Name = str_remove(str_extract(string = Name, pattern = "[a-zA-Z]+\\."), '\\.')) %>%
             rename(Name.title = Name)


# change female/male to F/M
train.data = train.data %>%
             mutate(Sex = ifelse(Sex == 'female', 'F', 'M'))


View(train.data)


# open questions:
# Should Pclass be of type 'ordered factor' instead of integer
# How important are Ticket number/Fare/Embarked features
# Could we get any more info from SibSp/Parch features





### check the distribution of independent variable
table(train.data$Survived)


