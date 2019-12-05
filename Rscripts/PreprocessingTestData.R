# apply the same transformation (as used in preprocessing training data) to the test data

# libraries
library(tibble)
library(Amelia)
library(dplyr)
library(stringr)
library(ggplot2)
library(reshape2)
library(tidyr)

# import test data
test.data.original = as_tibble(read.csv('Data/test.csv', stringsAsFactors = FALSE, na.strings=c('NA','NaN', '')))

### convert to factor
test.data = test.data.original
factor.cols = c('Sex', 'Ticket', 'Cabin', 'Embarked') # list all potential factor columns
test.data[factor.cols] = lapply(test.data[factor.cols], factor)

### handle missing values
# check missing data in %
round(sort(sapply(test.data, function(x){sum(is.na(x))}))/nrow(test.data)*100, 2)
# plot missing data
missmap(test.data, col=c('yellow', 'black'), main = 'Missing values in test data', legend=FALSE)

# 1. remove Cabin feature due to high amount of missing values
test.data$Cabin = NULL

# 2. Embarked is not missing here like in the train set

# 3. Age: use imputation model trained on train set
#... come back to this after other transformations are done ...

# 4. Additional feature with missing values - FARE
# because there is only one observation without fare value we will not waste time and take the average
test.data[which(is.na(test.data$Fare)),] # id = 1044
avg.fare = mean(test.data$Fare, na.rm = TRUE)
test.data = test.data %>% mutate(Fare = replace(Fare, which(is.na(Fare)), avg.fare))


### feature transformation

# 1. NAME: extract the name title and group it
test.data = test.data %>% 
            mutate(Name = as.factor(str_remove(str_extract(string = Name, pattern = '[a-zA-Z]+\\.'), '\\.'))) %>%
            rename(Name.title = Name)

title.conv = c(Capt='Officer', Col='Officer', Major='Officer', Jonkheer='Royalty', Don='Royalty', Sir='Royalty',
               Dr='Officer', Rev='Officer', Countess='Royalty', Dona='Royalty', Mme='Mrs', Mlle='Miss', Ms='Mrs',
               Mr='Mr', Mrs='Mrs', Miss='Miss', Master='Master', Lady='Royalty')

test.data = test.data %>% mutate(Name.title = as.factor(title.conv[as.character(Name.title)]))


# 2. SEX: change female/male to F/M for less text
test.data = test.data %>%
            mutate(Sex = as.factor(ifelse(Sex == 'female', 'F', 'M')))



# 3. Create a new feature Family size from summing #parent/#children, #spouses/siblings + 1 for the observed passenger
test.data$Family.size = as.integer(test.data$Parch + test.data$SibSp + 1)
test.data$Parch = NULL
test.data$SibSp = NULL


# 4. Create a new boolean feature if traveling alone
test.data$Travel.alone = ifelse(test.data$Family.size == 1, TRUE, FALSE)

# 5. TICKET
test.data$Ticket = NULL




#### get back to age imputation (=the last transformation)
load('Objects/age_imputation_glm_model.RData')

# split test data
test.data.missing.age = test.data[which(is.na(test.data$Age)),]
test.data.known.age = test.data[which(!is.na(test.data$Age)),]

# predict on known age (to evaluate)
age.predicted.trained = predict(lm.model3, test.data.known.age) 
age.diff = data.frame(age.predicted = ifelse(age.predicted.trained > 1, round(age.predicted.trained), age.predicted.trained),
                      age.real = test.data.known.age$Age,
                      index = 1:nrow(test.data.known.age))


ggplot(melt(id.vars = 'index', data = age.diff), aes(x = reorder(index, value), y = value, color = variable)) +
  geom_point() +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.position = 'bottom') +
  labs(color = '', y = 'Age') +
  ggtitle("Visualization of age imputation model's accuracy")
# not the best model but lets just try to proceed with survival prediction and get back to this later..

# we can now predict the actual missing values with lm.model3
age.predicted = predict(lm.model3, test.data.missing.age) 
test.data.missing.age$Age = ifelse(age.predicted > 1, round(age.predicted), age.predicted)

# combine parts of test data and save it
test.data = arrange(rbind(test.data.missing.age, test.data.known.age), PassengerId)
save(test.data, file = 'Objects/preprocessed_test_data.Rdata')


