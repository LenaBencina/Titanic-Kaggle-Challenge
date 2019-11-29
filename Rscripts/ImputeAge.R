library(dplyr)
library(rcompanion)
library(reshape2)

load(file = '/Users/Lena/Documents/Titanic-Kaggle-Challenge/train_data_missing_age.RData')

# lets try to impute age feature..
# first we need to split the data
train.data.missing.age = train.data[which(is.na(train.data$Age)),] # 177
train.data.known.age = train.data[which(!is.na(train.data$Age)),] # 712

# first some visualizations... to get more insight about the feature
# simple distribution plot
ggplot(train.data.known.age, aes(x = Age)) + geom_histogram(bins = 30)


# which features we want to use as predictors?
# intuitive guess would be: Pclass, Name.title, Sex and Travel.alone
lm.model1 = glm(Age ~ Pclass + Name.title + Sex + Travel.alone, data = train.data.known.age, family = gaussian)
summary(lm.model1)

# from the model summary we can se that Sex is not stat. significant

# this plot partly explains why ... the distribution is very similar
ggplot(train.data.known.age, aes(x=Age)) + geom_histogram(position = 'identity', bins = 40, binwidth = 1) + facet_grid(Sex ~ .)

# visualization of other features
ggplot(train.data.known.age, aes(x=Age)) + geom_histogram(position = 'identity', bins = 40, binwidth = 1) + facet_grid(Name.title ~ .)
ggplot(train.data.known.age, aes(x=Age)) + geom_histogram(position = 'identity', bins = 40, binwidth = 1) + facet_grid(Travel.alone ~ .)
ggplot(train.data.known.age, aes(x=Age)) + geom_histogram(position = 'identity', bins = 40, binwidth = 1) + facet_grid(Pclass ~ .)


# exclude Sex and try to include SibSp, Parch and Family.size
lm.model2 = glm(Age ~ Pclass + Name.title + Travel.alone + SibSp + Parch, data = train.data.known.age, family = gaussian)
summary(lm.model2)

# remove SibSp and Parch (high pvalue)
lm.model3 = glm(Age ~ Pclass + Name.title + Travel.alone, data = train.data.known.age, family = gaussian)
summary(lm.model3)

# lets see the training accuracy
age.predicted.trained = predict(lm.model3, train.data.known.age) 
age.diff = data.frame(age.predicted = ifelse(age.predicted.trained > 1, round(age.predicted.trained), age.predicted.trained),
                      age.real = train.data.known.age$Age,
                      index = 1:nrow(train.data.known.age))

ggplot(melt(id.vars = 'index', data = age.diff), aes(x = reorder(index, value), y = value, color = variable)) + geom_point()
# not the best model but lets just try to proceed with survival prediction and get back to this later..

# we can now predict the values with lm.model3
age.predicted = predict(lm.model3, train.data.missing.age) 
train.data.missing.age$Age = ifelse(age.predicted > 1, round(age.predicted), age.predicted)

# save the model
save(lm.model3, file = '/Users/Lena/Documents/Titanic-Kaggle-Challenge/Objects/age_imputation_glm_model.RData')



