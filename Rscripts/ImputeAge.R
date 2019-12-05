# libraries
library(reshape2)
library(ggplot2)

### Additional function for plotting distribution of Age by different features #############################
plot_age_dist_by_feature = function(feature){
  ggplot(train.data.known.age, aes(x=Age)) + 
    geom_histogram(position = 'identity', bins = 40, binwidth = 1, color = 'black') + 
    facet_wrap(reformulate(feature), ncol = 1)  
}
############################################################################################################


# load preprocessed train data ready to impute age
load(file = 'Objects/train_data_missing_age.RData')

# lets try to impute age feature..
# first we need to split the data
train.data.missing.age = train.data[which(is.na(train.data$Age)),] # 177
train.data.known.age = train.data[which(!is.na(train.data$Age)),] # 712

# first some visualizations... to get more insight about the feature
# simple distribution plot
age.dist.plot = ggplot(train.data.known.age, aes(x = Age)) + 
                geom_histogram(bins = 30, fill ='#275176', colour= '#81C853') +
                ggtitle('Distribution of Age before imputation') +
                ylim(0, 150) 

# We will use linear regression model to impute age
# question: which features we want to use as predictors?
# intuitive guess would be: Pclass, Name.title, Sex and Travel.alone
lm.model1 = glm(Age ~ Pclass + Name.title + Sex + Travel.alone, data = train.data.known.age, family = gaussian)
summary(lm.model1)

# from the model summary we can se that Sex and Miss class of Name.title are not stat. significant
# is the problem correlation between features Sex and Name.title?

# lets visualize it
plot_age_dist_by_feature('Sex')
# age distribution is very similar between Sex categories

# visualizations of other features
plot_age_dist_by_feature('Name.title')
plot_age_dist_by_feature('Travel.alone')
plot_age_dist_by_feature('Pclass')


# exclude Sex and try to include Family.size
lm.model2 = glm(Age ~ Pclass + Name.title + Travel.alone + Family.size, data = train.data.known.age, family = gaussian)
summary(lm.model2)

# remove Family size (high pvalue)
lm.model3 = glm(Age ~ Pclass + Name.title + Travel.alone, data = train.data.known.age, family = gaussian)
summary(lm.model3)

# lets see the training accuracy
age.predicted.trained = predict(lm.model3, train.data.known.age) 
age.diff = data.frame(age.predicted = ifelse(age.predicted.trained > 1, round(age.predicted.trained), age.predicted.trained),
                      age.observed = train.data.known.age$Age,
                      index = 1:nrow(train.data.known.age))

age.imputation.accuracy.plot = ggplot(melt(id.vars = 'index', data = age.diff), aes(x = reorder(index, value), y = value, color = variable)) +
  geom_point() +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.position = 'bottom') +
  labs(color = '', y = 'Age') +
  ggtitle("Visualization of age imputation model's accuracy") +
  scale_color_manual(values=c('#275176', '#81C853'))

  
# not the best model but lets just try to proceed with survival prediction and get back to this later it there will be time..

# we can now predict the missing values with lm.model3
age.predicted = predict(lm.model3, train.data.missing.age) 
# round values higher than 1
train.data.missing.age$Age = ifelse(age.predicted > 1, round(age.predicted), age.predicted)

# save the model (we will use it on the test data later as well)
#save(lm.model3, file = 'Objects/age_imputation_glm_model.RData')

# combine the data (known and imputed)
train.data.imputed = rbind(train.data.known.age, train.data.missing.age)

# new distribution
age.dist.imputed.plot = ggplot(train.data.imputed, aes(x = Age)) + 
                        geom_histogram(bins = 30, fill ='#275176', colour= '#81C853') +
                        ggtitle('Distribution of Age after imputation') +
                        ylim(0, 150) 


# save the final version of train data - ready for training
#save(train.data.imputed, file = 'Objects/preprocessed_train_data.RData')


