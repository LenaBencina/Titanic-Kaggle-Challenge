# libraries
library(tibble)
library(Amelia) # for missmap
library(dplyr)
library(stringr)
library(ggplot2)


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


# import train data
train.data = as_tibble(read.csv('Data/train.csv', stringsAsFactors = FALSE, na.strings=c('NA','NaN', '')))

# check number od observations
n.train = nrow(train.data)
n.train

# check features and their types
head(train.data)

# change types of categorical features to factor
factor.cols = c('Survived', 'Sex', 'Ticket', 'Cabin', 'Embarked') # list all potential factor columns
train.data[factor.cols] = lapply(train.data[factor.cols], factor)


# CHECK MISSING VALUES #####################################################################################################

# check missing data in %
round(sort(sapply(train.data, function(x){sum(is.na(x))}))/n.train*100, 2)

# plot missing data
missmap(train.data, col=c('yellow', 'black'), main = 'Missing values in train data', legend=FALSE)

# 1. remove Cabin feature due to high amount (77%) of missing values
train.data$Cabin = NULL

# 2. either remove or set to 'other' -> because there are only 2 observations, we will remove it 
table(train.data$Embarked, exclude = FALSE)
train.data = train.data[!is.na(train.data$Embarked),]

# 3. age is an important feature
# lets predict missing age values from other variables
#...we will come back to this...

# First we need to analyse all the features...


############################################################################################################################
# additional function for plotting distribution for each categorical feature vs survived
plot_survival_dist_by_feature = function(data, feature){
  data %>% group_by_(feature, 'Survived') %>% 
    summarise(count = n()) %>% 
    ggplot(aes_string(x=feature, y='count', fill='Survived')) + 
           geom_bar(stat='identity') + 
           scale_fill_manual(values=c("#999999", "#E69F00")) +
           ggtitle(paste('Distribution of survival by', feature))
}

# ANALYSE EACH FEATURE #####################################################################################################

# 0. First check the overall distribution of dependent variable..
table(train.data$Survived)
# Enough observations in each class..

# 1. NAME: name feature is not so useful.. lets try to extract the name title
train.data = train.data %>% 
             mutate(Name = as.factor(str_remove(str_extract(string = Name, pattern = '[a-zA-Z]+\\.'), '\\.'))) %>%
             rename(Name.title = Name)


# distribution of name titles
table(train.data$Name.title) 

# lets seperate classes with high occurence vs low for better visualization
tmp.name.df = train.data[, c('Name.title', 'Survived')]
tmp.name.df$group = 'low'
high.values = names(table(tmp.name.df$Name.title))[table(tmp.name.df$Name.title) > 10]
tmp.name.df[which(tmp.name.df$Name.title %in% high.values), 'group'] = 'high'

# plot distribution
plot_survival_dist_by_feature(filter(tmp.name.df, group == 'low'), 'Name.title')
plot_survival_dist_by_feature(filter(tmp.name.df, group == 'high'), 'Name.title')

# Should we combine the classes?
# classes with 1 occurence can cause "problems".. lets try to combine it
title.conv = c(Capt='Officer', Col='Officer', Major='Officer', Jonkheer='Royalty', Don='Royalty', Sir='Royalty',
               Dr='Officer', Rev='Officer', Countess='Royalty', Dona='Royalty', Mme='Mrs', Mlle='Miss', Ms='Mrs',
               Mr='Mr', Mrs='Mrs', Miss='Miss', Master='Master', Lady='Royalty')


train.data = train.data %>% mutate(Name.title = as.factor(title.conv[as.character(Name.title)]))
table(train.data$Name.title)

# plot with new classes
plot_survival_dist_by_feature(train.data, 'Name.title')



# 2. SEX: change female/male to F/M for less text..
train.data = train.data %>%
             mutate(Sex = as.factor(ifelse(Sex == 'female', 'F', 'M')))

plot_survival_dist_by_feature(train.data, 'Sex')
# It seems like sex is correlated with survival (more women survived and more men died)


# 3. Pclass: Should Pclass be of type 'ordered factor' instead of integer?
# We will leave it as integer because we want to incorporate the order and the relation between categories
plot_survival_dist_by_feature(train.data, 'Pclass')
# Pclass also looks correlated with the independent feature..
# In 1st class more people survived and in 3rd class more people died..


# 4. Ticket
table(train.data$Ticket)
# There is a lot of ticket classes with only 1 or 2 observations

table(table(train.data$Ticket))
# A lot (547) of different ticket numbers occure only once..
# Is this info relevant?
# Maybe there is a connection between people with the same ticket numbers?
# Most probably there are many new ticket categories in the test set..
# Lets remove it
train.data$Ticket = NULL



# 5. Fare
ggplot(train.data, aes(x=Fare)) + 
  geom_histogram(binwidth=1, colour='black') +
  ggtitle('Distribution of Fare')
    
# This is the info for how much each passenger payed for its ticket.
# This feature is relevant for sure.. Lets check how big were the fares from survived vs not-survived

ggplot(train.data, aes(x=Fare)) 
  geom_histogram(binwidth=3, colour='black') +
  facet_wrap(~Survived, ncol = 2, scales = 'fixed') +
  xlim(0,100) # xlim(100,520)


  
table(train.data$Survived[which(train.data$Fare < 50)])
table(train.data$Survived[which(train.data$Fare > 50)])
# we can see that survived have lower fares...



# 6. Embarked
plot_survival_dist_by_feature(train.data, 'Embarked')
# Is this info important? 
# Lets leave it for now.. and let the algorithms decide :)


# Could we get any more info from SibSp/Parch features

# 7. SibSp
table(train.data$SibSp)
plot_survival_dist_by_feature(train.data, 'SibSp')

# 8. Parch
table(train.data$Parch)

plot_survival_dist_by_feature(train.data, 'Parch')

# create a new feature Family size from summing #parent/#children, #spouses/siblings + 1 for the observed passenger
train.data$Family.size = as.integer(train.data$Parch + train.data$SibSp + 1)

# remove Parch and SibSp variables (this information is already incorporated in family size)
train.data$Parch = NULL
train.data$SibSp = NULL

# create a new boolean feature if traveling alone
train.data$Travel.alone = ifelse(train.data$Family.size == 1, TRUE, FALSE)

plot_survival_dist_by_feature(train.data, 'Travel.alone')
# we can se that between ppl traveling alone more people died

##################################################################################################################
# train data is now ready for the last transformation, i.e. age imputation
save(train.data, file = 'Objects/train_data_missing_age.RData')


