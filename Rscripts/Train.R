# libraries
library(caret)
library(ROCR)
library(tibble)
library(rattle)
library(reshape2)

# import additional functions
source('Rscripts/AdditionalFunctions.R')

# set seed for reproducible results
set.seed(1337)

# load preprocessed train data
train.data = loadRData(file = 'Objects/preprocessed_train_data.RData')


### Lets train some models now... 

# But first we will define a validation method so we can have some consistency throughout all the algorithms
# we will use 10-fold cross validation with 3 repeats, using prediction of class probabilities
train.control = trainControl(method='repeatedcv', 
                             number=10, 
                             repeats=3,
                             classProbs = TRUE)



# changing levels because the level names starting with numbers are causing error with classProbs parameter in train control
levels(train.data$Survived) = list(no = '0', yes = '1')


# LOGISTIC REGRESSION #############################################################################################
# model.lr.1 = train(Survived ~ Pclass + Name.title + Sex + Age + Fare + Embarked + Family.size + Travel.alone, 
#                    data = train.data,
#                    method = 'glm',
#                    family = 'binomial',
#                    trControl = train.control)
# save(model.lr.1, file = 'Objects/Models/model_lr_1.RData')
load(file = 'Objects/Models/model_lr_1.RData')
summary(model.lr.1)
# some of the variables are not stat. significant
# maybe there is some correlation between sex and Name.title (Mr/Mrs ...)

# model.lr.2 = train(Survived ~ Pclass + Sex + Age + Fare + Embarked + Family.size + Travel.alone, 
#                    data = train.data,
#                    method = 'glm',
#                    family = 'binomial', 
#                    trControl = train.control)
# 
# save(model.lr.2, file = 'Objects/Models/model_lr_2.RData')
load(file = 'Objects/Models/model_lr_2.RData')
summary(model.lr.2)
# remove Embarked and Fare as well

# model.lr.3 = train(Survived ~ Pclass + Sex + Age + Family.size + Travel.alone, 
#                    data = train.data,
#                    method = 'glm',
#                    family = 'binomial', 
#                    trControl = train.control)
# 
# save(model.lr.3, file = 'Objects/Models/model_lr_3.RData')
load(file = 'Objects/Models/model_lr_3.RData')
summary(model.lr.3)
# all the variables are now stat. significant - final model lr3

# calculate ROC and AUC for the best log regression model
tmp = get_ROC_and_AUC(model.lr.3, train.data)
model.lr.auc = tmp$AUC
model.lr.roc = tmp$ROC

# DECISION TREES #############################################################################################

# random cp tunning
# model.dt.1 = train(Survived ~ Pclass + Name.title + Sex + Age + Fare + Embarked + Family.size + Travel.alone, 
#                    data=train.data, 
#                    method='rpart', # decision trees
#                    trControl = train.control,
#                    tuneLength = 20)
# 
# save(model.dt.1, file = 'Objects/Models/model_dt_1.RData')
load(file = 'Objects/Models/model_dt_1.RData')
model.dt.1

# rpart PARAMETERS
# CP: The complexity parameter (cp) in rpart is the minimum improvement in the model needed at each node. It’s based on the cost complexity of the model.
# maxdepth: This parameter is used to set the maximum depth of a tree. Depth is the length of the longest path from a Root node to a Leaf node. Setting this parameter will stop growing the tree when the depth is equal the value set for maxdepth.
# minsplit: It is the minimum number of samples that must exist in a node for a split to happen or be attempted. For example, we set minimum samples in a split to be 5; then, a node can be further split for achieving purity when the number of samples in each split node is more than 5
# minbucket: It is the minimum number of samples that can be present in a Terminal node. For example, we set the minimum samples in a node to 5, meaning that every Terminal/Leaf node should have at least five samples We should also take care of not overfitting the model by specifying this parameter. If it is set to a too-small value, like 1, we may run the risk of overfitting our model.


# tune max depth (leave others to default)
# model.dt.2 = train(Survived ~ ., 
#                    data=train.data, 
#                    method='rpart2', # decision trees (use rpart2 to tune max depth as well)
#                    trControl = train.control, # 10 fold cross validation to avoid overfitting
#                    tuneGrid = expand.grid(maxdepth=(1:10)))
# 
# save(model.dt.2, file = 'Objects/Models/model_dt_2.RData')
load(file = 'Objects/Models/model_dt_2.RData')
model.dt.2

# plot
# fancyRpartPlot(model.dt.2$finalModel)

# calculate ROC and AUC for the best decision tree model
tmp = get_ROC_and_AUC(model.dt.2, train.data)
model.dt.auc = tmp$AUC
model.dt.roc = tmp$ROC

# https://stats.stackexchange.com/questions/105760/how-we-can-draw-an-roc-curve-for-decision-trees

# RANDOM FOREST #############################################################################################
# We will focuse on these two parameters:
# ntree = Number of trees to grow
# mtry = Number of variables available for splitting at each tree node


# lets tune mtry parameter with values from 1 to 15
# model.rf.1 = train(Survived ~ Pclass + Name.title + Sex + Age + Fare + Embarked + Family.size + Travel.alone,
#                    data = train.data,
#                    method = 'rf',
#                    tuneGrid  = expand.grid(.mtry = (1:15)),
#                    trControl = train.control)
# 
# save(model.rf.1, file = 'Objects/Models/model_rf_1.RData')
load(file = 'Objects/Models/model_rf_1.RData')
model.rf.1

# lets use the best mtry value from model 1 and tune the ntree parameter
# rf.modellist = list()
# for (ntree in c(200, 300, 400, 500, 600, 700)){
#   set.seed(ntree)
#   fit = train(Survived~.,
#               data = train.data,
#               method = 'rf',
#               tuneGrid = expand.grid(.mtry = model.rf.1$bestTune),
#               trControl = train.control,
#               ntree = ntree)
#   rf.modellist[[paste('rf.', toString(ntree), sep = '')]] = fit
# }
# 
# save(rf.modellist, file = 'Objects/Models/rf_model_list.RData')
load(file = 'Objects/Models/rf_model_list.RData')
rf.results = resamples(rf.modellist)
summary(rf.results, metric = 'Accuracy')

# model 1 is still better..

# calculate ROC as well for the best random forest model
tmp = get_ROC_and_AUC(model.rf.1, train.data)
model.rf.auc = tmp$AUC
model.rf.roc = tmp$ROC


# KNN #######################################################################################################

# the only parameter in knn is k (= number of nearest neighbours)
# model.knn = train(Survived ~ Pclass + Name.title + Sex + Age + Fare + Embarked + Family.size + Travel.alone,
#                     data = train.data,
#                     method = 'knn',
#                     trControl = train.control,
#                     tuneGrid = expand.grid(k = 1:15))
# 
# save(model.knn, file = 'Objects/Models/model_knn.RData')
load(file = 'Objects/Models/model_knn.RData')
model.knn

# knn is not performing so good... dont proceed with this algorithm

# calculate ROC as well for knn model
tmp = get_ROC_and_AUC(model.knn, train.data)
model.knn.auc = tmp$AUC
model.knn.roc = tmp$ROC

# SVM #######################################################################################################
# The tuneLength parameter is set to pick 9 arbitrary values for the C, 
# the 'cost' of the radial kernel. This parameter controls the complexity
# of the boundary between support vectors. The radial kernel also requires
# setting a smoothing parameter, sigma. 

# let tune C parameter first
# model.svm.1 = train(Survived ~ Pclass + Name.title + Sex + Age + Fare + Embarked + Family.size + Travel.alone,
#                     data = train.data,
#                     method = 'svmRadial', # Radial kernel
#                     tuneLength = 10, # 10 values of the cost function
#                     trControl = train.control)
# 
# 
# save(model.svm.1, file = 'Objects/Models/model_svm_1.RData')
load(file = 'Objects/Models/model_svm_1.RData')
model.svm.1

# Lets try to train to tune both parameters: sigma and C
# model.svm.2 = train(Survived ~ Pclass + Name.title + Sex + Age + Fare + Embarked + Family.size + Travel.alone,
#                     data = train.data,
#                     method = 'svmRadial',
#                     tuneGrid = expand.grid(sigma = c(.01, .015, 0.2), C = c(0.75, 0.9, 1, 1.1, 1.25)),
#                     trControl = train.control)
# 
# 
# save(model.svm.2, file = 'Objects/Models/model_svm_2.RData')
load(file = 'Objects/Models/model_svm_2.RData')
model.svm.2

# we get the highest accuracy with sigma = 0.2 and C = 1 (in model.svm.2)

# calculate ROC as well for best svm model
tmp = get_ROC_and_AUC(model.svm.2, train.data)
model.svm.auc = tmp$AUC
model.svm.roc = tmp$ROC


# COMPARING ALL THE MODELS #################################################################################

# 1. compare accuracy between models
results = resamples(list(log.reg. = model.lr.3, 
                         dec.trees = model.dt.2,
                         random.forest = model.rf.1,
                         knn = model.knn,
                         svm = model.svm.2))




# summarize the distributions
summary(results, metric = 'Accuracy')

# boxplots
bwplot(results, metric = 'Accuracy')



# 2. compare ROC and AUC
AUC = data.frame(lr = model.lr.auc, dt = model.dt.auc, rf = model.rf.auc, knn = model.knn.auc, svm = model.svm.auc)
rownames(AUC) = 'AUC'
AUC = rev(sort(AUC))
names(which.max(AUC))

# plot AUC
ggplot(melt(AUC, value.name = 'AUC'), aes(x = variable, y = AUC)) + 
       geom_bar(stat = 'identity', color = 'black') +
       coord_flip()


# add variable to define the model
model.lr.roc$model = 'lr'
model.dt.roc$model = 'dt'
model.rf.roc$model = 'rf'
model.knn.roc$model = 'knn'
model.svm.roc$model = 'svm'

# bind data
roc.plot.data = rbind(model.lr.roc, model.dt.roc, model.rf.roc, model.knn.roc, model.svm.roc)

# plot all the ROC curves on one graph
roc.plot = ggplot(roc.plot.data, aes(x = x, y = y, colour = model)) +
           geom_line() +
           labs(title = 'ROC', x = 'False positive rate', y = 'True positive rate')


# save final model --> RANDOM FOREST
#save(model.rf.1, file = 'Objects/Models/final_model_rf.Rdata')
#save(model.svm.2, file = 'Objects/Models/final_model_svm.Rdata')
