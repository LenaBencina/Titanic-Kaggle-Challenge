# Titanic Kaggle Challenge

This repo is includes my own solution for kaggle titanic challenge (https://www.kaggle.com/c/titanic/overview) using R. The project is structured as follows.


Data: training data, testing data and submission file with final predictions

Objects: objects made while preprocessing and training (all the final models are in the Models folder)

Report: R markdown file (Rmd) used for creating a report and the actual report as html file

Rscripts: all the R code used in the project
    Preprocessing.R - all the preprocessing of the training data (without age imputation)
    ImputeAge.R - creating age imputation model based on training data
    PreprocessingTestData.R - all the preprocessing of the testing data
    Train.R - all the fitting
    Predict.R - final prediction on the testing data
    AdditionalFunctions.R - file with some additional functions used across multiple scripts
    




