# Titanic Kaggle Challenge

This repo includes my own solution for kaggle titanic challenge (https://www.kaggle.com/c/titanic/overview) using R. The project is structured as follows.


**Data**: training data, testing data and submission file with final predictions

**Objects**: RData objects made while preprocessing and training (all the final models are in the Models folder)

**Report**: R markdown file (Rmd) used for creating a report and the actual report as html file

**Rscripts**: all the R code used in the project
    
   1. *Preprocessing.R*  - all the preprocessing of the training data (without age imputation)
    
   2. *ImputeAge.R* - creating age imputation model based on training data
 
   3. *PreprocessingTestData.R* - all the preprocessing of the testing data
 
   4. *Train.R* - all the fitting
 
   5. *Predict.R* - final prediction on the testing data
 
   6. *AdditionalFunctions.R* - file with some additional functions used across multiple scripts
    




