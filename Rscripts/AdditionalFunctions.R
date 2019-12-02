# additional function for loading RData with desired variable name
loadRData = function(fileName){
  load(fileName)
  get(ls()[ls() != 'fileName'])
}


# additional function for calculating FPR and TPR to plot ROC
get_ROC_and_AUC = function(model, data){
  pred = predict(model, data, type = 'prob')[,2] # predict
  
  # classifier evaluation using ROCR starts with creating a prediction object.
  # This function is used to transform the input data into a standardized format.
  roc = prediction(pred, data$Survived)
  
  # measure the quality of a prediction with tpr and fpr
  perform = performance(roc, measure = 'tpr', x.measure = 'fpr')
  
  # reshape data for plotting with ggplot
  perform_df = as_tibble(data.frame(x = perform@x.values[[1]], y = perform@y.values[[1]]))
  
  # calculate AUC
  auc = performance(roc, measure = 'auc')
  auc = auc@y.values[[1]]
  
  # return AUC and data for plotting ROC curve
  return(list(AUC = auc, ROC = perform_df))
}



