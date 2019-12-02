# import additional functions
source('Rscripts/AdditionalFunctions.R') 

# load test data
load(file = 'Objects/preprocessed_test_data.Rdata')

# load final model
final.model = loadRData('Objects/Models/final_model_rf.Rdata')

# predict on test set
predicted = predict(final.model, test.data)

# change survival values back to 0/1 to compare
predicted = ifelse(predicted == 'yes', 1, 0)

# final form for submission
submission.data = data.frame(PassengerId = test.data$PassengerId, Survived = predicted)

# write to file
write.csv(submission.data, 'Data/submission_file.csv', row.names = FALSE, quote = FALSE)
