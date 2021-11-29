rm(list=ls())

library(caret)
library(mclust)

beans_data = read.csv("Dry_Bean_Dataset.csv",header=T, sep=";", na.strings="?", dec=",")
str(beans_data)

class_factor = factor(beans_data$Class)
beans_data$Class = class_factor

set.seed(1)

partitions = createFolds(beans_data$Class, k = 3)

results = c(0,0,0)

for(partition in 1:3) {
  training_indexes = unlist(partitions[-partition])
  test_indexes     = partitions[[partition]]
  
  training_data    = beans_data[training_indexes,-17]
  training_labels  = beans_data[training_indexes,17]
  
  test_data        = beans_data[test_indexes,-17]
  test_labels      = beans_data[test_indexes,17]
  
  ## Normalização por Score-Z
  
  normalization_parameters = preProcess(training_data, method = c("center","scale"))
  training_data            = predict(normalization_parameters, training_data)
  test_data                = predict(normalization_parameters, test_data)
  rm(normalization_parameters)
  
  gmm.model = MclustDA(training_data, training_labels, modelNames = c('VII'), verbose = FALSE)
  
  gmm.predict = predict(gmm.model, test_data)
  
  confusion_matrix = confusionMatrix(gmm.predict$classification, test_labels)
  
  print(confusion_matrix)
  
  results[partition] = confusion_matrix$overall[1]
}

mean(results)
