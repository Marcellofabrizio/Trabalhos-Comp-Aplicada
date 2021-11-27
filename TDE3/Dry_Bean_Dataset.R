rm(list=ls())

library(caret)
library(mclust)

beans_data = read.csv("Dry_Bean_Dataset.csv",header=T, sep=";", na.strings="?", dec=",")
str(beans_data)

class_factor = factor(beans_data$Class)
beans_data$Class = class_factor

set.seed(1)

partitions = createFolds(beans_data$Class, k = 3)

acc_matrix = matrix(3,3)

best = -1

for(experiment in 1:6){
  for(partition in 1:3) {
    
    training_indexes = unlist(partitions[-partition])
    test_indexes     = partitions[[partition]]
    
    training_data    = beans_data[training_indexes,-17]
    training_labels  = beans_data[training_indexes,17]
    
    test_data        = beans_data[test_indexes,-17]
    test_labels      = beans_data[test_indexes,17]
    
    ## Normalização por Score-Z
    if(experiment == 1 || experiment == 3 || (experiment > 4 && (best == 1 || best == 3))){
      normalization_parameters = preProcess(training_data, method = c("center","scale"))
      training_data            = predict(normalization_parameters, training_data)
      test_data                = predict(normalization_parameters, test_data)
      rm(normalization_parameters)
    }
    
    ## Seleção de caracteristicas
    if(experiment < 3 || (experiment < 3 && (best < 3))){
      correlation_matrix = cor(training_data)
      strong_correlations = findCorrelation(correlation_matrix, cutoff=0.95)
      
      if(length(strong_correlations) > 0) {
        training_data[,strong_correlations] = NULL
        test_data[,strong_correlations] = NULL
      }
    }
    
    ## Redução de dimensionalidade utilizando PCA - 95% de tresh
    training_pca  = preProcess(training_data, 
                               method=c("center", "scale", "pca"), thresh = 0.95)
    training_data = predict(training_pca, training_data)
    
    test_pca  = preProcess(test_data, 
                           method=c("center", "scale", "pca"), thresh = 0.95)
    test_data = predict(test_pca, test_data)
    
    rm(training_pca, test_pca)
  }
}