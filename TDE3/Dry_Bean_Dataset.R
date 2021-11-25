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

for(partition in 1:3) {
  
  training_indexes = unlist(partitions[-partition])
  test_indexes     = partitions[[partition]]
  
  training_data    = beans_data[training_indexes,-17]
  training_labels  = beans_data[training_indexes,17]

  test_data        = beans_data[test_indexes,-17]
  test_labels      = beans_data[test_indexes,17]
}