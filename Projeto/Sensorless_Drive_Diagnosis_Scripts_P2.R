rm(list=ls())

library(devtools)
install_github("vqv/ggbiplot")
library(ggplot2)
library(gridExtra)
library(GGally)
library(grid)
library(stringr)
library(corrplot)
library(caret)
library(mclust)
library(ggfortify)
library(ggbiplot)

# Número de partições que serão criadas
K = 5 

set.seed(1)

sensor_data = read.csv("Sensorless_drive_diagnosis.txt", sep=" ", na.strings="?", dec=".")

str(sensor_data)

sensor_data = na.omit(sensor_data)

features_names = vector("list", length = length(names(sensor_data)))
for(i in 1:length(names(sensor_data))-1) {
  features_names[i] = str_interp("Feature${i}")
}

features_names[49] = "label"
names(sensor_data) = features_names

partitions = createFolds(sensor_data$label, k=K)

accuracies = matrix(nrow = 2, ncol = K)

for(partition in 1:K) {
    
  test_indexes = partitions[[partition]]  
  test_data    = sensor_data[test_indexes, -49]
  
  training_indexes = unlist(partitions[-partition])
  training_data    = sensor_data[training_indexes -49]
}
