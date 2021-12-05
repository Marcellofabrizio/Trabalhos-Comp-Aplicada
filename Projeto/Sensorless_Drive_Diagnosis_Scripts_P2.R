rm(list = ls())

library(stringr)
library(caret)
library(mclust)
library(class)
# Número de partições que serão criadas
K = 5

set.seed(1)

sensor_data = read.csv(
  "Sensorless_drive_diagnosis.txt",
  sep = " ",
  na.strings = "?",
  dec = "."
)

str(sensor_data)

sensor_data = na.omit(sensor_data)

features_names = vector("list", length = length(names(sensor_data)))
for (i in 1:length(names(sensor_data)) - 1) {
  features_names[i] = str_interp("Feature${i}")
}

features_names[49] = "label"
names(sensor_data) = features_names

sensor_data$label = factor(sensor_data$label, levels = 1:11)

partitions = createFolds(sensor_data$label, k = K)

accuracies = matrix(nrow = K, ncol = 12)

for (partition in 1:K) {
  test_indexes = partitions[[partition]]
  training_indexes = unlist(partitions[-partition])
  
  test_data    = sensor_data[test_indexes,-49]
  test_labels  = sensor_data[test_indexes, 49]
  
  training_data    = sensor_data[training_indexes,-49]
  training_labels  = sensor_data[training_indexes, 49]
  
  ##Remoção dos outliers
  outliers = c()
  for (i in 1:length(training_data)) {
    outliers = c(outliers, which(training_data[, i] %in% boxplot.stats(training_data[, i])$out))
  }
  
  outliers = unique(outliers)
  #raining_data   = training_data[-outliers, ]
  #training_labels = training_labels[-outliers]
  
  ## Normalização por Z-Score
  normalization_parameters = preProcess(training_data, method = c("center", "scale"))
  training_data            = predict(normalization_parameters, training_data)
  test_data                = predict(normalization_parameters, test_data)
  rm(normalization_parameters)
  
  
  ## Seleção de caracteristicas
  correlation_matrix = cor(training_data)
  strong_correlations = findCorrelation(correlation_matrix, cutoff = 0.95)
  
  if (length(strong_correlations) > 0) {
    training_data[, strong_correlations] = NULL
    test_data[, strong_correlations] = NULL
  }
  
  ## Projeção e Aplicação do PCA
  # training_pca  = preProcess(training_data, method = c("center", "scale", "pca"), thresh = .98)
  # training_data = predict(training_pca, training_data)
  # 
  # test_pca  = preProcess(test_data, method = c("center", "scale", "pca"),thresh = .98)
  # test_data = predict(test_pca, test_data)
  
  ## Treinamento de dados utilizando modelo de misturas gaussianas
  
  # methods = c("VII", "VVI")
  # 
  # for (method in 1:4) {
  #   gmm.model = MclustDA(training_data, training_labels, modelNames = c(methods[method]))
  #   
  #   ## Predição dos dados
  #   gmm.predict = predict(gmm.model, test_data)
  #   
  #   confusion_matrix = confusionMatrix(gmm.predict$classification, test_labels)
  #   accuracies[partition, method] = confusion_matrix$overall[1]
  # }
  
  for(k_nn in c(5, 7, 13, 17, 19, 23, 29, 31, 37, 41, 47, 51)){
    pr <- knn(training_data, test_data, cl=training_labels, k=k_nn)
    
    tb <- table(pr,test_labels)
    
    accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
    accuracies[partition, k_nn] = accuracy(tb)
  }
}

