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

# sensor_data$label = factor(sensor_data$label, levels = 1:11)
sensor_data$label = make.names(factor(sensor_data$label, levels = c('1', '2', '3', '4', '5', '6', '7', '8',
                                                                    '9', '10', '11')))

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
  # outliers = c()
  # for (i in 1:length(training_data)) {
  #   outliers = c(outliers, which(training_data[, i] %in% boxplot.stats(training_data[, i])$out))
  # }
  # 
  # outliers = unique(outliers)
  # training_data   = training_data[-outliers, ]
  # training_labels = training_labels[-outliers]
  
  ## Normalização por Z-Score
  normalization_parameters = preProcess(training_data, method = c("center", "scale"))
  training_data            = predict(normalization_parameters, training_data)
  test_data                = predict(normalization_parameters, test_data)
  # rm(normalization_parameters)
  
  
  ## Seleção de caracteristicas
  correlation_matrix = cor(training_data)
  strong_correlations = findCorrelation(correlation_matrix, cutoff = 0.95)
  
  if (length(strong_correlations) > 0) {
    training_data[, strong_correlations] = NULL
    test_data[, strong_correlations] = NULL
  }
  
  ##########################################
  ##  GMM com 8 gausseanas
  ##########################################
  
  beginning = proc.time()
  gmm.model = MclustDA(training_data, training_labels, 8, modelNames = c('VVV'), verbose = FALSE)
  print("Tempo de treino:")
  
  ## Predição dos dados

  gmm.predict = predict(gmm.model, test_data)

  result = proc.time() - beginning
  
  accuracies[partition, 1] = confusionMatrix(table(gmm.predict$classification, test_labels))$overall[1]   
  
  accuracies[partition, 2] = result['elapsed']
  
  accuracies[partition, 3] = mapply(nMclustParams, mclust.options("emModelNames"),
                                    d = length(training_data), G = 8)['VVV']
  
  ##########################################
  ##  GMM com 12 gausseanas
  ##########################################
  
  beginning = proc.time()
  gmm.model = MclustDA(training_data, training_labels, 12, modelNames = c('VVV'), verbose = FALSE)
  
  ## Predição dos dados
  
  gmm.predict = predict(gmm.model, test_data)
  
  result = proc.time() - beginning
  
  accuracies[partition, 4] = confusionMatrix(table(gmm.predict$classification, test_labels))$overall[1]    
  
  accuracies[partition, 5] = result['elapsed']
  
  accuracies[partition, 6] = mapply(nMclustParams, mclust.options("emModelNames"),
                                    d = length(training_data), G = 12)['VVV']
  
  
  ##########################################
  ##  GMM com EM
  ##########################################
  
  beginning = proc.time()
  gmm.model = MclustDA(training_data, training_labels, modelNames = c('VVV'), verbose = FALSE)

  ## Predição dos dados
  
  gmm.predict = predict(gmm.model, test_data)
  
  result = proc.time() - beginning
  
  accuracies[partition, 7] = confusionMatrix(table(gmm.predict$classification, test_labels))$overall[1]  
  
  accuracies[partition, 8] = result['elapsed']
  
  accuracies[partition, 9] = mapply(nMclustParams, mclust.options("emModelNames"),
                                    d = length(training_data), G = 5)['VVV']
  
  beginning = proc.time()
  bagging.model = train(training_data, training_labels, trControl = trainControl(classProbs = TRUE),
                            method="treebag")
  
  bagging.predict = predict(bagging.model, test_data)
  
  result = proc.time() - beginning

  accuracies[partition, 10] = confusionMatrix(table(bagging.predict, test_labels))$overall[1]
  accuracies[partition, 11] = result['elapsed']
  
  
  write.table(accuracies, 'results.csv' ,sep = ';', dec = ',')
}


