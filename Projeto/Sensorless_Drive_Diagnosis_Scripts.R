rm(list=ls())

library('ggplot2')
library('gridExtra')
library('GGally')
library(grid)
library(stringr)
library(corrplot)
library(caret)
library(ggfortify)
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
sensor_data = read.csv("Sensorless_drive_diagnosis.txt", sep=" ", na.strings="?", dec=".")

summary(sensor_data)

features_names = vector("list", length = length(names(sensor_data)))
for(i in 1:length(names(sensor_data))-1) {
  features_names[i] = str_interp("Feature${i}")
}

features_names[49] = "label"

names(sensor_data) = features_names

labels_factor = factor(sensor_data$label)
sensor_data$label = labels_factor

summary(labels_factor)

classes = unique(sensor_data$label)[1:10]
num_classes = length(classes)

colors=c("red", "green", "blue")

index = 0

for(j in 1:10) {
  par(mfrow=c(1,3), cex=1)
  for(i in (1+index):(3+index)){
    plot = boxplot(sensor_data[,i] ~ sensor_data$label, 
                   col=colors, xlab = "", 
                   ylab="", 
                   main = names(sensor_data)[i], 
                   xaxt = "n",  
                   yaxt = "n")
    axis(side = 1, labels = FALSE)
    axis(side = 2, las = 2, cex.axis = 0.9, font =20)
    text(x = 1:num_classes,
         labels = classes,
         par("usr")[3],
         xpd = T,
         srt = 90,
         adj = 1.2,
         cex = 0.9)
  } 
  index = 3 + index
}

# Analizando as primeiras três dimensões do conjunto de dado, percebe-se uma grande quantidade
# de outliers, assim, será realizada a remoção deles. 

# Removendo os outliers do conjunto de dados
outliers = c()
for (i in 1:length(sensor_data)) {
  outliers = c(outliers, which(sensor_data[,i] %in% boxplot.stats(sensor_data[,i])$out))
}

sensor_data = sensor_data[-unique(outliers),]
# Após a remoção dos outliers percebe-se a redução no número de observações no conjunto de dados

sensor_data_labels = sensor_data[,48]
sensor_data = sensor_data[,-48]


normalize_params = preProcess(sensor_data, method=c("center","scale"))
sensor_data = predict(normalize_params, sensor_data)

# Após a normalização, temos que os dados estão livrados de anomalias nos seus valores. 
# Assim, os dados poderão ser melhor visualizados.

correlation = cor(sensor_data[1:47])
cor_indexes = findCorrelation(correlation, verbose = T, cutoff=.95) #indices de correlação forte

corrplot(correlation, tl.cex=0.5, type="upper")

if(length(cor_indexes) > 0) {
  sensor_data[,cor_indexes] = NULL 
}


index = 0
for(j in 1:9) {
  par(mfrow=c(1,3), cex=0.5)
  for(i in (1+index):(3+index)){
    plot = boxplot(sensor_data[,i] ~ sensor_data$label, 
                   col=colors, xlab = "", 
                   ylab="", 
                   main = names(sensor_data)[i], 
                   xaxt = "n",  
                   yaxt = "n")
    axis(side = 1, labels = FALSE)
    axis(side = 2, las = 2, cex.axis = 0.9, font =20)
    text(x = 1:num_classes,
         labels = classes,
         par("usr")[3],
         xpd = T,
         srt = 90,
         adj = 3)
  } 
  index = 3 + index
}

plot = boxplot(sensor_data[,8] ~ sensor_data$label, 
               col=colors, xlab = "", 
               ylab="", 
               main = names(sensor_data)[8], 
               xaxt = "n",  
               yaxt = "n")
axis(side = 1, labels = FALSE)
axis(side = 2, las = 2, cex.axis = 0.9, font =20)
text(x = 1:num_classes,
     labels = classes,
     par("usr")[3],
     xpd = T,
     srt = 90,
     adj = 3)

pca_params = preProcess(sensor_data, method=c("center", "scale", "pca"), thresh = .98)

projected_data = predict(pca_params, sensor_data)
summary(projected_data)

plot = boxplot(projected_data[,2] ~projected_data$label, 
               col=colors, xlab = "", 
               main = names(projected_data)[i], 
               ylab="",
               xaxt = "n",  
               yaxt = "n")
axis(side = 1, labels = FALSE)
axis(side = 2, las = 2, cex.axis = 0.9, font =20)
text(x = 1:num_classes,
     labels = classes,
     par("usr")[3],
     xpd = T,
     srt = 90,
     adj = 3)

plot = boxplot(projected_data[,3] ~projected_data$label, 
               col=colors, xlab = "", 
               ylab="",
               xaxt = "n",  
               yaxt = "n")
axis(side = 1, labels = FALSE)
axis(side = 2, las = 2, cex.axis = 0.9, font =20)
text(x = 1:num_classes,
     labels = classes,
     par("usr")[3],
     xpd = T,
     srt = 90,
     adj = 3)

plot = boxplot(projected_data[,4] ~projected_data$label, 
               col=colors, xlab = "", 
               ylab="",
               xaxt = "n",  
               yaxt = "n")
axis(side = 1, labels = FALSE)
axis(side = 2, las = 2, cex.axis = 0.9, font =20)
text(x = 1:num_classes,
     labels = classes,
     par("usr")[3],
     xpd = T,
     srt = 90,
     adj = 3)

plot = boxplot(projected_data[,5] ~projected_data$label, 
               col=colors, xlab = "", 
               ylab="",
               xaxt = "n",  
               yaxt = "n")
axis(side = 1, labels = FALSE)
axis(side = 2, las = 2, cex.axis = 0.9, font =20)
text(x = 1:num_classes,
     labels = classes,
     par("usr")[3],
     xpd = T,
     srt = 90,
     adj = 3)


plot = boxplot(projected_data[,6] ~projected_data$label, 
               col=colors, xlab = "", 
               ylab="",
               xaxt = "n",  
               yaxt = "n")
axis(side = 1, labels = FALSE)
axis(side = 2, las = 2, cex.axis = 0.9, font =20)
text(x = 1:num_classes,
     labels = classes,
     par("usr")[3],
     xpd = T,
     srt = 90,
     adj = 3)

pca = prcomp(sensor_data[,1:length(sensor_data)-1], center=TRUE, scale=TRUE)

g <- ggbiplot(pca,var.scale = 1, alpha = 0.1, varname.abbrev = T)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')

explained_variance = pca$sdev^2 / sum(pca$sdev^2)

# Gŕafico da variancia das PCs
par(mar=c(5, 3, 3, 1))
barplot (
  explained_variance, 
  names.arg=1:28, 
  main = "Variância",
  xlab = "Componentes Principais",
  ylab = "Variância Explicada",
  col ="steelblue"
)
