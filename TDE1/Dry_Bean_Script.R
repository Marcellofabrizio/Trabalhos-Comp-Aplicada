rm(list=ls())

# Leitura do arquivo de dados
csv_data = read.csv("Dry_Bean_Dataset.csv",header=T, sep=";", na.strings="?")
summary(csv_data)

"
1-
Carregue a base de dados e mostre a estrutura do dataset (str()). 
Comente sobre o número de amostras, de variáveis (e seu tipo).
O arquivo do dataset não pode ser modificado de forma alguma. 
A leitura deverá ser realizada de tal maneira qualquer característica dos dados. 
"
str(csv_data)
# 13611 observações com 17 variáveis cada

csv_data$Class = factor(csv_data$Class)

"
2-
Altere a variável do tipo do feijão (Class) para um factor. 
Utilizando um comando mostre como estimar o número de classes existentes. 
"
summary(csv_data$Class)
length(unique(csv_data$Class))
#7 classes de feijões

"
3-
Quantas amostras existem por classe? Use um gráfico de barras para ilustrar as quantidades.
"