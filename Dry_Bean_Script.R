rm(list=ls())

# Leitura do arquivo de dados
csv_data = read.csv("Dry_Bean_Dataset.csv",header=T, sep=";", na.strings="?")
summary(csv_data)

# Remoção das linhas com NA ou NaN
csv_data = na.omit(csv_data)
str(csv_data)