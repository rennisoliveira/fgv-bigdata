# Passo 1)
# Primeiramente vamos importar os seguinter pacotes:
# caret: Biblioteca para funcionalidade de ML mlbench: Biblioteca para Validação de Modelos ggplot2: Biblioteca para visualização de dados
library(caret)
library(mlbench)
library(ggplot2)

setwd("/home/rennis/rennisHD00/Projetos/Pessoal/R/rennis-fgv-bigdata/preditiva-avancada")
# Passo 2)
# Agora vamos carregar o dataset, com cabeçalho (header=TRUE) e o separador de colunas sendo ','.
df <- read.csv(file="dataset/FIsh.csv", header=TRUE, sep=",")
# Passo 3)
# Agora vamos fazer uma rápida análise dos dados buscando identificar algumas relações visualmente e também verificar a qualidade dos dados.
# Análise dos Dados
head(df)

df$AGE <- as.numeric(df$AGE)
df$TEMPERATURE <- as.numeric(df$TEMPERATURE)
df$SIZE <- as.numeric(df$SIZE)

summary(df)

ggplot(data = df, aes(x = SIZE, y = AGE)) +
  geom_point()

# Particionando Dados em Treino e Teste
# Passo 4)
# Agora vamos particionar o dataset em treino/teste. Para isso definimos "p=0.7", isto é 70% da base será escolhida aleatóriamente para treino e 30% para teste do modelo.
# Observe também que o "set.seed(x)" garante que ao replicarmos essa partição em outro computador por exemplo, os mesmos dados irão respectivamente prar treino e teste
set.seed(280)
trainIndex <- createDataPartition(df$SIZE, p = .7, list = FALSE)

dfTrain <- df[ trainIndex,]
dfTest  <- df[-trainIndex,]

