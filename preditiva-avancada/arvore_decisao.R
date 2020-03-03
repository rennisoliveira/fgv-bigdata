## LIMPA VARIAVEIS
rm(list=ls()) 
# Set Diretorio
setwd("/home/rennis/rennisHD00/Projetos/Pessoal/R/rennis-fgv-bigdata/preditiva-avancada")
# Arvore de Decisao
# Passo 1)
# Primeiramente vamos importar os seguinter pacotes:
# caret: Biblioteca para funcionalidade de ML
# mlbench: Biblioteca para Validação de Modelos

#Importando Bibliotecas e Dados
library(caret)
library(mlbench)

# Dataset
df <- read.csv(file="dataset/Admission_Predict.csv", header=TRUE, sep=";", encoding = 'utf-8')
tail(df)
head(df)

df$UNIVERSITY_RATING <- as.factor(df$UNIVERSITY_RATING)
df$S_ID <- NULL
summary(df)

# Passo 4)
# Agora vamos particionar o dataset em treino/teste. Para isso definimos "p=0.7", isto é 70% da base 
# será escolhida aleatóriamente para treino e 30% para teste do modelo.
# Observe também que o "set.seed(x)" garante que ao replicarmos essa partição em outro computador 
# por exemplo, os mesmos dados irão respectivamente prar treino e teste
set.seed(18)
trainIndex <- createDataPartition(df$RANK, p = .7, list = FALSE)

dfTrain <- df[ trainIndex,]
dfTest  <- df[-trainIndex,]

# Árvore Simples
set.seed(10)

# Definindo Parâmetros do Cross Validation
cv <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE, classProbs=TRUE)

# Treinando Modelo de Árvore Simples
model_5.0 <- train(RANK~. , data = dfTrain, method = "C5.0Tree",trControl = cv)
model_5.0

# Montando a Matriz de Confusão
pred_5.0 <- predict(model_5.0 ,newdata=dfTest)
confusionMatrix(data=pred_5.0, dfTest$RANK)

# Fazendo Scoring do Modelo
classProbs_5.0 <- predict(model_5.0, newdata=dfTest, type="prob")
head(classProbs_5.0)

# Verificando Importância das Variáveis Preditoras
imp <- varImp(model_5.0, useModel=FALSE, scale=FALSE)
imp
# plot(imp)
### PENDENTE

set.seed(143)
# Definindo Parâmetros do Cross Validation
cv <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE, classProbs=TRUE)
# Treinando Modelo com Boosting (XgBoost)
model_boosting <- train(RANK~. , data = dfTrain, method = "xgbTree",trControl = cv)
model_boosting
# Gerando Matriz de Confusão
pred_boosting <- predict(model_boosting ,newdata=dfTest)
confusionMatrix(data=pred_boosting, dfTest$RANK)

# Fazendo Scoring do Modelo
classProbs_boosting <- predict(model_boosting, newdata=dfTest, type="prob")
head(classProbs_boosting)

# Importância das Variáveis Preditoras
imp <- varImp(model_5.0, useModel=FALSE, scale=FALSE)
imp
plot(imp)


# Árvore com Bagging
set.seed(186)
# Definindo Parâmetros do Cross Validation
cv <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE, classProbs=TRUE)
# Treindo Modelo com Bagging
model_bagging <- train(RANK~. , data = dfTrain, method = "treebag",trControl = cv)
model_bagging
# Gerando Matriz de Confusão
pred_bagging <- predict(model_bagging ,newdata=dfTest)
confusionMatrix(data=pred_bagging, dfTest$RANK)

# Fazendo Scoring do Modelo
classProbs_bagging <- predict(model_bagging, newdata=dfTest, type="prob")
head(classProbs_bagging)

# Importância das Variáveis Preditoras
imp <- varImp(model_5.0, useModel=FALSE, scale=FALSE)
imp
plot(imp)


# Árvore Random Forest
set.seed(191)
# Definindo Parâmetros do Cross Validation
cv <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE, classProbs=TRUE)
# Treinando Modelo Random Forest
model_rf <- train(RANK~. , data = dfTrain, method = "rf",trControl = cv)
model_rf
# Gerando Matriz de Confusão
pred_rf <- predict(model_rf ,newdata=dfTest)
confusionMatrix(data=pred_rf, dfTest$RANK)
# Fazendo Scoring do Modelo
classProbs_rf <- predict(model_rf, newdata=dfTest, type="prob")
head(classProbs_rf)
# Importância das Variáveis Preditoras
imp <- varImp(model_5.0, useModel=FALSE, scale=FALSE)
imp
plot(imp)