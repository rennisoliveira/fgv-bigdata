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

# 1) Treine 3 árvores de decisão (Boosting, Bagging e Random Forest) e calcule suas 
# respectivas matrizes de confusão
# Obs) Utilize seed(9283) para partição dos dados e treino dos modelos
df <- read.csv(file="dataset/BostonHospital_Exercise.csv", header=TRUE, sep=",", encoding = 'utf-8')

set.seed(9283)
trainIndex <- createDataPartition(df$Categoria, p = .7, list = FALSE)

dfTrain <- df[ trainIndex,]
dfTest  <- df[-trainIndex,]



set.seed(9283)
cv <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE, classProbs=TRUE)
model_bagging <- train(Categoria~. , data = dfTrain, method = "treebag",trControl = cv)
pred_bagging <- predict(model_bagging ,newdata=dfTest)
confusionMatrix(data=pred_bagging, dfTest$Categoria)


set.seed(9283)
model_boosting <- train(Categoria~. , data = dfTrain, method = "xgbTree",trControl = cv)
pred_boosting <- predict(model_boosting ,newdata=dfTest)
confusionMatrix(data=pred_boosting, dfTest$Categoria)

set.seed(9283)
model_rf <- train(Categoria~. , data = dfTrain, method = "rf",trControl = cv)
pred_rf <- predict(model_rf ,newdata=dfTest)
confusionMatrix(data=pred_rf, dfTest$Categoria)

imp_bagging <- varImp(model_bagging, useModel=FALSE, scale=FALSE)
imp_bagging

imp_boosting <- varImp(model_boosting, useModel=FALSE, scale=FALSE)
imp_boosting

imp_rf <- varImp(model_rf, useModel=FALSE, scale=FALSE)
imp_rf


# 3) Para cada modelo determine o tipo de patologia para o seguinte indivíduo
patient <- data.frame("Q1"="Dor de Cabeça" , "Q2"="Paralisisa" , "Q3"="Problema Auditivo em Parente" , 
                      "Q4"="Mais" , "Q5"="Dor de Garganta" ,
                      "Q6"="Não" , "Q7"="Ganho de Peso" , "Q8"="Pressão Alta", "Categoria"="")
y_Model <- predict(model_bagging, patient)
patient$Model <- y_Model
head(patient)


classProbs_rf <- predict(model_rf, newdata=patient, type="prob")
head(classProbs_rf)

y_Model <- predict(model_boosting, patient)
patient$Model <- y_Model
head(patient)


classProbs_rf <- predict(model_boosting, newdata=patient, type="prob")
head(classProbs_rf)

y_Model <- predict(model_rf, patient)
patient$Model <- y_Model
head(patient)


classProbs_rf <- predict(model_rf, newdata=patient, type="prob")
head(classProbs_rf)