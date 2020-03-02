## LIMPA VARIAVEIS
rm(list=ls()) 

# LIB
library(caret)
library(datasets)
library(mlbench)
library(ggplot2)

# Set Diretorio
setwd("/home/rennis/rennisHD00/Projetos/Pessoal/R/rennis-fgv-bigdata/preditiva-avancada")

#  1) Treine um modelo de regressão logística e calcule sua matriz de confusão
# OBS) Utilize novamente 70% da base para treino com seed(314)
df <- read.csv(file="dataset/BostonCredit_Exercise.csv", header=TRUE, sep=",")
tail(df)

set.seed(314)
# Agora vamos particionar o dataset em treino/teste. Para isso definimos "p=0.7", 
# isto é 70% da base será escolhida aleatóriamente para treino e 30% para teste do modelo.
# Observe também que o "set.seed(x)" garante que ao replicarmos essa partição em outro 
# computador por exemplo, os mesmos dados irão respectivamente prar treino e teste
trainIndex <- createDataPartition(df$APPROVED, p = .7, list = FALSE)
dfTrain <- df[ trainIndex,]
dfTest  <- df[-trainIndex,]

# Observe que em "method" definimos "glm" de generalized linear model e que definimos variável target 
# sendo APPROVED e colocamos todas as outras variáveis como preditoras. Observe também que agora incluímos 
# o parâmetro de 10 fold cross validation na hora do treino do modelo
set.seed(314)
options(warn=-1)
cv <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE,
                   summaryFunction=twoClassSummary, classProbs = TRUE)
# Y = APROVED e ~. esta todas as outras variaveis
model <- train(APPROVED~., data = dfTrain, method = "glm", 
               metric="ROC",trControl = cv, control = list(maxit = 50))

# Chamando o modelo obtemos informações como:
# Número de amostras no treino
# Número de features preditoras
# Sensitividade e Specificidade do Modelo
model     

dfPred <- predict(model, newdata=dfTest)
confusionMatrix(data=dfPred, dfTest$APPROVED)

# 2) Quais as 3 features mais importantes do modelo ?
imp <- varImp(model, useModel=FALSE, scale=FALSE)
imp
plot(imp)

# 3) Determine a aprovação ou não de crédito para o seguinte indivíduo
candidato <- data.frame("GENDER" ="0" , "AGE" =125 , "DEBT" =13 , "MARRIED"=1 , "BANK_CUSTOMER" = 0 ,
                        "EDUCATION_LEVEL"='w' , "YEARS_EMPLOYED"=0 ,  "PRIOR_DEFAULT"=1, 
                        "EMPLOYED"=1, "CITIZEN"="0", "INCOME"=1 ,"APPROVED"= '')

y_Model <- predict(model, candidato)
candidato$APPROVED <- y_Model
head(candidato)

# Questão 4) Descreve o que acontece com a matriz de confusão do modelo ao adicionarmos o 
# quadrado da feature "AGE" como nova feature
# Obs) Utilize o mesmo seed da questão 1.
set.seed(314)
options(warn=-1)

dfTrain$INCOME_2 = dfTrain$AGE**2
dfTest$INCOME_2 = dfTest$AGE**2

cv <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE,
                   summaryFunction=twoClassSummary, classProbs = TRUE)

model <- train(APPROVED~., data = dfTrain, method = "glm", 
               metric="ROC",trControl = cv, control = list(maxit = 50))

model

dfPred <- predict(model, newdata=dfTest)
confusionMatrix(data=dfPred, dfTest$APPROVED)

resample_stats <- thresholder(model, 
                              threshold = seq(.5, 1, by = 0.05), 
                              final = TRUE)
dfPred <- predict(resample_stats, newdata=dfTest)
confusionMatrix(data=dfPred, dfTest$APPROVED)
# Desafio) Retreine novamente os modelos utilizando como seeds 314 e 300. Agora compare o
# custo benefício dos pontos de corte 0.3 , 0.5 e 0.7 com os seguintes custos:
set.seed(314)
cv <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE,
                   summaryFunction=twoClassSummary, classProbs = TRUE)

model_314 <- train(APPROVED~., data = dfTrain, method = "glm", 
                   metric="ROC",trControl = cv, control = list(maxit = 50))

dfProbs <- predict(model_314, newdata=dfTest, type="prob")
# cuts <- c(0.3, 0.5, 0.7)
cuts <- c(0.9)
for (i in cuts) {
  dfPred <- ifelse(dfProbs$Yes >= i, 'Yes', 'No')
  dfPred <- as.factor(dfPred)
  print(paste("Nível de Corte: ", i))
  print(confusionMatrix(data=dfPred, dfTest$APPROVED)) 
}

set.seed(300)

cv <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE,
                   summaryFunction=twoClassSummary, classProbs = TRUE)


model_300 <- train(APPROVED~., data = dfTrain, method = "glm", 
                   metric="ROC",trControl = cv, control = list(maxit = 50))


dfProbs <- predict(model_314, newdata=dfTest, type="prob")


cuts <- c(0.3, 0.5, 0.7)
for (i in cuts) {
  dfPred <- ifelse(dfProbs$Yes >= i, 'Yes', 'No')
  dfPred <- as.factor(dfPred)
  print(paste("Nível de Corte: ", i))
  print(confusionMatrix(data=dfPred, dfTest$APPROVED)) 
}