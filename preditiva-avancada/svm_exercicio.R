## LIMPA VARIAVEIS
rm(list=ls()) 
# Set Diretorio
setwd("/home/rennis/rennisHD00/Projetos/Pessoal/R/rennis-fgv-bigdata/preditiva-avancada")
# Lib
library(caret)
library(mlbench)
# Carga dataset
df <- read.csv(file="dataset/SVM_Exercise.csv", header=TRUE, sep=",")

options(warn=-1)
set.seed(1422)

trainIndex <- createDataPartition(df$RATING, p = .7, list = FALSE)

dfTrain <- df[ trainIndex,]
dfTest  <- df[-trainIndex,]

options(warn=-1)
cv <- trainControl(method = "repeatedcv", number = 5, savePredictions = TRUE)
model_linear <- train(RATING~., data = dfTrain, method = "svmLinear", trControl = cv, 
                      preProcess = c("center", "scale"))
model_linear

dfPred <- predict(model_linear, newdata=dfTest)
confusionMatrix(data=dfPred, dfTest$RATING)

dfPred <- predict(model_linear, newdata=dfTest)
confusionMatrix(data=dfPred, dfTest$RATING)

imp <- varImp(model_linear, useModel=FALSE, scale=FALSE)
imp
plot(imp)

apple <- data.frame("P/E"= 15.45, "P/B"= 7.53, "EPS"=12.15 , 
                    "P/S"=3.47 , "US"=1 , "PEG"=1.74 , "Rating"= '?')
y_Model <- predict(model_linear, apple)
apple$Model <- y_Model
head(apple)

options(warn=-1)
cv <- trainControl(method = "repeatedcv", number = 10)
model_rbf <- train(RATING~., data = dfTrain, method = "svmRadial", trControl = cv, 
                   preProcess = c("center", "scale"))
model_rbf

dfPred <- predict(model_rbf, newdata=dfTest)
confusionMatrix(data=dfPred, dfTest$RATING)
