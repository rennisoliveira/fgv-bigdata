### 1 Carregar mapa de Recife
setwd("/home/rennis/rennisHD00/Projetos/Pessoal/R/rennis-fgv-bigdata/estatistica-espacial")
library(rgdal)
library(readr)
library(spdep)
library(tidyverse)

## Lendo o shapefile com os bairros da cidade de Recife
recife = readOGR("map/Recife/Recife.shp")

## Plotando o shapefile com todos os bairros da cidade de Recife
plot(recife, axes = TRUE)

#Transformando para latitude e longitude
# recife = spTransform(x = recife, CRSobj = CRS("+proj=longlat +datum=WGS84"))
# plot(recife, axes = TRUE)


## spTransform - transformacao de dados e projecao de mapas
#Argumentos:
#x - shape a ser transformado
#CRSobj - objeto da classe de sistema de referencia de coordenadas

#Lendo o arquivo de dados
#Registro das vendas vendas do produto de interesse
base = read_csv("dataset/Vendas_agregada_bairro.csv")

#Visualinado a base de dados
base

#-------------------------------------------------------------------------------------#
#Associando os bairros de Recife ao numero de produtos vendido
#As informacoes sao relacionadas a partir do nome do Bairro (coluna EBAIRRNOME no objeto 
#recife e coluna no_bairro_residencia no objeto base)
#-------------------------------------------------------------------------------------#

#Transformando a variavel no_bairro_residencia em fator para ficar compat√É≠vel com recife@data
base$no_bairro_residencia = factor(base$no_bairro_residencia)

#Fazendo a relacao dos dois data frames
recife@data = left_join(x = recife@data, y = base, by = c("EBAIRRNOME" = "no_bairro_residencia"))
coordenadas = coordinates(obj = recife)
head(coordenadas)
plot(recife)

#W com o criterio queen
W.rook = poly2nb(pl = recife, row.names = recife$EBAIRRNOME, queen = FALSE)
W.rook
summary(W.rook)
plot(recife, border = "grey")
plot(W.rook, coordenadas, add = TRUE, col = "cyan")



## Lista de vizinhanca espacial com pesos
# Rook
recWQW <- nb2listw(neighbours = W.rook, style="W") #outras opcoes: B, C, S e U
recWQW$weights

# Binario
recWQB <- nb2listw(neighbours = W.rook, style="B") #outras opcoes: B, C, S e U
recWQB$weights

moran.test(x = recife$num.vendas,listw = recWQW)

moran.test(x = recife$num.vendas,listw = recWQB)

moranlocREC = localmoran(x = recife$num.vendas,listw = recWQW, na.action=na.exclude,zero.policy=TRUE);moranlocREC

head(moranlocREC)
