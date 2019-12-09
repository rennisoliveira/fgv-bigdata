# Trabalho - Descrição
if(1=2) {
# Dados – Localizações das ocorrências de crimes na cidade de Houston.
# Gostaria que vocês me entregassem um relatório com a análise de vocês e o script do R 
# (o mais comentado possível, indicando o que estão fazendo com cada comando, com suas 
# próprias palavras). O relatório de vocês deve apresentar discussões 
# e conclusões sobre os problemas envolvidos no trabalho. 
# Foquem nas interpretações no relatório e não nos comandos. 
# Pensem numa demanda de trabalho e que esses resultados servirão como insumos 
# para tomadas de decisões por outros setores da empresa.

# Façam uma análise descritiva/exploratória (apresentação do espaço, do fenômeno de interesse, 
# além de investigar os efeitos de 1a e 2a ordem) das localizações das ocorrências dos crimes. 
# Em seguida, façam análises considerando os diferentes dias da semana (variável day). 
# Ao realizar a análise discutam a estimação da função de intensidade 
# e diferentes formas de verificar a existência do efeito de segunda ordem. 
# Discutam as possíveis diferenças observadas entre os efeitos estimados (1a e 2a ordem) 
# do cenário global e dos 7 cenários avaliados (dias), isto é, o comportamento da intensidade 
# de crimes parece ocorrer de forma similar em todos os dias da semana para a cidade de Houston 
# ou os padrões tendem a ser diferentes?

# Mapa com a localização de Pontos do evento de crimes em Houston
}

# Setando o diretorio de trabalho
setwd("/home/rennis/rennisHD00/Projetos/Pessoal/R/rennis-fgv-bigdata/estatistica-espacial/trabalho")

# Importando os pacotes
if(!require("rgdal")) install.packages("rgdal")
if(!require("maptools")) install.packages("maptools")  
if(!require("dplyr")) install.packages("dplyr")
if(!require("spatstat")) install.packages("spatstat")
if(!require("ggmap")) install.packages("ggmap") 
if(!require("gridExtra")) install.packages("gridExtra")  

## Especifico para maquinas Linux
if(!exists(".sp2owin", mode="function")) source("../../utils/sp2owin.R")

library(rgdal)
library(maptools)
library(dplyr)
library(spatstat)
library(sp)
library(ggmap)

crimes <- read.csv("dataset/Base Houston.csv")
    
# Conhecendo as características dos dados
summary(crimes)
summary(crimes$offense)
summary(crimes$day)
summary(crimes$hour)

#Importando o shapefile de Houston
HoustonShp <- readOGR("map/Houston_City_Limit.shp")

# Observando qual o tipo de coordenadas através da visualização do eixo
plot(HoustonShp, axes=TRUE)

# Preparando o shape para ser utilizado pela biblioteca spatstat
## Houston <- as.owin(HoustonShp) # Windows
Houston <- .sp2owin(HoustonShp)

# Plotando Objeto
plot(Houston, axes = TRUE)

# Criando um padrao de pontos a ser plotado
Houstonppp = ppp(crimes$lon, crimes$lat, window=Houston)
plot(Houstonppp, axes=TRUE)

# outra forma de visualizar os dados no mapa para entender as características do terreno
qmplot(x = lon, y = lat, data = crimes,
       colour = I('red'), size = I(1.5), darken = .3)


# visualizando o mapa com a API do Google Maps
library(ggplot2)
library(gridExtra)

#Definindo a chave da API de acesso ao Google Maps
register_google(key="AIzaSyA6n4J6vbGg1D76ZiNFnhmSeLH8es_p4y8")


#criando objetos para os graficos de Houston com diferentes camadas de layouts
Hter = get_googlemap('Houston',zoom=10,maptype='terrain')
Hsat = get_googlemap('Houston',zoom=10,maptype='satellite')
Hrod = get_googlemap('Houston',zoom=10,maptype='roadmap')
Hhib = get_googlemap('Houston',zoom=10,maptype='hybrid')

#Dividindo a plotagem  dos graficos em uma grade
grid.arrange(ggmap(Hter) + ggtitle("Terreno"), 
             ggmap(Hsat) + ggtitle("Satelite"),
             ggmap(Hrod) + ggtitle("Rodovia"),
             ggmap(Hhib) + ggtitle("Hibrido"), ncol=2)

#Plotando as ocorrencias sobre o mapa do Google Maps com tipo Satelite
ggmap(Hsat) + geom_point(data=crimes, aes(x=lon,y=lat), col="red", size=1.5, alpha=0.5)

# sobrepondo o shape de Houston em cima da vis?o do google maps
ggmap(Hsat) + 
  geom_polygon(aes(x = long, y = lat, group = group), data = HoustonShp,
               colour ='white ', fill =' black ', alpha = .4, size = .3) +
  geom_point(data=crimes, aes(x=lon,y=lat), col="red", size=1.5, alpha=0.5)

# Existem pontos fora do poligono?????




