#-------------------------------------------------------------------------------------#
# ---------------------------- Modelando Dados de Area  ------------------------------#
#-------------------------------------------------------------------------------------#
setwd("/home/rennis/rennisHD00/Projetos/Pessoal/R/rennis-fgv-bigdata/estatistica-espacial-aplicacoes")
#Carregando os pacotes necessarios
library(georob)
library(tidyverse)
library(plotGoogleMaps)
library(RColorBrewer)

#Lendo o arquivo de dados
#Dados sobre o zinco
base = read_csv2("dataset/base_zinco-v2.csv")

#Visualinado a base de dados
head(base)

#-------------------------------------------------------------------------------------#
# ------------------ Mapas : valor do Zinco nos locais mensurados  -------------------#
#-------------------------------------------------------------------------------------#

#Plotando a variavel de interesse nos pontos observados com uma escala de cores
base %>% ggplot(aes(x = base$lat, y = base$long)) + 
  geom_point(aes(color = ))


#Mapas interativos
library(leaflet)

#Raio do circulo indicando o valor do zinco
leaflet(base) %>%
  addTiles() %>%
  addCircles(lng = ~long,
             lat = ~lat,
             weight = 1,
             radius = ~zinc/10)

#Cor do circulo indicando o valor do zinco
qpal <- colorQuantile("Reds", base$zinc, n = 8)

leaflet(base) %>% 
  addTiles() %>%
  addCircles(lng = ~long, 
             lat = ~lat, 
             weight = 1, 
             radius = 80, 
             color = ~qpal(zinc), 
             fillOpacity = 1) %>%
  addLegend("bottomright", 
            pal = qpal, 
            values = ~zinc, 
            title = "Zinco", 
            opacity = 1)

#Incializando o mapa
mapa = leaflet() %>% 
  setView(lng=5.743, lat=50.97, zoom=13 ) %>%
  
  # Add two tiles
  addProviderTiles("Esri.WorldImagery", group="Visualizacao 1") %>%
  addTiles(options = providerTileOptions(noWrap = TRUE), group="Visualizacao 2") %>%
  
  # Add 2 marker groups
  addCircleMarkers(data=base %>% filter(ffreq == 1), lng=~long , lat=~lat, radius=5 , color="black",  fillColor="red", stroke = TRUE, fillOpacity = 0.8, group="1") %>%
  addCircleMarkers(data=base %>% filter(ffreq == 2), lng=~long , lat=~lat, radius=5 , color="black",  fillColor="blue", stroke = TRUE, fillOpacity = 0.8, group="2") %>%
  addCircleMarkers(data=base %>% filter(ffreq == 3), lng=~long , lat=~lat, radius=5 , color="black",  fillColor="yellow", stroke = TRUE, fillOpacity = 0.8, group="3") %>%
  
  # Add the control widget
  addLayersControl(overlayGroups = c("1","2","3") , baseGroups = c("Visualizacao 1","Visualizacao 2"), options = layersControlOptions(collapsed = FALSE))


mapa

#Como verificar relacao entre os valores do zinco com a elevacao?

#--------------------------------------------------------------------------------------------------------#
#- Analise exploratoria : verificando as relacoes entre a variavel resposta e as variaveis explicativas -#
#--------------------------------------------------------------------------------------------------------#

#Grafico de dispersao do zinco e da distancia indicando as categorias de ffreq
base %>% 
  ggplot(aes(x = dist, y = zinc, color = as.factor(ffreq))) + 
  geom_point()

#Grafico de dispersao do zinco e da distancia indicando as categorias de soil
base %>% 
  ggplot(aes(x = dist, y = zinc, color = as.factor(soil))) + 
  geom_point()

#Grafico de dispersao do log do zinco e da distancia indicando as categorias de soil para cada tipo de ffrq
base %>% 
  ggplot(aes(x = dist, y = log(zinc), color = as.factor(soil))) + 
  geom_point() + facet_grid(ffreq ~.) 

#Grafico de dispersao do log do zinco e da raiz quadrada da distancia indicando as categorias de soil para cada tipo de ffrq
base %>% 
  ggplot(aes(x = sqrt(dist), y = log(zinc), color = as.factor(soil))) + 
  geom_point() + 
  facet_grid(ffreq ~.)


#-------------------------------------------------------------------------------------#
# ------------------ Ajuste do Modelo de regressao linear multiplo -------------------#
#-------------------------------------------------------------------------------------#


#Ajustando o modelo de regressao linear 
ajuste.ml = lm(formula = log(zinc)~sqrt(dist)+factor(ffreq), data = base)
summary(ajuste.ml)

#Analise de residuos
par(mfrow = c(2,2))
plot(ajuste.ml)
par(mfrow = c(1,1))

#Ajustando o variograma amostral
vario.amostral <- sample.variogram(residuals(ajuste.ml), locations=base[, c("x","y")], lag.dist.def=100, max.lag=2000,
                                   estimator="matheron") 

plot(vario.amostral, type="l", main="variograma amostral do residuo log(zinc)~sqrt(dist)+ffreq")


#Calculando o variograma direcional
plot(sample.variogram(residuals(ajuste.ml), locations=base[, c("x","y")],
                      lag.dist.def=100, max.lag=2000, xy.angle.def=c(0, 22.5, 67.5, 112.5, 157.5, 180),
                      estimator="matheron"), type="l", main="variograma amostral dos residuos log(zinc)~sqrt(dist)+ffreq")


#Ajustando os parametros do variograma teorico
vario.spher <- fit.variogram.model(vario.amostral, variogram.mode="RMspheric", param=c(variance=0.2, nugget=0.05, scale=1000))
summary(vario.spher)

#Plotando o variograma teorico com parametros estimados a partir dos dados
plot(vario.amostral, type="l", main="variograma amostral do residuo log(zinc)~sqrt(dist)+ffreq")
lines(vario.spher, col = "red")

#Ajustando o modelo considerando a dependencia espacial
ajuste.spher.reml <- georob(log(zinc)~sqrt(dist)+ factor(ffreq), 
                            data = base, 
                            locations=~long+lat, 
                            variogram.model = "RMspheric", 
                            param=c(variance=0.1, nugget=0.05, scale=845), tuning.psi=1000)

summary(ajuste.spher.reml)

#Calculando o variograma para o residuo do modelo espacial
plot(sample.variogram(residuals(ajuste.spher.reml), locations=base[, c("x","y")],
                      lag.dist.def=100, max.lag=2000, xy.angle.def=c(0, 22.5, 67.5, 112.5, 157.5, 180),
                      estimator="matheron"), type="l", main="variograma amostral dos residuos log(zinc)~sqrt(dist)+ffreq")


#Obtendo o AIC
extractAIC(ajuste.spher.reml)


