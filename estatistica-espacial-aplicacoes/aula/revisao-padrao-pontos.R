#-------------------------------------------------------------------------------------#
# --------------------------- Revisando Padrao de Pontos  ----------------------------#
#-------------------------------------------------------------------------------------#
#Carregando o pacote ggmap
library(ggmap)

#Caso nao tenha o pacote no seu micro (somente se nao tiver) "descomente" os comandos abaixo e baixe o pacote do github 

#if(!requireNamespace("devtools")) install.packages("devtools")
#devtools::install_github("dkahle/ggmap", ref = "tidyup")


#Carregando pacotes
library(rgdal)
library(maptools)
library(dplyr)
library(spatstat)
library(plotly)
library(tidyverse)
library(sp)
library(RColorBrewer)
library(cartography)
library(tmap)
library(spdep)
library(sf)
library(leaflet)
library(ggrepel)
library(shiny)
library(lattice)
library(gstat)
library(scales)
library(gridExtra)
library(georob)
library(plotGoogleMaps)
library(devtools)

#-------------------------------------------------------------------------------------#
# ------------------------- Analise de padrao de pontos  -----------------------------#
#-------------------------------------------------------------------------------------#
setwd("/home/rennis/rennisHD00/Projetos/Pessoal/R/rennis-fgv-bigdata/estatistica-espacial-aplicacoes")
#Importem o arquivo com as localizações dos de roubo em Sao Paulo
roubosSP = read_csv2("dataset/baseroubos.csv")
  
  #Visualizando os dados de roubo
  roubosSP

#-------------------------------------------------------------------------------------#
# -------------------------------- Analise Grafica  ----------------------------------#
#-------------------------------------------------------------------------------------#

#Importando o shapefile de Sao Paulo
SP = readOGR("map/distritoSP/DEINFO_DISTRITO.shp")
  
  #Plotando o shapefile
  par(mar=c(2.5,2.5,1.5,0.5))
  plot(SP, axes = TRUE)

#Definindo o shapefile como uma janela onde os pontos serao plotados
#owin - observation window
SPO <- as.owin(SP)


#Transformando latlong em coordenadas em UTM
library(PBSmapping)
dad = tibble(Y = roubosSP$LATITUDE,X = roubosSP$LONGITUDE)
attr(dad, "zone") <- 23
attr(dad, "projection") <- "LL"
dados.UTM = convUL(dad, km=FALSE, southern = TRUE)

#Base de dados com coordenadas UTM
dados.UTM = dados.UTM %>% 
  rename(LATITUDE = Y, LONGITUDE = X)


#Criando o Padrao de Pontos no Plano (Planar Point Pattern)
SPppp = ppp( x = dados.UTM$LONGITUDE, y = dados.UTM$LATITUDE, window = SPO)

#Plotando as localizacoes dos delitos
par(mar=c(0.5,0.5,1.5,0.5))
plot(SPppp, pch=21, cex=0.4, bg="blue", main="Roubos em SP")

#Uma outra possibilidade de visualizacao
graf = qmplot(x = LONGITUDE, 
              y = LATITUDE, 
              data = roubosSP,
              colour = I('red'), 
              size = I(1.5), 
              darken = .3)

graf

#Como fazer para obtermos uma visualizacao do padrao de pontos para cada mes
graf + facet_wrap(~MES)

#-------------------------------------------------------------------------------------#
# ------------------------------ Efeito de 1a ordem  ---------------------------------#
#-------------------------------------------------------------------------------------#

#Estimando o efeito de primeira ordem (intensidade) usando diferentes kernels e diferentes raios
SPkde.q = density.ppp(x = SPppp, sigma=2000, kernel="quartic")
SPkde.g = density.ppp(x = SPppp, sigma=20000, kernel="gaussian")
SPkde.e = density.ppp(x = SPppp, sigma=200000, kernel="epanechnikov")

##density.ppp - calcula a funcao de intensidade de acordo com o kernel escolhido
#Argumentos:
#x - objeto da classe ppp
#sigma - é o valor do raio (tau na expressao dos slides)
#kernel - o kernel que deseja-se usar

#Plotando os dados e as funcoes intensidades estimadas pelas diversas funcoes kernel
par(mfrow=c(2,2))
par(mar=c(0.5,0.5,1.5,0.5))
plot(SPppp, pch=21, cex=0.9, bg="blue", main="Roubos", cex.main=0.5)
plot(SPkde.q, main="Kernel Quartico", cex.main=0.5)
plot(SPkde.g, main="Kernel Normal")
plot(SPkde.e, main="Kernel Epanechnikov")
par(mfrow=c(1,1))

#-------------------------------------------------------------------------------------#
# -------------------------------- Salvando graficos  --------------------------------#
#-------------------------------------------------------------------------------------#

pdf('pdf/densidade_roubos.pdf')
plot(SPkde.g, main="Kernel Normal")
dev.off()


graf
ggsave("pdf/roubos.pdf")

#-------------------------------------------------------------------------------------#
# ------------------------------ Efeito de 2a ordem  ---------------------------------#
#-------------------------------------------------------------------------------------#

#Estimando a funcao G
SP.G = Gest(SPppp)

#Gest - estima a funcao de distribuicao G de um padrao de pontos
#Argumento
#X - um objeto da classe ppp

#Plotando a funcao G
par(mar=c(2.5,2.5,1.5,0.5))
plot(SP.G, main="Funcao G")

#Realizando o teste de Clark-Evans para testar agregacao espacial
clarkevans.test(SPppp, alterfffnative = " ")

#Funcoes para estimar os envelopes da funcao G
Genv = envelope(SPppp, fun = Gest, nsim = 20)
plot(Genv)


#-------------------------------------------------------------------------------------#
# --------------------------- Criando mapas interativos  -----------------------------#
#-------------------------------------------------------------------------------------#

#Para facilitar, vamos trabalhar somente com os dados do mes de marco e abril
#Crie uma subbase somente com estes dados
roubosSP

base = select(filter(roubosSP, MES %in% c(3,4)),ANO,MES,LATITUDE,LONGITUDE) 
base

  mapa_roubo_SP <- leaflet(base) %>%
  addTiles() %>%
  addMarkers(lng = ~LONGITUDE,
             lat = ~LATITUDE,
             popup = ~as.character(MES),
             label = ~as.character(MES))

##addTiles - adiciona o layout e camadas ao grafico
##addMarkers - adiciona elementos graficos e camadas ao grafico
#Argumentos:
#lng - coordenada de longitude
#lat - coordenada de latitude
#popup - variavel que voce deseja associar aos pontos para que quando o mouse passe sobre o ponto a mesma seja apresentada

mapa_roubo_SP

#Criando outro icone
greenLeafIcon <- makeIcon(
  iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94,
  shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
  shadowWidth = 50, shadowHeight = 64,
  shadowAnchorX = 4, shadowAnchorY = 62
)

mapa_roubo_SP2 = leaflet(data = base) %>% 
  addTiles() %>%
  addMarkers(~LONGITUDE, ~LATITUDE, icon = greenLeafIcon)

mapa_roubo_SP2

#Criando um icone para cada mes
leafIcons <- icons(
  iconUrl = ifelse(base$MES == 3,
                   "http://leafletjs.com/examples/custom-icons/leaf-green.png",
                   "http://leafletjs.com/examples/custom-icons/leaf-red.png"
  ),
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94,
  shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
  shadowWidth = 50, shadowHeight = 64,
  shadowAnchorX = 4, shadowAnchorY = 62
)

mapa_roubo_SP3 = leaflet(data = base) %>% 
  addTiles() %>%
  addMarkers(~LONGITUDE, ~LATITUDE, icon = leafIcons)

mapa_roubo_SP3

#Incializando o mapa
mapa_roubo_SP4 = leaflet() %>% 
  setView(lng=-46.6, lat=-23.65, zoom=9 ) %>%
  
  # Add two tiles
  addProviderTiles("Esri.WorldImagery", group="Visualizacao 1") %>%
  addTiles(options = providerTileOptions(noWrap = TRUE), group="Visualizacao 2") %>%
  
  # Add 2 marker groups
  addCircleMarkers(data=base %>% filter(MES == 3), lng=~LONGITUDE , lat=~LATITUDE, radius=5 , color="black",  fillColor="red", stroke = TRUE, fillOpacity = 0.8, group="Marco") %>%
  addCircleMarkers(data=base %>% filter(MES == 4), lng=~LONGITUDE , lat=~LATITUDE, radius=5 , color="black",  fillColor="blue", stroke = TRUE, fillOpacity = 0.8, group="Abril") %>%
  
  # Add the control widget
  addLayersControl(overlayGroups = c("Marco","Abril") , baseGroups = c("Visualizacao 1","Visualizacao 2"), options = layersControlOptions(collapsed = FALSE))


mapa_roubo_SP4

#-------------------------------------------------------------------------------------#
# --------------- Transformando padrao de pontos em dados de area  -------------------#
#-------------------------------------------------------------------------------------#

#Transformando o shape em um objeto da classe sf
SP2 = as(SP, "sf")

#Checando a projecao do sahpe
st_crs(SP2) #CRS: 4326

# Convertendo a base de dados original em um objeto sf
roubos_sf <- roubosSP %>% 
  st_as_sf(
    coords = c("LONGITUDE", "LATITUDE"),
    crs = 4326,
    agr = "constant",
    stringsAsFactors = FALSE,
    remove = TRUE
  )

#Juntando os pontos com os poligonos (subregioes)
roubos_join <- st_join(roubos_sf, SP2, join=st_within)

# Contando o numero de casos por poligono (subregiao)
roubos_contagem <- count(as_tibble(roubos_join), DISTRITO)

#Salvando os dados
write_excel_csv2(roubos_contagem,"dataset/resultado/base_roubos_contagem.csv")

#-------------------------------------------------------------------------------------#
# ---------------------------- Revisando Dados de Area  ------------------------------#
#-------------------------------------------------------------------------------------#

## Lendo o shapefile com os bairros da cidade de Recife
RJ = readOGR("map/RJ/municipiosrj.shp")
  
  ## Plotando o shapefile com todos os bairros da cidade de Recife
  par(mar = c(4,4,2,2))
plot(RJ, axes = TRUE)

#Lendo o arquivo de dados
#Registro de casos de hanseniase
base = read_csv2("dataset/hanseniase.csv")
  #Visualinado a base de dados
  base

#Fazendo a relacao dos dois data frames
RJ@data = left_join(RJ@data, base, by = "ID_OBJETO")

#Visualizando o data frame existente em recife acrescido da variavel 
head(RJ@data)

#-------------------------------------------------------------------------------------#
# ---------------- Mapas coropleticos: Numero de casos de hanseniase  ----------------#
#-------------------------------------------------------------------------------------#


#Plotando mapa coropletico

#Usando o pacote tmap

#Transformando o shape em um objeto da classe sf
RJ2 = as(RJ, "sf")

#Verificando a classe do objeto RJ2
class(RJ2)

#Definindo o tipo do mapa como estatico
tmap_mode("plot")

  #Construindo o mapa
tm_shape(RJ2) + 
  tm_fill(col = "Casos",
          palette = "Greens",
          title = "Numero de casos")

#Construindo o mapa - modificando o numero de intervalos
tm_shape(RJ2) + 
  tm_fill(col = "Casos",
          n = 10,
          palette = "Greens",
          title = "Casos de Hanseniase")

#Construindo o mapa - definindo os limites e o numero de intervalos
mapa = tm_shape(RJ2) + 
  tm_fill(col = "Casos",
          breaks = quantile(RJ2$Casos, probs = seq(0,1,.1)),
          palette = "Greens",
          title = "Casos de Hanseniase")

#Visualizando o mapa
mapa

#Mudando o estilo do mapa
mapa + 
  tm_style("cobalt")

#Incluindo o nome dos municipios com tamanho da fonte definido pelo numero de casos
mapa + 
  tm_text("NOME", size = "Casos")

#Definindo o tipo do mapa como interativo
tmap_mode("view")

tm_shape(RJ2) + 
  tm_fill(col = "Casos",
          breaks = quantile(RJ2$Casos, probs = seq(0,1,.25)),
          palette = "Greens",
          title = "Casos de Hanseniase",
          id = "NOME")

#Grafico para mais de uma variavel
facets = c("Casos","Casos2")

tm_shape(RJ2) + 
  tm_polygons(facets) +
  tm_facets(nrow = 1, sync = TRUE)

#Quick thematic map plot
qtm(RJ2, fill = "Casos")

#Mapas de simbolos proporcionais

#Definindo o tipo do mapa
tmap_mode("plot")

#Construindo o mapa
tm_shape(RJ2) + 
  tm_fill(col = "Casos",
          breaks = quantile(RJ2$Casos, probs = seq(0,1,.25)),
          palette = "Greens",
          title = "Numero de casos") +
  tm_symbols(col = "black", border.col = "white", size = "Casos2")

#-------------------------------------------------------------------------------------#
# -------------------- Criando a matriz W baseado em contiguidade  -------------------#
#-------------------------------------------------------------------------------------#

#W com o criterio queen
W.queen = poly2nb(pl = RJ, row.names = RJ$NOME, queen = TRUE)

## poly2nb - uma das funcoes para criar a matriz W
#Argumentos:
#pl - um objeto da classe SpatialPolygons
#row.names - um vetor indicando os rotulos das sub-regioes
#queen - se TRUE, cria o criterio queen

#Visualizando a quantidade de links criados
summary(W.queen)

#Extraindo as coordenadas dos centroides dos poligonos do shape
coordenadas = coordinates(obj = RJ)
head(coordenadas)

## coordinates - define coordenadas espaciais para criar um objeto espacial
#Argumentos:
#obj - um objeto da classe SpatialPolygons


#Plotando a estrutura de vizinhanca criada
plot(RJ, border = "grey")
plot(W.queen, coordenadas, add = TRUE, col = "red")

#W com o criterio rook
W.rook = poly2nb(pl = RJ, row.names = RJ$NOME, queen = FALSE)

#Visualizando a quantidade de links criados
summary(W.rook)

#Plotando a estrutura de vizinhanca criada
plot(RJ, border = "grey")
plot(W.rook, coordenadas, add = TRUE, col = "yellow")

#Sobrepondo as duas estruturas de vizinhanca
plot(RJ, border = "grey")
plot(W.queen, coordenadas, add = TRUE, col = "red")
plot(W.rook, coordenadas, add = TRUE, col = "yellow")

#-------------------------------------------------------------------------------------#
# ---------------- Definindo os pesos associados aos elementos de W  -----------------#
#-------------------------------------------------------------------------------------#

## Lista de vizinhanca espacial com pesos
recWQW <- nb2listw(neighbours = W.queen, style="W") #outras opcoes: B, C, S e U
recWQW$weights

recWQB <- nb2listw(neighbours = W.queen, style="B") #outras opcoes: B, C, S e U
recWQB$weights

## nb2listw - define uma lista de vizinhanca com pesos espaciais
#Argumentos:
#neighbours - um objeto da classe nb
#style - tipo de peso

#-------------------------------------------------------------------------------------#
# ------------------------------ Autocorrelacao global  ------------------------------#
#-------------------------------------------------------------------------------------#

#Calculando o indice de moram considerando uma estrutura de vizinhanca queen com padronizacao pelas linhas
moran.test(x = RJ2$Casos,listw = recWQW)

## moran.test - calcula o indice global de moran
#Argumentos:
#x - a variavel de interesse
#listw - um objeto do tipo nb2listw

#Calculando o indice de moram considerando uma estrutura de vizinhanca queen com pesos iguais
moran.test(RJ2$Casos,recWQB)

#-------------------------------------------------------------------------------------#
# -------------------------- Autocorrelacao local (LISA)  ----------------------------#
#-------------------------------------------------------------------------------------#

#Calculando o Moran Local
moranlocREC = localmoran(x = RJ2$Casos,listw = recWQW, na.action=na.exclude, zero.policy=TRUE)

## moranlocREC - calcula o indice local de moran
#Argumentos:
#x - a variavel de interesse
#listw - um objeto do tipo nb2listw

#Visualizando as autocorrelacoes locais
head(moranlocREC)

#-------------------------------------------------------------------------------------#
# -------------------- Revisando Dados de Superficie Continua  -----------------------#
# ----------------------- Dados de Temperatura em Sao Paulo  -------------------------#
#-------------------------------------------------------------------------------------#

## Lendo o shapefile com os municipios de Sao Paulo
sp = readOGR("map/SP/35UFE250GC_SIR.shp")

## Plotando o shapefile com os municipios
plot(sp)

#A maneira mais simples de criar mapas com o ggplot2 requer transformar o shapefile para 
# o formato sf
sp2 = as(sp, "sf")

# Plotando apenas o shapefile
ggplot() +
  geom_sf(data = sp2, fill = "white", color = "black") +
  ggtitle("Estado de Sao Paulo")

# Lendo a base de dados 
# A base contem o nome da estacao, suas coordenadas, altitude e temperatura no momento
estacao = read_csv2('dataset/estacao_met.csv', locale = locale(encoding = "ISO-8859-1"))

# O procedimento para criar um mapa com essas estacoes e similar ao de criar um 
# mapa de padrao de pontos. Mas como adicionar a medida de interesse ao mapa?
# Basta adicionar o argumento color dentro da funcao aes() no geom_point!

ggplot()+
  geom_sf(data = sp2, fill = "white") +
  geom_point(data = estacao, aes(x = Long, y = Lat, color = Temp), size = 3) +
  ggtitle("Temperaturas máximas 28/08") 

# E possivel um procedimento similar utilizando o argumento size.
# Os pontos serao maiores de acordo com a escala dos dados recebidos pela funcao.

ggplot()+
  geom_sf(data = sp2, fill = "white") +
  geom_point(data = estacao, aes(x = Long, y = Lat, size = Altitude)) +
  ggtitle("Altitude das estacoes") 

# Tambem e possivel unir as duas formas para representar mais informacoes em 
# um unico mapa!

ggplot()+
  geom_sf(data = sp2, fill = "white") +
  geom_point(data = estacao, aes(x = Long, y = Lat, size = Altitude, color = Temp)) +
  ggtitle("Estacoes climaticas SP") 


#-------------------------------------------------------------------------------------#
# -------------------- Revisando Dados de Superficie Continua  -----------------------#
# ----------------------- Dados de Zinco no solo na Holanda  -------------------------#
#-------------------------------------------------------------------------------------#

#Lendo o arquivo de dados
#Registro das vendas vendas do produto de interesse
base = read_csv("dataset/base_zinco.csv")

#Visualinado a base de dados
base


#Plotando o hispotgrama da variavel de interessa
ggplot(data = base, aes(x=zinc)) + 
  geom_histogram(bins = 20) + 
  xlab("Zinco") + 
  ylab("Frequencia")

#Plotando o hispotgrama do log da variavel de interessa
ggplot(data = base, aes(x=log(zinc))) + 
  geom_histogram(bins = 15) + 
  xlab("Lod do zinco") + 
  ylab("Frequencia")

## ggplot - função para construir graficos
#Argumentos:
#data - data frame
#geom_histogram - cria o histograma
#xlab - modifica o rotulo do eixo x
#ylab - modifica o rotulo do eixo y
#bins - numero de intervalos do histograma

#-------------------------------------------------------------------------------------#
# ------------------ Mapas : valor do Zinco nos locais mensurados  -------------------#
#-------------------------------------------------------------------------------------#

#Plotando a variavel de interesse nos pontos observados com uma escala de cores
ggplot(data = base, aes(x = x, y = y)) + 
  geom_point(aes(color = log(zinc)))

#analisando os valores do log do zinco
summary(log(base$zinc))

#Categorizando a variavel log do zinco
base$logzinc_cat = cut(x = log(base$zinc), breaks = c(4, 4.5, 5, 5.5, 6.5, 7.6))

## cut - transforma uma variavel quantitativa em qualitativa
#Argumentos:
#x - variável quantitativa
#breaks - intervalos das categorias a serem criadas

#Plotando a variavel log do zinco categorizada com escala de cores
ggplot(data = base, aes(x = x, y = y)) + 
  geom_point(aes(color = logzinc_cat)) + 
  scale_color_brewer(palette = "YlGnBu")

## ggplot - função para construir graficos
#Argumentos:
#data - data frame
#geom_point - cria um grafico de pontos
#scale_color_brewer - define a paleta de cores

#-------------------------------------------------------------------------------------#
# ---------------------------- Construindo variogramas  ------------------------------#
#-------------------------------------------------------------------------------------#

#Calculando o variograma
vari = variogram(object = log(zinc) ~ 1, locations = ~x + y, data = base)

## variogram - calcula o variograma
#Argumentos:
#object - use variavel ~ 1 para calcular o variograma default
#locations - localizacoes espaciais
#data - data frame

#Visualizando o objeto criado 
vari

#Plotando o variograma
plot(vari)

#-------------------------------------------------------------------------------------#
# ---------------------------- Investigando anisotropia  -----------------------------#
#-------------------------------------------------------------------------------------#

#Calculando os variogramas direcionais
vari.aniso = variogram(object = log(zinc) ~ 1, locations = ~x + y, data = base, alpha = c(0, 45, 90, 135))


## variogram - calcula o variograma
#Argumentos:
#object - use variavel ~ 1 para calcular o variograma default
#locations - localizacoes espaciais
#data - data frame

#Visualinado o objeto criado
vari.aniso

#Plotando os variogramas direcionais
ggplot(data = vari.aniso, aes(x = dist, y = gamma)) + 
  geom_point() + facet_wrap(~dir.hor)

## ggplot - função para construir graficos
#Argumentos:
#data - data frame
#geom_point - cria um grafico de pontos
#facet_wrap - ddivide o grafico a ser plotado em funcao das categorias da variavel especificada

#-------------------------------------------------------------------------------------#
# ------------------------- Estimando variogramas teoricos  --------------------------#
#-------------------------------------------------------------------------------------#

#Estima os parametros do modelo especificado
fit.variogram(object = vari, model = vgm("Sph"))
fit.variogram(object = vari, model = vgm("Exp"))
fit.variogram(object = vari, model = vgm("Gau"))
fit.variogram(object = vari, model = vgm("Mat"), fit.kappa = TRUE)

## fit.variogram - estima os parametros do variograma teorico especificado
#Argumentos:
#object - variograma estimado
#model - variograma a ser modelado
#fit.kappa - se TRUE estima o kappa da funcao matern

#Ajuste os modelos abaixos usando os valores obtidos no passo anterior
vari.esf.ajus <- vgm(psill = 0.59, model = "Sph", range = 897, nugget = 0.05)
vari.exp.ajus <- vgm(psill = 0.71, model = "Exp", range = 449.76, nugget = 0)
vari.gau.ajus <- vgm(psill = 0.497, model = "Gau", range = 386.54, nugget = 0.11)
vari.mat.ajus <- vgm(psill = 0.578, model = "Mat", range = 221.50, nugget = 0.089, kappa = 1.3) 

grid.arrange(
  plot(vari, vari.esf.ajus, main = "Esferica"),
  plot(vari, vari.exp.ajus, main = "Exponencial"),
  plot(vari, vari.exp.ajus, main = "Gaussiana"),
  plot(vari, vari.exp.ajus, main = "Mattern"),ncol=2)

# Carregando grade para interpolação
zinco.grid = read_csv("dataset/grade_zinco.csv")

#Transforma os dados espaciais como sendo em grade 
gridded(zinco.grid) <- ~x+y

#Define coordenadas espaciais para criar um objeto espacial
coordinates(base) = ~x+y

#Obtendo a krikagem ordinaria
kri.esf = krige(formula = log(zinc)~1, base, zinco.grid, model = vari.esf.ajus)
kri.exp = krige(formula = log(zinc)~1, base, zinco.grid, model = vari.exp.ajus)
kri.gau = krige(formula = log(zinc)~1, base, zinco.grid, model = vari.gau.ajus)
kri.mat = krige(formula = log(zinc)~1, base, zinco.grid, model = vari.mat.ajus)

grid.arrange(
  spplot(kri.esf, "var1.pred", main = "Esferica"),spplot(kri.exp, "var1.pred", main = "Exponencial"),
  spplot(kri.gau, "var1.pred", main = "Gaussiana"),spplot(kri.mat, "var1.pred", main = "Matern"), ncol=2
)

