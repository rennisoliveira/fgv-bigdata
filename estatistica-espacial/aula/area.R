#-------------------------------------------------------------------------------------#
# ------------------------------- Script: Dados de Area ------------------------------#
#-------------------------------------------------------------------------------------#

# Setando o diretorio de trabalho
setwd("/home/rennis/rennisHD00/Projetos/Pessoal/R/rennis-fgv-bigdata/estatistica-espacial")

## Especifico para maquinas Linux
if(!exists(".sp2owin", mode="function")) source("../utils/sp2owin.R")

#Carregando o pacote rgdal
library(rgdal)

## Lendo o shapefile com os bairros da cidade de Recife
recife = readOGR("map/Recife/Recife.shp")

## Plotando o shapefile com todos os bairros da cidade de Recife
plot(recife, axes = TRUE)

#Transformando para latitude e longitude

# Linux nao pode fazer - Trava Cartogram
recife2 = spTransform(x = recife, CRSobj = CRS("+proj=longlat +datum=WGS84"))
plot(recife2, axes = TRUE)


## spTransform - transformacao de dados e projecao de mapas
#Argumentos:
#x - shape a ser transformado
#CRSobj - objeto da classe de sistema de referencia de coordenadas

#Carregando o pacote tidyverse
library(tidyverse)

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

#Transformando a variavel no_bairro_residencia em fator para ficar compatível com recife@data
base$no_bairro_residencia = factor(base$no_bairro_residencia)

#Fazendo a relacao dos dois data frames
recife@data = left_join(x = recife@data, y = base, by = c("EBAIRRNOME" = "no_bairro_residencia"))

#Visualizando o data frame existente em recife acrescido da variavel 
head(recife@data)

#Carregando o pacote sp e RColorBrewer
library(sp)
library(RColorBrewer)
library(cartography)
library(cartogram)
library(tmap) 

#-------------------------------------------------------------------------------------#
# ------------------ Mapa Cartograma: Venda do produto em Recife  --------------------#
#-------------------------------------------------------------------------------------#
class(recife)
head(recife@data$num.vendas)
dados_cartograma = cartogram(shp = recife, weight = "num.vendas", itermax = 5)

## cartogram - constroi cartogramas com base no algoritmo de rubber sheet distortion
#Argumentos:
#shp - um objeto da classe SpatialPolygonsDataFrameclass
#weight - o nome da variavel contida em shp que sera utilizada para o peso
#itermax - numero maximo de interacoes para a transformacao do cartograma

tm_shape(shp = dados_cartograma) +
  tm_polygons(col = "num.vendas",
              style="quantile", # "cat", "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", and "jenks"
              palette = brewer.pal(9, "YlOrRd"), title=c("Numero de vendas")) +
  tm_layout(frame=FALSE)

#Argumentos:
#shp - um objeto da classe SpatialPolygonsDataFrameclass
#col - a variavel que deseja plotar
#style - tipo de divisao para criar os intervalos
#palette - paleta de cores
#title - Titulo da legenda
#frame - FALSE faz com que um quadro ao redor da figura nao seja plotado

#-------------------------------------------------------------------------------------#
# ------------- Mapa Simbolos proporcionais: Venda do produto em Recife  -------------#
#-------------------------------------------------------------------------------------#
par(mar = c(0.5,0.5,1.5,0.5))
plot(recife, col = "gray", border = "white", lwd=0.4)   
propSymbolsChoroLayer(spdf = recife, var = "num.vendas", var2 = "porc_panfletagem", inches = 0.15,   
                      col = brewer.pal(9, "YlOrRd"), 
                      method = "quantile", # "equal", "sd"   
                      nclass = 5, border = NULL, legend.var.pos = "n", legend.var2.pos = "topleft",   
                      legend.var2.title.txt = "Porcentagem panfletagem")   

## propSymbolsChoroLayer - funcao para plotar dados espaciais com atributos
#Argumentos:
#spdf - um objeto da classe SpatialPolygonsDataFrameclass
#var1 - a variavel/atributo que determinara o tamanho do circulo
#var1 - a variavel/atributo que determinara a cor do circulo
#inches - o raio do maior circulo
#col - vetor com as cores
#method - metodo para fazer a divisao dos intervalos
#nclass - numero de classes (intervalos)
#border - cor da borda dos simbolos
#legend.var.pos - posicao da legenda
#legend.var2.title.txt - titulo da legenda

#-------------------------------------------------------------------------------------#
# ------------------ Mapas coropleticos: Venda do produto em Recife  -----------------#
#-------------------------------------------------------------------------------------#


#Plotando o mesmo mapa coropletico, usando funcoes diferentes

#Usando a funcao spplot

#Criando os intervalos com base em quantis
intervalos1=quantile(recife$num.vendas, probs = seq(0,1,0.125))
intervalos1[9] = intervalos1[9] + 1

#Plotando o mapa com tons avermelhados
spplot(obj = recife, zcol = c("num.vendas"), at = intervalos1, col.regions =brewer.pal(8, "Reds")) #Outras opções de cores: Greens, BrBG, Accent


#Usando a funcao choroLayer 
par(mar = c(0.5,0.5,1.5,0.5))
plot(recife)  
choroLayer(spdf = recife, var = "num.vendas", border = "gray",  
           method = "equal", # "sd", "equal", "quantile", "fisher-jenks","q6" or "geom"  
           nclass = 5, lwd = 0.4, col = brewer.pal(5, "Reds"),  
           legend.pos = "topleft", legend.title.txt = "Numero de Vendas", add= TRUE)  


## choroLayer - funcao para plotar dados espaciais com atributos
#Argumentos:
#spdf - um objeto da classe sf
#var - a variavel/atributo que deseja plotar
#border - cor das linhas que definem os poligonos
#method - o metodo que vai definir os intervalos de classes
#nclass - o numero de classes
#lwd - a espessura da linha dos poligonos
#col - vetor com as cores
#legend.pos - posicao da legenda
#legend.title.txt - titulo do texto
#add - para adicionar sobre o grafico que esta plotado


#Transformando o shapefile para o ggplot aceitar
recife2 = as(recife, "sf")

#Classe do objeto
class(recife2)

# Plotando apenas o shapefile
ggplot() +
  geom_sf(data = recife2, fill = "white", color = "black") +
  ggtitle("Cidade de Recife")

# Adicionando os dados ao mapa
# Basta adicionar a variavel ao argumento fill dentro da funcao aes()
# O mapa e compativel com outros argumentos das funcoes do ggplot2!
# scale_fill_continuous: low = cor do valor mais baixo no gradiente, high = cor do valor mais alto
# limits = amplitude do intervalo, name = titulo da legenda

summary(recife2$num.vendas.1sem)

graf = ggplot() +
  geom_sf(data = recife2, aes(fill = num.vendas.1sem), color = "transparent") +
  ggtitle("Vendas 1 semestre") +
  scale_fill_continuous(low = "red", high = "darkgreen", limits = c(1, 1420), name = "Numero de vendas")

graf

graf + scale_fill_viridis_c(name = "Numero de vendas")
graf + scale_fill_gradient(name = "Numero de vendas")


## ggplot - plota graficos
#Argumentos:
#geom_sf - plotando poligonos
#scale_fill_continuous - define a escala de cores

#Ativando o pacote plotly
library(plotly)

#Criando um grafico interativo
ggplotly(graf)

#Criando intervalo para a variavel numero de vendas
recife2 = recife2 %>% 
  mutate(num.vendas.cat = cut(num.vendas,breaks = c(0,80,150,250,350,450,1000,2000,3100)))

## mutate - cria novas variaveis dentro da base de dados

#Plotando a variavel categorizada
graf2 = ggplot() +
  geom_sf(data = recife2, aes(fill = num.vendas.cat)) +
  ggtitle("Vendas 1 semestre") 

graf2

#Ativando o pacote ggmap
library(ggmap)

#Definindo a API
register_google(key="AIzaSyA6n4J6vbGg1D76ZiNFnhmSeLH8es_p4y8")

#Pegando o mapa de Recife do Google Maps
recsat = get_googlemap('Recife',zoom=11,maptype='satellite')
plot(recsat)

##get_googlemap - faz grafico com estetica Google Maps
#Nome da regiao - O nome do lugar que se deseja baixar o mapa. 
#Zoom - Deve ser um valor inteiro de 1 a 21 - proximidade da figura
#Maptype - O tipo do mapa que sera baixado do Google
#Terrain - Mapa com marcacoes das ruas
#Satellite - Mapa com imagens de satelite
#Roadmap - Mapa com marcacoes de vias importantes
#Hybrid - Hibrido entre o mapas de Satelite e Roadmap


#Plotando sobre o mapa do google maps o grafico coropletico

# Definindo uma funcao que fixa a caixa em EPSG:3857
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

# Usando a funcao ggmap_bbox:
map <- ggmap_bbox(recsat)
plot(map)

# Transformando recife2 para EPSG 3857 (Pseudo-Mercator, usado pelo Google)
recife_3857 <- st_transform(recife2, 3857)

#Plotando o mapa sobre o mapa do google
ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = recife_3857, aes(fill = num.vendas), inherit.aes = FALSE, alpha = 0.65) + 
  scale_fill_continuous(low = "white", high = "red", name = "Numero de vendas" )

#-------------------------------------------------------------------------------------#
# -------------------- Criando a matriz W baseado em contiguidade  -------------------#
#-------------------------------------------------------------------------------------#

#Carregando o pacote stringr      
library(spdep)

#W com o criterio queen
W.queen = poly2nb(pl = recife, row.names = recife$EBAIRRNOME, queen = TRUE)

## poly2nb - uma das funcoes para criar a matriz W
#Argumentos:
#pl - um objeto da classe SpatialPolygons
#row.names - um vetor indicando os rotulos das sub-regioes
#queen - se TRUE, cria o criterio queen

#Visualizando a quantidade de links criados
summary(W.queen)

#Extraindo as coordenadas
coordenadas = coordinates(obj = recife)
head(coordenadas)

## coordinates - define coordenadas espaciais para criar um objeto espacial
#Argumentos:
#obj - um objeto da classe SpatialPolygons


#Plotando a estrutura de vizinhanca criada
plot(recife, border = "grey")
plot(W.queen, coordenadas, add = TRUE, col = "red")

#Visualizando a matriz W com o criterio queen
mat.W.queen <- nb2mat(W.queen, style = "B")
colnames(mat.W.queen) <- rownames(mat.W.queen)
mat.W.queen[1:5,1:5]

#W com o criterio rook
W.rook = poly2nb(pl = recife, row.names = recife$EBAIRRNOME, queen = FALSE)

#Visualizando a quantidade de links criados
summary(W.rook)

#Plotando a estrutura de vizinhanca criada
plot(recife, border = "grey")
plot(W.rook, coordenadas, add = TRUE, col = "yellow")


#Sobrepondo as duas estruturas de vizinhanca
plot(recife, border = "grey")
plot(W.queen, coordenadas, add = TRUE, col = "red")
plot(W.rook, coordenadas, add = TRUE, col = "yellow")


#-------------------------------------------------------------------------------------#
# --------------------- Criando a matriz W baseado em distancia  ---------------------#
#-------------------------------------------------------------------------------------#

#Extraindo os k vizinhos mais proximos
k1viz = knearneigh(x = coordenadas, k = 1)
k3viz = knearneigh(x = coordenadas, k = 3)

## knearneigh - define os k vizinhos mais proximos
#Argumentos:
#x - objeto do tipo SpatialPoints
#k - numero de vizinhos

#Plotando a estrutura de vizinhanca criada
par(mfrow = c(1,2))
#Vizinhanca considerando o vizinho mais proximo
plot(recife, border = "grey")
plot(knn2nb(knn = k1viz), coordenadas, add = TRUE, col = "blue")
#Vizinhanca considerando os 3 vizinhos mais proximos
plot(recife, border = "grey")
plot(knn2nb(knn = k3viz), coordenadas, add = TRUE, col = "blue")
par(mfrow = c(1,1))

## knn2nb - define uma lista de vizinhanca de um objeto knn
#Argumentos:
#knn - um objeto retornado por knearneigh

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
# ------------------------ Avaliando variavel x media movel  -------------------------#
#-------------------------------------------------------------------------------------#


#Padronizando os dados
vendas_padronizada <- scale(x = base$num.vendas)
vendas_W <- mat.W.queen %*% vendas_padronizada

## scale - padroniza um conjunto de valores
#Argumentos:
#x - conjunto de valores a ser padronizado


#Plotando variavel versus media movel
par(mar=c(4,4,2,.5))
plot(vendas_padronizada, vendas_W, pch = 19, lwd = 2, cex = 0.9, xlab = "Z", ylab = "WZ")
abline(v=0, h=0)

#-------------------------------------------------------------------------------------#
# ------------------------------ Autocorrelacao global  ------------------------------#
#-------------------------------------------------------------------------------------#

#Calculando o indice de moram considerando uma estrutura de vizinhanca queen com padronizacao pelas linhas
moran.test(x = recife$num.vendas,listw = recWQW)

## moran.test - calcula o indice global de moran
#Argumentos:
#x - a variavel de interesse
#listw - um objeto do tipo nb2listw

#Calculando o indice de moram considerando uma estrutura de vizinhanca queen com pesos iguais
moran.test(recife$num.vendas,recWQB)


## moran.plot - plota a variavel versus os valores transformados em funcao de W
#Argumentos:
#x - a variavel de interesse
#listw - um objeto do tipo nb2listw

#-------------------------------------------------------------------------------------#
# ------------------------------ Autocorrelacao local  -------------------------------#
#-------------------------------------------------------------------------------------#

#Calculando o LISA
moranlocREC = localmoran(x = recife$num.vendas,listw = recWQW, na.action=na.exclude,zero.policy=TRUE)

## moranlocREC - calcula o indice local de moran
#Argumentos:
#x - a variavel de interesse
#listw - um objeto do tipo nb2listw

#Visualizando as autocorrelacoes locais
head(moranlocREC)

moranlocREC

