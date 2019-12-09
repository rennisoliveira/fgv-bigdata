# Setando o diretorio de trabalho
setwd("/home/rennis/rennisHD00/Projetos/Pessoal/R/rennis-fgv-bigdata/estatistica-espacial")

#-------------------------------------------------------------------------------------#
# -------------------- Inicie instalando o pacote GGMAP do GITHUB --------------------#
#-------------------------------------------------------------------------------------#
# install.packages("devtools")
# install.packages("rgdal")
# if(!requireNamespace("devtools")) install.packages("devtools")
# devtools::install_github("dkahle/ggmap", ref = "tidyup")

## Especifico para maquinas Linux
if(!exists(".sp2owin", mode="function")) source("../utils/sp2owin.R")
library(ggmap)

#-------------------------------------------------------------------------------------#
# ---------------- Script: R como um Sistema de Informacao Geografica ----------------#
#-------------------------------------------------------------------------------------#

#Carregando o pacote rgdal
library(rgdal)
## Lendo o shapefile com os Recife da cidade de Recife
recife = readOGR("map/Recife/Recife.shp")

## Plotando o shapefile com todos os bairros da cidade de Recife
plot(recife)

## Informacao sobre a classe do objeto recife
class(recife)

#Acessando o nome das variaveis que se encontram dentro do data frame do shapefile
names(recife)

#Visualizando as linhas iniciais do data frame que se encontra no shapefile
head(recife@data$EBAIRRNOME)

#Carregando o pacote tidyverse
library(tidyverse)

#Lendo o arquivo de dados
#Registro das vendas vendas do produto de interesse
base = read_csv2("dataset/Vendas.csv")

#Visualinado a base de dados
base

#Calculando o numero de vendas do produto para cada bairro
base_aux = base %>% 
  group_by(no_bairro_residencia) %>% 
  summarise(num.vendas = n())
## %>% - Operador pipe adiciona o elemento a esquerda como primeiro argumento da funcao a direita
## group_by - transforma o tible em um tible agrupado
## summarise - utilizada em tibles agrupados para calcular novas variaveis pelo agrupamento considerado 
#-------------------------------------------------------------------------------------#
#Associando os bairros de Recife ao numero de produtos vendido
#As informacoes sao relacionadas a partir do nome do Bairro (coluna EBAIRRNOME no objeto 
#recife e coluna no_bairro_residencia no objeto base)
#-------------------------------------------------------------------------------------#
#Verificando a classe da variavel no_bairro_residencia
class(base_aux$no_bairro_residencia)

#Verificando a classe da variavel EBAIRRONOME do objeto recife@data
class(recife@data$EBAIRRNOME)

#Transformando a variavel no_bairro_residencia em fator para ficar compatível com recife@data
base_aux$no_bairro_residencia = factor(base_aux$no_bairro_residencia)

#Fazendo a relacao dos dois data frames
recife@data = left_join(x = recife@data, y = base_aux, by = c("EBAIRRNOME" = "no_bairro_residencia"))

## funcao left_join combina duas bases por meio de uma variavel, mantendo todos as linhas da base a esquerda 
#Argumentos:
## x - primeiro data frame
## y - segundo data frame
## by - variavel utilizada para fazer o link

#Visualizando o data frame existente em recife acrescido da variavel 
head(recife@data$num.vendas)

#Carregando o pacote sp e RColorBrewer
library(sp)
library(RColorBrewer)
#-------------------------------------------------------------------------------------#
# ------------------ Mapa coropleticos: Venda do produto em Recife  ------------------#
#-------------------------------------------------------------------------------------#
#Definindo os intervalos das classes
intervalos=c(0,50,100,200,300,400,500,700,1000)
#Plotando o mapa com tons avermelhados
spplot(
    obj =   recife
          , zcol = c("num.vendas")
          , at = intervalos
          , col.regions = brewer.pal(9, "Reds")
    ) #Outras opções de cores: Greens, BrBG, Accent

## spplot - funcao para plotar dados espaciais com atributos
#Argumentos:
#obj - um objeto da classe spatial-class
#zol - a variavel/atributo que deseja plotar
#at - o intervalo que definem as classes
#col.regions - vetor com as cores
#Definindo intervalos usando os dados
intervalos1 = pretty(x = recife@data$num.vendas, n = 6)
#Plotando o mapa com tons avermelhados
spplot(obj = recife, zcol = c("num.vendas"), at = intervalos1, col.regions = brewer.pal(7, "Reds")) #Outras opções de cores: Greens, BrBG, Accent
#Obtendo medidas descritivas da variavel numero de vendas para avaliar o intervalo escolhido
summary(recife@data$num.vendas)
#Criando os intervalos com base em quantis
intervalos2 = quantile(recife$num.vendas, probs = seq(0,1,0.125))
intervalos2
intervalos2[9] = intervalos2[9] + 1
#Plotando o mapa com tons avermelhados
spplot(obj = recife
        , zcol = c("num.vendas")
       , at = intervalos2
       , col.regions =brewer.pal(8, "Reds")
      ) #Outras opções de cores: Greens, BrBG, Accent

#Carregando o pacote cartography
library(cartography)  

#Mapas coropleticos
#Serve para criar figuras com mais de um grafico
par(mfrow = c(1,2))
#serve para modificar as margens das figuras
par(mar = c(0.5,0.5,1.5,0.5))

#Grafico 1
plot(recife)  
choroLayer(spdf = recife, var = "num.vendas", border = "gray",  
           method = "equal", # "sd", "equal", "quantile", "fisher-jenks","q6" or "geom"  
           nclass = 5, lwd = 0.4, col = brewer.pal(5, "Reds"),  
           legend.pos = "topright", legend.title.txt = "Numero de Vendas", add= TRUE)  
#Grafico 2
plot(recife)  
choroLayer(spdf = recife, var = "num.vendas", border = "white",  
           method = "quantile", # "sd", "equal", "quantile", "fisher-jenks","q6" or "geom"  
           nclass = 5, lwd = 0.4, col = brewer.pal(5, "Greens"),  
           legend.pos = "bottomleft", legend.title.txt = "Numero de Vendas",add= TRUE)  
par(mfrow = c(1,1))

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

#-------------------------------------------------------------------------------------#
# ---------------------- Comparando as vendas no dois semestres  ---------------------#
#-------------------------------------------------------------------------------------#

#Suponha que o interesse seja o de comparar o numero de compras do produto no primeiro semestre e no segundo semestre

#Carregando o pacote stringr      
library(stringr)
library(lubridate)
#Inicialmente iremos criar as duas variaveis: vendas no 1o semestre e vendas no 2o semestre e vendas totais
base_aux2 =  base %>% 
  mutate(
            data_compra = str_sub(dt_compra,1,8)
          , data = dmy(data_compra)
          , semestre = semester(data_compra)) %>%
  group_by(no_bairro_residencia) %>% 
  summarise(num.vendas.1sem = length(semestre[semestre == 1]), num.vendas.2sem = length(semestre[semestre == 2]))
base_aux2
#Transformando a variavel no_bairro_residencia em fator para ficar compativel com recife@data
base_aux2$no_bairro_residencia = factor(base_aux2$no_bairro_residencia)

#Fazendo a relacao dos dois data frames
recife@data = left_join(recife@data, base_aux2, by = c("EBAIRRNOME" = "no_bairro_residencia"))

#Visualizando o data frame existente em recife acrescido da variavel 
head(recife@data)

#Criando o intervalo
intervalos3 = quantile(c(recife@data$num.vendas.1sem,recife@data$num.vendas.2sem), probs = seq(0,1,0.125))
intervalos3[9] = intervalos3[9] + 1
intervalos3

#Plotando o mapa com tons alaranjados 
spplot(recife,c("num.vendas.1sem","num.vendas.2sem"),at=intervalos3,col.regions =brewer.pal(8, "Oranges"),names.attr = c("1 Semestre","2 Semestre")) 

#-------------------------------------------------------------------------------------#
# ----------------------- Transformando coordenadas no shape  ------------------------#
#-------------------------------------------------------------------------------------#

#Importando o shapefile de Nova York
NYShp <- readOGR("map/nyshape/nyshape.shp")
# Linux Usar - David 
#NYShp <- readShapeSpatial("../map/nyshape/nyshape.shp")

#Plotando o mapa de Nova York e visualizando os eixos
par(mar = c(2.5,2.5,1.5,0.5))
plot(NYShp, axes = TRUE)

#Plotando o mapa de Recife e visualizando os eixos
plot(recife, axes = TRUE)

#Transformando as coordena
recife2 = spTransform(x = recife, CRSobj = CRS("+proj=longlat +datum=WGS84"))
plot(recife2, axes = TRUE)

##spTransform - funcao para modificar a projecao dos dados
#Argumentos:
#x - objeto a ser transformado
#CRSobj - objeto da classe CRS (sistema de referencia de coordenadas)

#-------------------------------------------------------------------------------------#
# -------------------- Mapa com a localização exata de um evento  --------------------#
# -------------------- Mapa com a localização dos delitos em NY  ---------------------#
#-------------------------------------------------------------------------------------#

#Importando o arquivo com as localizações dos delitos em NY
NYPD<-read_csv2("dataset/NYPD.csv")

#Carregando o pacote spatstat e o pacote maptools
# 
remove.packages("spatstat")
library(spatstat)
# 
remove.packages("maptools")
library(maptools)

#Definindo o shapefile como uma janela onde os pontos serao plotados - necessario para o uso do pacote spatstat

# Não funciona no Linux
# NYO <- as.owin(NYShp)
NYO <- .sp2owin(NYShp)

##as.owin - definir o shapefile como a janela onde serão plotados os pontos
#Argumentos:
#W - um objeto da classe SpatialPolygonDataFrame (por exemplo)

#Plotando o shapefile
plot(NYO)

#Criando o padrao de pontos a ser plotado
NYppp = ppp(NYPD$Longitude, NYPD$Latitude, window=NYO)

##ppp - criar um objeto com classe ppp representando o padrao de pontos
#Argumentos:
#x - longitude
#y - latitude
#window - um objeto owin. 

#Plotando as localizacoes dos delitos
par(mar=c(0.5,0.5,1.5,0.5))
plot(NYppp, pch=21, cex=0.9, bg="blue", main="Ocorrencias de crimes em Nova York")

#-------------------------------------------------------------------------------------#
# ------------------------ Interacao do R com o Google Maps  -------------------------#
#-------------------------------------------------------------------------------------#

#Carregando os pacotes ggplot2, ggmap e gridExtra
library(ggplot2)
library(gridExtra)

#Definindo a API
register_google(key="AIzaSyA6n4J6vbGg1D76ZiNFnhmSeLH8es_p4y8")


#Salvando graficos da cidade de NY com diferentes layouts
nyter = get_googlemap('New York City',zoom=10,maptype='terrain')
nysat = get_googlemap('New York City',zoom=10,maptype='satellite')
nyrod = get_googlemap('New York City',zoom=10,maptype='roadmap')
nyhyb = get_googlemap('New York City',zoom=10,maptype='hybrid')

##get_googlemap - faz grafico com estetica Google Maps
#Nome da regiao - O nome do lugar que se deseja baixar o mapa. 
#Zoom - Deve ser um valor inteiro de 1 a 21 - proximidade da figura
#Maptype - O tipo do mapa que sera baixado do Google
  #Terrain - Mapa com marcacoes das ruas
  #Satellite - Mapa com imagens de satelite
  #Roadmap - Mapa com marcacoes de vias importantes
  #Hybrid - Hibrido entre o mapas de Satelite e Roadmap

#Plotando os graficos em uma grade
grid.arrange(ggmap(nyter) + ggtitle("Terrain"), 
             ggmap(nysat) + ggtitle("Satellite"),
             ggmap(nyrod) + ggtitle("Roadmap"),
             ggmap(nyhyb) + ggtitle("Hybrid"), ncol=2)

## grid.arrange - funcao equivalente a par(mfrow=c(,)), plota mais de uma gráfico em uma figura
#Argumentos
#... - graficos
#ncol - numero de colunas que a figura deve ter

#Plotando as ocorrencias sobre o mapa do Google Maps 
ggmap(nysat) + geom_point(data=NYPD, aes(x=Longitude,y=Latitude), col="red", size=1.5, alpha=0.5)

## Grafico com camadas que sao adicionadas pelo +
## ggmap - funcao que plota o grafico do google maps de NY com o estilo Satellite
## geom_point - funcao que especifica que sera incluida uma camada sobre o grafico feito na camada anterior
# Argumentos
# data - base de dados
# aes - define a estetica do grafico, no caso acima, esta sendo pedido para plotar a variavel Longitude em x e a Latitude em y (serao pontos, pois estamos no geom_point)
# col - define a cor dos pontos
# size - define o tamanho dos pontos
# alpha - define a opacidade

#Sobrepondo o shapefile no mapa do Google Maps e plotando os pontos
ggmap(nysat) + 
  geom_polygon(aes(x = long, y = lat, group = group), data = NYShp,
               colour ='white ', fill =' black ', alpha = .4, size = .3) +
  geom_point(data=NYPD, aes(x=Longitude,y=Latitude), col="red", size=1.5, alpha=0.5)


#Uma outra possibilidade
qmplot(x = Longitude, y = Latitude, data = NYPD,
       colour = I('red'), size = I(1.5), darken = .3)

##qmplot - plot de mapas rapidos (quick map plot)
#Argumentos:
#x - longitude
#y - latitude
#data - base de dados
#colour - cor
#size - tamanho do simbolo
#darken - quanto maior, mais escuro sera a cor

#-------------------------------------------------------------------------------------#
# ---------------------------- Geocodificando enderecos  -----------------------------#
#-------------------------------------------------------------------------------------#

library(googleway)

endereco = google_geocode(address = "Praia de Botafogo, 190 - Botafogo, Rio de Janeiro - RJ, 22250-900", key = "AIzaSyA6n4J6vbGg1D76ZiNFnhmSeLH8es_p4y8")

##google_geocode - georreferebcia enderecos

endereco$results$geometry$location

FGV = get_googlemap(center = c(endereco$results$geometry$location$lng, 
                               endereco$results$geometry$location$lat), 
                    zoom=17, maptype='terrain')

ggmap(FGV)

#Importando a base de enderecos
base = read_delim("dataset/Enderecos.csv", delim=";")
base

