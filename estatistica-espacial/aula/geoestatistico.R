setwd("/home/rennis/rennisHD00/Projetos/Pessoal/R/rennis-fgv-bigdata/estatistica-espacial")
#-------------------------------------------------------------------------------------#
# ----------------------- Script: Dados de Superficie continua -----------------------#
#-------------------------------------------------------------------------------------#
#Nao esqueca de iniciar sua pasta de trabalho!!! Faca o R "enxergar" o diretorio
#no qual voce ira trabalhar.

#Carregando pacotes
library(tidyverse)
library(lattice)
library(gstat)
library(sp)
library(tidyverse)
library(scales)
library(RColorBrewer)
library(plotGoogleMaps)
library(gridExtra)
library(georob)
library(gstat)



#Lendo o arquivo de dados
#Registro das vendas vendas do produto de interesse
base = read_csv("dataset/base_zinco.csv")

#Visualinado a base de dados
base

#-------------------------------------------------------------------------------------#
# ------------------ Mapas : valor do Zinco nos locais mensurados  -------------------#
#-------------------------------------------------------------------------------------#

#Plotando a variavel de interesse nos pontos observados com uma escala de cores
ggplot(data = base, aes(x = x, y = y)) + geom_point(aes(color = log(zinc)))

#analisando os valores do log do zinco
summary(log(base$zinc))

#Categorizando a variavel log do zinco
base$logzinc_cat = cut(x = log(base$zinc), breaks = c(4, 4.5, 5, 5.5, 6.5, 7.6))

## cut - transforma uma variavel quantitativa em qualitativa
#Argumentos:
#x - variável quantitativa
#breaks - intervalos das categorias a serem criadas

#Plotando a variavel log do zinco categorizada com escala de cores
ggplot(data = base, aes(x = x, y = y)) + geom_point(aes(color = logzinc_cat)) + 
  scale_color_brewer(palette = "YlGnBu")

## ggplot - função para construir graficos
#Argumentos:
#data - data frame
#geom_point - cria um grafico de pontos
#scale_color_brewer - define a paleta de cores

#Outra visualizacao dos dados no mapa

#transformando a base em um objeto data.frame
base2 = as.data.frame(base)

#definindo coordenadas espaciais para criar um objeto espacial
coordinates(base2)<-c("x","y")

#Definindo o argumento do sistema de referência de coordenadas
proj4string(base2) <- CRS("+init=epsg:28992") #http://spatialreference.org

#Fazendo um plot dos valores do 
mapa <- plotGoogleMaps(base2, filename='zinco_mapa.htm', zcol = "zinc", colPalette = brewer.pal(5, "Reds"))

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
ggplot(data = vari.aniso, aes(x = dist, y = gamma)) + geom_point() + facet_wrap(~dir.hor)

## ggplot - função para construir graficos
#Argumentos:
#data - data frame
#geom_point - cria um grafico de pontos
#facet_wrap - ddivide o grafico a ser plotado em funcao das categorias da variavel especificada

#-------------------------------------------------------------------------------------#
# ------------------------- Estimando variogramas teoricos  --------------------------#
#-------------------------------------------------------------------------------------#

vari.esf <- vgm(psill = 0.70, model = "Sph", range = 1000, nugget = 0.15)
vari.exp <- vgm(psill = 0.70, model = "Exp", range = 1000, nugget = 0.15)
vari.gau <- vgm(psill = 0.70, model = "Gau", range = 1000, nugget = 0.15)
vari.mat <- vgm(psill = 0.70, model = "Mat", range = 1000, nugget = 0.15, kappa = 0.2) 

## vgm - gera o variograma teorico
#Argumentos:
#psill - patamar
#model - funcao de covariancia
#range - alcance
#nugget - efeito pepita
#kappa - parametro da funcao matern


#Plotando os variogramas teoricos
grid.arrange(
plot(vari, vari.esf, main = "Esferica"),
plot(vari, vari.exp, main = "Exponencial"),
plot(vari, vari.exp, main = "Gaussiana"),
plot(vari, vari.exp, main = "Mattern"),ncol=2)

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
vari.esf.ajus <- vgm(psill = 0.59060511, model = "Sph", range = 897.0011, nugget = 0.05065971)
vari.exp.ajus <- vgm(psill = 0.7186599, model = "Exp", range = 897.0011, nugget = 0.0000000 )
vari.gau.ajus <- vgm(psill = 0.4974715, model = "Gau", range = 386.5349, nugget = 0.1167886)
vari.mat.ajus <- vgm(psill = 0.5783097, model = "Mat", range = 221.5003, nugget = 0.0890710, kappa = 1.3) 

grid.arrange(
  plot(vari, vari.esf.ajus, main = "Esferica"),
  plot(vari, vari.exp.ajus, main = "Exponencial"),
  plot(vari, vari.exp.ajus, main = "Gaussiana"),
  plot(vari, vari.exp.ajus, main = "Mattern"),ncol=2)
par(mfrow=c(2,2))


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

#Ajustando variogramas para a funcao Mattern variando o alcance
vari.mat.ajus1 <- vgm(psill = 0.09, model = "Mat", range = 200, nugget = 0.09, kappa = 1.3) 
vari.mat.ajus2 <- vgm(psill = 0.09, model = "Mat", range = 400, nugget = 0.09, kappa = 1.3) 
vari.mat.ajus3 <- vgm(psill = 0.09, model = "Mat", range = 600, nugget = 0.09, kappa = 1.3) 
vari.mat.ajus4 <- vgm(psill = 0.09, model = "Mat", range = 1300, nugget = 0.09, kappa = 1.3) 

#Fazendo a krikagem para o matern variando o alcance
kri.mat1 = krige(log(zinc)~1, base, zinco.grid, model = vari.mat.ajus1)
kri.mat2 = krige(log(zinc)~1, base, zinco.grid, model = vari.mat.ajus2)
kri.mat3 = krige(log(zinc)~1, base, zinco.grid, model = vari.mat.ajus3)
kri.mat4 = krige(log(zinc)~1, base, zinco.grid, model = vari.mat.ajus4)

grid.arrange(
  spplot(kri.mat1, "var1.pred", main = "Alcance: 200"), spplot(kri.mat2, "var1.pred", main = "Alcance: 400"),
  spplot(kri.mat3, "var1.pred", main = "Alcance: 600"),spplot(kri.mat4, "var1.pred", main = "Alcance: 1300"), ncol=2
)

