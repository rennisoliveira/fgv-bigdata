# Set diretorio
setwd("/home/rennis/rennisHD00/Projetos/Pessoal/R/rennis-fgv-bigdata/estatistica-espacial-aplicacoes")

#Carregando os pacotes necessarios
library(rgdal)
library(MASS)
library(RColorBrewer)
library(spdep)
library(tidyverse)
library(cartography)
library(tmap)
library(spatialreg)
library(corrplot)

#Importando a base de dados de incendios da regiao de Castilla-La Mancha
municipio_brasileiro = read_csv2("dataset/Municipios_Brasileiros.csv") 
homicidios_rj = read_csv2("dataset/Dados Homicidios RJ.csv")
df_municipio_homicidio = inner_join(x = municipio_brasileiro, y = homicidios_rj
                , by = c(  "Código IBGE" = "Codigo IBGE")
)
rm(municipio_brasileiro)
rm(homicidios_rj)

## Lendo o shapefile de regioes do Censo de NY
RJ = readOGR("map/Shapefile municípios RJ/municipiosRJ_2010.shp")

#Visualizando os dados (que ja encontram-se inseridos no shape)
head(RJ@data)
as.owin.SpatialGridDataFrame()
#Transformando variaveis na base de dados
# RJ@data$populacao = RJ@data$populacao

#Defina seu mapa coropletico usando o pacote tmap
# 1 - e preciso transformar o shape em um objeto sf
# 2 - faca uma avaliacao sobre o numero de classes e seus limites
# 3 - escolha uma paleta que possua coras mais intensas para uma maior quantidade de casos
#Transformando o shape em um objeto da classe sf
RJ2 = as(RJ, "sf")



if(1 == 2) {
  
  #Definindo o tipo do mapa como estatico
  tmap_mode("plot")
  #Construindo o mapa para a proporcao
  tm_shape(RJ2) + 
    tm_fill(col = "pip",
            palette = "RdYlGn",
            breaks = quantile(RJ$pip),
            title = "Proporção Transformada")
  #Construindo o mapa para Z
  tm_shape(NY2) + 
    tm_fill(col = "Z",
            breaks = quantile(NY2$Z, propbs = seq(0,1,0.125)),
            palette = "Reds",
            title = "Variavel Z",
            midpoint = NA)
  
  #Avaliando autocorrelacao entre as variaveis explicativas e a variavel resposta
  dad = tibble(Z = NY2$Z, PEXPOSURE = NY2$PEXPOSURE, PCTAGE65P = NY2$PCTAGE65P, PCTOWNHOME = NY2$PCTOWNHOME)
  correlacao = cor(dad)
  corrplot(correlacao, method = "color")
  
  #-------------------------------------------------------------------------------------#
  # --- Criando a matriz W baseado no criterio QUENN com peso padronizado por linha  ---#
  #-------------------------------------------------------------------------------------#
  
  #W com o criterio queen
  W.queen = poly2nb(pl = NY2, queen = TRUE)
  W.queen
  ## poly2nb - uma das funcoes para criar a matriz W
  #Argumentos:
  #pl - um objeto da classe SpatialPolygons
  #row.names - um vetor indicando os rotulos das sub-regioes
  #queen - se TRUE, cria o criterio queen
  W.Queen.pesoW <- nb2listw(neighbours = W.queen , style="W") #outras opcoes: B, C, S e U
  W.Queen.pesoW
  
  #-------------------------------------------------------------------------------------#
  # ----------- Criando a matriz W baseado no criterio QUENN com peso binário ----------#
  #-------------------------------------------------------------------------------------#
  
  ## Lista de vizinhanca espacial com pesos
  W.Queen.pesoB <- nb2listw(neighbours = W.queen, style="B") #outras opcoes: B, C, S e U
  W.Queen.pesoB
  
  #-------------------------------------------------------------------------------------#
  # Criando a matriz W com 3 vizinhos para cada regiao e com peso padronizado por linha #
  #-------------------------------------------------------------------------------------#
  
  #Extraindo as coordenadas do shape
  coordenadas = coordinates(NY)
  
  #Extraindo os k vizinhos mais proximos
  k3viz = knearneigh(x = coordenadas, k = 3)
  
  ## knearneigh - define os k vizinhos mais proximos
  #Argumentos:
  #x - objeto do tipo SpatialPoints
  #k - numero de vizinhos
  
  #W com 1 vizinho
  W.3viz = knn2nb(knn = k3viz)
  
  ## knn2nb - define uma lista de vizinhanca de um objeto knn
  #Argumentos:
  #knn - um objeto retornado por knearneigh
  
  ## Lista de vizinhanca espacial com pesos
  W.3viz.pesoW<- nb2listw(neighbours =W.3viz  , style="W") #outras opcoes: B, C, S e U
  W.3viz.peso<- nb2listw(neighbours =W.3viz  , style="B") #outras opcoes: B, C, S e U
  
  
  #-------------------------------------------------------------------------------------#
  # ------------------------------ Autocorrelacao global  ------------------------------#
  #-------------------------------------------------------------------------------------#
  
  #Calcule o indice de moran considerando as tres estruturas de vizinhanca criada
  # 1 - e preciso especificar a variavel de interesse
  # 2 - e preciso especificar uma lista de vizinhanca (W comm os pesos definidos)
  
  moran.test(x = NY$PROPCAS, listw = W.Queen.pesoW)
  moran.test(x = NY$PROPCAS, listw = W.Queen.pesoB)
  moran.test(x = NY$PROPCAS, listw = W.3viz.pesoW)
  moran.test(x = NY$PROPCAS, listw = W.3viz.pesoB)
  
  ## moran.test - calcula o indice global de moran
  #Argumentos:
  #x - a variavel de interesse
  #listw - um objeto do tipo nb2listw
  
  # Conclusoes?
  
  #Diferencas entre as 3 matrizes de vizinhanca?
  
  #-------------------------------------------------------------------------------------#
  # ---------------- Ajustando um modelo de regressao linear multiplo  -----------------#
  #-------------------------------------------------------------------------------------#
  
  #Ajustando um modelo linear multiplo
  ajusteML1 = lm(formula = Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data = NY2)
  
  ## lm - ajusta um modelo de regressao linear multiplo
  #Argumentos:
  #formula - modelo a ser ajustado (indicando a variavel resposta e as variaveis explicativas)
  #data - data frame que contem as variaveis
  
  summary(ajusteML1)
  
  #Normalidade dos resíduos
  ggplot(data.frame(res = residuals(ajusteML1)), aes(sample = res)) + stat_qq() + stat_qq_line()
  
  #Verificando independencia dos residuos
  moran.test(x = residuals(ajusteML1),listw = W.Queen.pesoW)
  
  #-------------------------------------------------------------------------------------#
  # ----------------------------- Ajustando um modelo SAR  -----------------------------#
  #-------------------------------------------------------------------------------------#
  
  #Ajustando um modelo SAR
  ajusteSAR1 = spautolm(formula = Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data = NY@data, listw = W.Queen.pesoW, family = "SAR")
  
  ## spautolm - ajusta um modelo autorregressivo ou um modelo condicional
  #Argumentos:
  #formula - modelo a ser ajustado (indicando a variavel resposta e as variaveis explicativas)
  #data - data frame que contem as variaveis
  #family - o modelo a ser ajustado
  #listw - define a estrutura de vizinhanca
  
  summary(ajusteSAR1)
  
  #Normalidade dos resíduos
  ggplot(data.frame(res = residuals(ajusteSAR1)), aes(sample = res)) + stat_qq() + stat_qq_line()
  
  #Verificando independencia dos residuos
  moran.test(x = residuals(ajusteSAR1),listw = W.Queen.pesoW)
  
  #-------------------------------------------------------------------------------------#
  # ----------------------------- Comparando o ML e o SAR ------------------------------#
  #-------------------------------------------------------------------------------------#
  
  #Comparando o AIC
  AIC(ajusteML1)
  
  AIC(ajusteSAR1)
  
  
  #Comparando os erros
  erroML = sum((ajusteML1$fitted.values - NY@data$Z)^2)
  erroSAR = sum((ajusteSAR1$fit$fitted.values - NY@data$Z)^2)
  
  erroML
  
  erroSAR
  
  
  #Criando a variavel residuo dentro do shape
  NY2$resML = resid(ajusteML1)
  NY2$resSAR = resid(ajusteSAR1)
  
  #Grafico para mais de uma variavel
  facets = c("resML","resSAR")
  
  tm_shape(NY2) + 
    tm_polygons(facets, 
                palette = "RdYlGn",
                midpoint = NA) +
    tm_facets(nrow = 1, sync = TRUE )
  
  
  #-------------------------------------------------------------------------------------#
  # ----------------------------- Ajustando um modelo CAR  -----------------------------#
  #-------------------------------------------------------------------------------------#
  
  #Ajustando um modelo SAR
  ajusteCAR1 = spautolm(formula = Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data = NY@data, listw = W.Queen.pesoB, family = "CAR")
  
  ## spautolm - ajusta um modelo autorregressivo ou um modelo condicional
  #Argumentos:
  #formula - modelo a ser ajustado (indicando a variavel resposta e as variaveis explicativas)
  #data - data frame que contem as variaveis
  #family - o modelo a ser ajustado
  #listw - define a estrutura de vizinhanca
  
  summary(ajusteCAR1)
  
  #Verificando independencia dos residuos
  moran.test(x = residuals(ajusteCAR1),listw = W.Queen.pesoB)
  
  #Avaliando o AIC
  AIC(ajusteCAR1)
  
  #-------------------------------------------------------------------------------------#
  # ---------------------- Comparando diferentes estruturas no SAR   -------------------#
  #-------------------------------------------------------------------------------------#
  
  #Criterio Queen e Peso B
  ajusteSAR.QB = spautolm(formula = Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data = NY@data, listw = W.Queen.pesoB, family = "SAR")
  summary(ajusteSAR.QB)
  
  #Criterio Queen e Peso W
  ajusteSAR.QW = spautolm(formula = Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data = NY@data, listw = W.Queen.pesoW, family = "SAR")
  summary(ajusteSAR.QW)
  
  #Criterio 3 vizinhos e Peso W
  ajusteSAR.3VW = spautolm(formula = Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data = NY@data, listw = W.3viz.pesoW, family = "SAR")
  summary(ajusteSAR.QB)
  
  #Comparando os tres ajustes
  AIC(ajusteSAR.QB)
  AIC(ajusteSAR.QW)
  AIC(ajusteSAR.3VW)
  
  
  #-------------------------------------------------------------------------------------#
  # ------------------------- Cuidados no ajuste do modelo CAR   -----------------------#
  #-------------------------------------------------------------------------------------#
  
  #Criterio Queen e Peso W
  ajusteCAR.QW = spautolm(formula = Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data = NY@data, listw = W.Queen.pesoW, family = "CAR")
  summary(ajusteSAR.QW)
  
}