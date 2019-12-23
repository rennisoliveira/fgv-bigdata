#Importando as bases de dados
library(tidyverse)
library(sf)
library(tmap)
library(rgdal)
library(ggmap)
library(units)

#######################################################
# 1)
#######################################################

#Importando os dados
tree = read_csv("NY_arvore.csv")

#Importando o shape
NYShp = readOGR("nyshape.shp")

#Fazendo mapa de pontos usando o qmplot
graf1 = qmplot(x = longitude, 
       y = latitude, 
       data = tree, 
       color = I("red"), 
       size = I(.5), 
       alpha = I(.5)) 

graf1

#Separando o mapa pela qualidade da saude das arvores
graf1 + facet_wrap(~health)

#######################################################
# 2)
#######################################################

# Convertendo a base de dados original em um objeto sf
tree_sf <- tree %>% 
  st_as_sf(
    coords = c("x_sp", "y_sp"), #trabalhando com as coordenadas UTM
    crs = 2263, # CRS referente a NY
    agr = "constant",
    stringsAsFactors = FALSE,
    remove = TRUE
  )

#Transformando o shape em um objeto da classe sf
NYShp2 = as(NYShp, "sf")

#Checando as projecoes
st_crs(tree_sf) #CRS: 2263
st_crs(NYShp2) #CRS: 4326

#Mudando a projecao do shape para 2263
NYShp2 = st_transform(NYShp2, crs = 2263)

# Juntando os pontos com os poligonos (subregioes)
tree_join <- st_join(tree_sf, NYShp2, join=st_within)

# Contando o numero de casos por poligono (subregiao)
tree_contagem <- count(as_tibble(tree_join), boro_cd) %>%
  print()

#Acrescentando os dados no shape e criando as variaveis area por km^2 e a taxa
NYShp_dados = left_join(NYShp2, tree_contagem, by = "boro_cd") %>% 
  mutate(area = set_units(st_area(.), km^2),
         taxa = n/area)

#Definindo o mapa
tm_shape(NYShp_dados) + 
  tm_fill(col = "taxa",
          palette = "Greens",
          title = "Arvores por km^2",
          id = "taxa")


#Definindo o mapa
tmap_mode("view")

tm_shape(NYShp_dados) + 
  tm_fill(col = "taxa",
          palette = "Greens",
          title = "Arvores por km^2",
          id = "taxa")


