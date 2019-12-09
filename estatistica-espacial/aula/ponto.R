# Setando o diretorio de trabalho
setwd("/home/rennis/rennisHD00/Projetos/Pessoal/R/rennis-fgv-bigdata/estatistica-espacial")

## Especifico para maquinas Linux
if(!exists(".sp2owin", mode="function")) source("../utils/sp2owin.R")
#-------------------------------------------------------------------------------------#
# -------------------- Mapa com a localização exata de um evento  --------------------#
# -------------------- Mapa com a localização dos delitos em NY  ---------------------#
#-------------------------------------------------------------------------------------#

#Carregando o pacote rgdal, maptools e dplyr
library(rgdal)
library(maptools)
library(dplyr)

#Importando o shapefile de Nova York
NYShp <- readOGR("map/nyshape/nyshape.shp")

#Carregando o pacote readr
library(readr)

#Importando o arquivo com as localizações dos delitos em NY
NYPD = read_csv2("dataset/NYPD.csv")

NYPD

#Carregando o pacote spatstat
library(spatstat)

#Definindo o shapefile como uma janela onde os pontos serao plotados - 
# necessario para o uso do pacote spatstat
# NYO <- as.owin(NYShp)
NYO <- .sp2owin(NYShp)

#Plotando o shapefile
plot(NYO, axes = TRUE)

#Criando o padrao de pontos a ser plotado
NYppp = ppp(NYPD$Longitude, NYPD$Latitude, window=NYO)

#Plotando as localizacoes dos delitos
par(mar=c(0.5,0.5,1.5,0.5))
plot(NYppp, pch=21, cex=0.9, bg="red", main="Ocorrencias de crimes em Nova York")

#Identificando os pontos pelo tipo de crime
NYpppBairro = ppp(NYPD$Longitude, NYPD$Latitude, window=NYO, marks = NYPD$TYPE)
plot(NYpppBairro, pch=21, bg=c("blue","red","yellow","green"), main="Tipos de crime")

#identificando os pontos pelo dia da semana
NYpppDia = ppp(NYPD$Longitude, NYPD$Latitude, window=NYO, marks = NYPD$Day)
plot(NYpppDia, main="Dia da semana")

#Estimando o efeito de primeira ordem (intensidade) usando diferentes kernels
NYkde.q = density.ppp(x = NYppp, sigma=0.01, kernel="quartic")
NYkde.g = density.ppp(x = NYppp, sigma=0.01, kernel="gaussian")
NYkde.e = density.ppp(x = NYppp, sigma=0.01, kernel="epanechnikov")

##density.ppp - calcula a funcao de intensidade de acordo com o kernel escolhido
#Argumentos:
#x - objeto da classe ppp
#sigma - é o valor do raio (tau na expressao dos slides)
#kernel - o kernel que deseja-se usar

#Plotando os dados e as funcoes intensidades estimadas pelas diversas funcoes kernel
par(mfrow=c(2,2))
plot(NYppp, pch=21, cex=0.9, bg="blue", main="Ocorrencias", cex.main=0.5)
plot(NYkde.q, main="Kernel Quartico", cex.main=0.5)
plot(NYkde.g, main="Kernel Normal")
plot(NYkde.e, main="Kernel Epanechnikov")
par(mfrow=c(1,1))

#Avaliando o impacto de diferentes raios (tau)

NYkde.tau1 = density.ppp(x = NYppp, sigma=0.005, kernel="gaussian")
NYkde.tau2 = density.ppp(x = NYppp, sigma=0.01, kernel="gaussian")
NYkde.tau3 = density.ppp(x = NYppp, sigma=0.02, kernel="gaussian")
NYkde.tau4 = density.ppp(x = NYppp, sigma=0.05, kernel="gaussian")
NYkde.tau5 = density.ppp(x = NYppp, sigma=0.5, kernel="gaussian")

par(mfrow=c(3,2))
plot(NYppp, pch=21, cex=0.9, bg="blue", main="Ocorrencias", cex.main=0.5)
plot(NYkde.tau1, main="Sigma=0.005", cex.main=0.5)
plot(NYkde.tau2, main="Sigma=0.01", cex.main=0.5)
plot(NYkde.tau3, main="Sigma=0.02", cex.main=0.5)
plot(NYkde.tau4, main="Sigma=0.05", cex.main=0.5)
plot(NYkde.tau5, main="Sigma=0.5", cex.main=0.5)
par(mfrow=c(1,1))

#-------------------------------------------------------------------------------------#
#Funcao que estima o raio por meio de validacao cruzada (custosa computacionalmente)
#-------------------------------------------------------------------------------------#

#raio.est = bw.diggle(NYppp)
#raio.est
#plot(raio.est)


#Carregando o pacote ggmap
library(ggmap)
#Definindo a API

register_google(key="AIzaSyA6n4J6vbGg1D76ZiNFnhmSeLH8es_p4y8")

#Plotando o grafico com recursos do Google Maps
nyhyb = get_googlemap('New York City',zoom=10,maptype='hybrid')
ggmap(nyhyb)

#Criando o grafico com a densidade e o layout do Google Maps
google = ggmap(nyhyb) + stat_density2d(aes(x=Longitude,y=Latitude, fill = ..level..), alpha = .8, h=.025, n = 400,geom = "polygon", data = NYPD) 
plot(google)

##stat_density2d - estima um kernel gaussiano
#Argumento:
#h - raio
#n - numero de pontos na grade
#alpha - opacidade

#Modificando a escala de cores
google + scale_fill_gradient(low = "pink", high= "black")

#Fazendo o plot da densidade considerando os diferentes tipos de crime
google + scale_fill_gradient(low = "black", high= "red") + facet_wrap(~ TYPE)

#-------------------------------------------------------------------------------------#
# Atividade
#-------------------------------------------------------------------------------------#

#1. Importem o shapefile e guardem num objeto chamado RIO
#2. Importem o conjunto de dados e guardem num objeto chamado Dengue
#3. Facam o tratamento necessario e guardem num objeto chamado RIOppp o shape com os pontos






























#--
#Importando o shapefile de Nova York
RIO <- readOGR("map/Lim_Mun/Lim_Mun.shp")
RIO = spTransform(RIO, CRS("+proj=longlat +datum=WGS84"))

#Carregando o pacote readr
library(readr)

#Importando o arquivo com as localizações dos delitos em NY
dengue = read_csv("dataset/riodengue.csv")

#Carregando o pacote spatstat
library(spatstat)

#Definindo o shapefile como uma janela onde os pontos serao plotados - necessario para o uso do pacote spatstat
# RIO <- as.owin(RIO)
RIO <- .sp2owin(RIO)

#Plotando o shapefile
plot(RIO, axes = TRUE)

#Criando o padrao de pontos a ser plotado
RIOppp = ppp(dengue$rio.Long_WGS84, dengue$rio.Lat_WGS84, window=RIO)

plot(NYppp)



#--

#Estimando a funcao G
RIO.G = Gest(RIOppp)

#Gest - estima a funcao de distribuicao G de um padrao de pontos
#Argumento
#X - um objeto da classe ppp

#Estimando a funcao K
RIO.K = Kest(RIOppp)

#Kest - estima a funcao K de Ripley de um padrao de pontos
#Argumento
#X - um objeto da classe ppp

#Estimando a funcao F
RIO.F = Fest(RIOppp)

#Fest - estima a funcao F de um padrao de pontos
#Argumento
#X - um objeto da classe ppp

#Plotando a funcao G
plot(RIO.G, main="Funcao G")


#Plotando as funcoes G, K e F
par(mfrow = c(2,2))
par(mar=c(2.5,2.5,1.5,.5))
plot(RIO.G, cbind(km, theo) ~ theo, main="Funcao G")
plot(RIO.K, cbind(iso, theo) ~ theo, main="Funcao K")
plot(RIO.F, cbind(km, theo) ~ theo, main="Funcao F")
par(mfrow = c(1,1))

#-------------------------------------------------------------------------------------#
# ----------------------- Checando se e razoavel assumir CSR   -----------------------#
#-------------------------------------------------------------------------------------#

#Dividindo o espaco em regioes e contando a quantidade de ocorrencias
cont = quadratcount(X = RIOppp, nx = 2, ny = 3)

##quadratcount - dividi uma janela em retangulos e conta o numero de pontos em cada um deles
#Argumentos:
#X - objeto do tipo ppp
#nx - numero de particoes no eixo x
#nx - numero de particoes no eixo y

#Visualizando as contagens em cada celula
cont

#Plotando as contagens
par(mfrow=c(1,2))
par(mar=c(0.5,0.5,1.5,1))
plot(RIOppp, pch=21, cex=0.5, bg="green")
plot(cont)
par(mfrow=c(1,1))


#Testando aleatoriedade espacial completa (H1: padrao nao apresenta CSR)
teste = quadrat.test(X = RIOppp, nx = 2, ny = 3)

##quadrat.test - realiza um teste de qui-quadrado para testar completa aleatoriedade espacial
#Argumentos:
#X - objeto do tipo ppp
#nx - numero de particoes no eixo x
#nx - numero de particoes no eixo y

#Visualizando o resultado do teste
teste

#Conclusao?!

#Testando aleatoriedade espacial completa (H1: padrao de pontos apresenta agrupamento)
teste_agrupamento = quadrat.test(X = RIOppp, nx = 2, ny = 3, alternative = "clustered")
teste_agrupamento

#Conclusao?!

#Testando aleatoriedade espacial completa (H1: padrao de pontos apresenta regularidade)
teste_regularidade = quadrat.test(X = RIOppp, nx = 2, ny = 3, alternative = "regular")
teste_regularidade

#Conclusao?!

#Realizando o teste de Clark-Evans para verificar nao aleatoriedade espacial completa
clarkevans.test(RIOppp)

#Realizando o teste de Clark-Evans para verificar agregacao espacial
clarkevans.test(RIOppp, alternative = "less")

#Realizando o teste de Clark-Evans para verificar regularidade espacial
clarkevans.test(RIOppp, alternative = "greater")


#Funcoes para estimar os envelopes das funcoes F, G e K
Env_Kest = envelope(RIOppp, fun = Kest, nsim=10) #alto custo computacional
Env_Gest = envelope(RIOppp, fun = Gest, nsim=10)
Env_Fest = envelope(RIOppp, fun = Fest, nsim=10)

#Plotando as funcoes e seus respectivos envelopes
par(mfrow=c(2,2))
plot(RIOppp, pch=21, cex=0.9, bg="blue")
  plot(Env_Kest, main = "Envelope K")
  plot(Env_Gest, main = "Envelope G")
  plot(Env_Fest, main = "Envelope F")
par(mfrow=c(1,1))









