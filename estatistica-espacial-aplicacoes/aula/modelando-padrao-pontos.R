#-------------------------------------------------------------------------------------#
# -------------------------- Modelando um padrao de Pontos  --------------------------#
#-------------------------------------------------------------------------------------#

#Não esqueca de direionar o R para a sua pasta de trabalho antes de comecar a atividade.

#Carregando os pacotes necessarios
library(rgdal)
library(maptools)
library(spatstat)
library(MASS)
library(tidyverse)
library(raster)

#-------------------------------------------------------------------------------------#
# ------------------------ Importacao dos dados e do shape  --------------------------#
#-------------------------------------------------------------------------------------#

#Importando a base de dados de incendios da regiao de Castilla-La Mancha
incendios = 

#Importando o shapefile de Castilla-La Mancha
Castilla <- 

#Definindo o shapefile como uma janela onde os pontos serao plotados - necessario para o uso do pacote spatstat
CastillaO <- as.owin(Castilla)

#Criando o padrao de pontos a ser plotado
Castillappp = ppp(x = , y = , window = )

##ppp - cria um objeto da classe ppp
#Argumentos:
#x - vetor de coordenadas x das localizacoes
#y - vetor de coordenadas y das localizacoes
#window - um objeto da classe owin

#Plotando as localizacoes dos delitos
par(mar=c(0.5,0.5,1.5,0.5))
plot(Castillappp, pch=21, cex=0.5, bg="red", main="Locias de focos de incendios em Castilla-La Mancha")

#-------------------------------------------------------------------------------------#
# ------------------------------- Analise exploratoria  ------------------------------#
#-------------------------------------------------------------------------------------#

#Estime o efeito de primeira ordem (de uma unica maneira)
## argumentos que precisam ser definidos
## 1 - o objeto ppp a ser usado
## 2 - o valor do raio (usem o raio  = 7)
## 3 - a funcao kernel a ser utilizada - exemplo: "quartic", "gaussian", "disc", "epanechinikov" etc.

Castillade = density.ppp(x = , sigma = , kernel = )

#Plote o padrao de pontos e a funcao de intensidade estimada
par(mfrow=c(1,2))
par(mar=c(0.5,0.5,1.5,1.5))
plot(Castillappp, pch=21, cex=0.5, bg="red", main="Ocorrencias", cex.main=0.5)
plot(Castillade, main="kernel", cex.main=0.5)
par(mfrow=c(1,1))

#Estime o efeito de segunda ordem (faca uma analise grafica e um teste de hipoteses)

## argumentos que precisam ser definidos
## 1 - o objeto ppp a ser usado

F.estimado = Fest(X = )

#Plotandl a funcao F
par(mar = c(3,3,1.5,.5))
plot(F.estimado)

#Conclusao???

## argumentos que precisam ser definidos
## 1 - o objeto ppp a ser usado
## 2 - a hipotese alternativa do teste que deseja realizar (faca o teste H0: o padrao e CSR x H1: o padrao de pontos e de agrupamento)

clarkevans.test(X = , alternative = " ")

#Conclusao???


#-------------------------------------------------------------------------------------#
# -------------------- Ajustando o processo de Poisson homogeneo   -------------------#
#-------------------------------------------------------------------------------------#

#Ajustando o processo de Poisson homogeneo
ajuste1 = ppm(Q = Castillappp ~ 1)

##ppm - Ajusta o processo de Poisson
#Argumentos:
#Q - Objeto ppp ~ 1 (ajusta o homogeneo)

#Visualizando o modelo ajustado
ajuste1

#Numero de pontos observados
Castillappp$n

#Area do espaco de interesse
spatstat::area(Castillappp)

#estimativa da intensidade
Castillappp$n/spatstat::area(Castillappp)

#-------------------------------------------------------------------------------------#
# ------------------ Ajustando o processo de Poisson nao-homogeneo   -----------------#
#-------------------------------------------------------------------------------------#

#Ajustando um processo de Poisson nao-homogeneo que e log-linear na coordenada cartesiana y
ajuste2 = ppm(Q = Castillappp ~ y)

##ppm - Ajusta o processo de Poisson
#Argumentos:
#Q - Objeto ppp ~ covarivaies (ajusta o processo nao-homogeneo)

#Visualizando o modelo ajustado
ajuste2

#Calculando o efeito de uma unica variavel no modelo
par(mar=c(4,4.5,2,.5))
plot(effectfun(model =ajuste2, covname ="y", se.fit = TRUE), main = "")

##effectfun - realiza um teste de qui-quadrado para testar completa aleatoriedade espacial
#Argumentos:
#model - modelo ajustado
#covname - nome da variavel que quer avaliar o efeito

#Plotando a funcao de intensidade do modelo ajustado
par(mar=c(1.5,1.5,1.5,1))
plot(ajuste2, how = "image", se = FALSE, pause = FALSE, axes = TRUE)

#-------------------------------------------------------------------------------------#
# ----------------- Testando a inclusao de uma covariavel no modelo   ----------------#
#-------------------------------------------------------------------------------------#

anova(ajuste1, ajuste2, test = "LRT")

#A variavel y deve ser incluida?


#-------------------------------------------------------------------------------------#
# ---------------------- Considerando funcoes das coordenadas ------------------------#
#-------------------------------------------------------------------------------------#


#Podemos considerar funcoes da coordenadas
ajuste3 = ppm(Q = Castillappp ~ y + I(x^2))

ajuste3

#Interpretacoes?

#-------------------------------------------------------------------------------------#
# ----------------- Incluindo covariaveis diferente das coordenadas ------------------#
#-------------------------------------------------------------------------------------#

#--------------------------#
#  covariaveis em imagens  #
#--------------------------#

#Importando a imagem da elevacao
elevacao = raster(x = "Elevacao.img")

##raster - importa uma imagem
#Argumentos:
#x - objeto a ser importado

#convertendo um objeto em uma imagem de pixel
elevacaoIM = as.im(elevacao)

#visualizando a imagem pixel
par(mar = c(1,1,2,2))
plot(elevacaoIM)

#Os objetos dentro de pop
names(elevacaoIM)

#Ajustando um modelo
ajuste4 = ppm(Q = Castillappp ~ x + y + elevacaoIM)

ajuste4

#---------------------------------------#
#  Obtendo intervalo de confianca       #
#---------------------------------------#
confint(ajuste4, level = 0.95)

#---------------------------------------#
#  Verificando multicolineariedade      #
#---------------------------------------#
co = vcov(ajuste4, what="corr")
round(co, 2)

#Ajustando um modelo cuja origem e o centro da regiao
ajuste4_2 = update(ajuste4, . ~ I(x-210) + I(y-200))
ajuste4_2


co2 = vcov(ajuste4_2, what="corr")
round(co2, 2)

#Plotando a funcao de intensidade do modelo ajustado
par(mar=c(1.5,1.5,1.5,1))
plot(ajuste4_2, how = "image", se = FALSE, pause = FALSE, axes = TRUE)

#Gráfico de residuos do modelo
residuos = residuals.ppm(ajuste4_2)
residuos
plot(residuos)

#--------------------------#
#  Selecao de modelos      #
#--------------------------#

#Ajustando um modelo
ajuste5 = ppm(Q = Castillappp ~ polynom(x,2) + polynom(y,2) + elevacaoIM)

ajuste5

#Comparando o valor do AIC de dois modelos
AIC(ajuste1)
AIC(ajuste5)

#Selecionando o melhor subconjunto de variaveis
stepAIC(ajuste5)

#---------------------------#
#  Como tratar com offsets  #
#---------------------------#




#-------------------------------------------------------------------------------------#
# ----------------- Calculando riscos relativos entre duas localidades ---------------#
#-------------------------------------------------------------------------------------#

#Calculando o risco relativo s_1 versus s_2

#s_1 (151,127) - posicao no objeto elevacaoIM
s1 = c(251.875,299.875) 
#s_2 (61,127) - posicao no objeto elevacaoIM
s2 = c(251.875,119.875)

plot(Castillappp, pch=21, cex=0.3, bg="red")
points(s1[1],s1[2], pch = 20,col="green", cex=2)
points(s2[1],s2[2], pch = 20,col="red", cex=2)

RR.num = exp(-2.6427 - 0.0023 * s1[1] + 0.0034 * s1[2] + 0.00021 * elevacaoIM$v[151,127])
RR.den = exp(-2.6427 - 0.0023 * s2[1] + 0.0034 * s2[2] + 0.00021 * elevacaoIM$v[61,127])

RR = RR.num/RR.den

RR

#-------------------------------------------------------------------------------------#
# -------------------------- Modelando processos com marccas -------------------------#
#-------------------------------------------------------------------------------------#


#Criando o padrao de pontos com marcas
Castillappp.mark = ppp(x = incendios$x, y = incendios$y, window = CastillaO, marks = factor(incendios$causa))

#Visualizando o processo marcado pelo tipo
plot(Castillappp.mark)

#Ajustando o processo marcado
ajuste6 = ppm(Castillappp.mark ~ marks + elevacaoIM)

ajuste6

#Plotando a funcao de intensidade do modelo ajustado
par(mar=c(1.5,1.5,1.5,1))
plot(ajuste5, how = "image", se = FALSE, pause = FALSE, axes = TRUE)

#-------------------------------------------------------------------------------------#
# --------------------------- Regressao logistica Espacial ---------------------------#
#-------------------------------------------------------------------------------------#

fit.model = slrm(Castillappp ~ x + y + elevacaoIM, eps = 1)
fit.model

confint(fit.model)

