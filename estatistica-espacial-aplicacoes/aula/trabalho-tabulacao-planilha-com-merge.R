# Set diretorio
setwd("/home/rennis/rennisHD00/Projetos/Pessoal/R/rennis-fgv-bigdata")
setwd("./estatistica-espacial-aplicacoes")
getwd()

library(readxl)
library(dplyr)
library(dplyr, warn.conflicts = FALSE)
df_tabela3175 = read_xlsx('dataset/trabalho/tabela3175_populacao_raca_sexo_situacao_domiciliar.xlsx'
               , sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)

# Redefine nome das colunas para facilitar o tratamento
df_tabela3175 <- df_tabela3175 %>% rename(
                municipio = "Tabela 3175 - População residente, por cor ou raça, segundo a situação do domicílio, o sexo e a idade"
              , sexo = "...2"                                                                                                 
              , faixa_etaria = "...3"                                                                                                 
              , branco_urbano = "...4"                                                                                                 
              , branco_rural = "...5"                                                                                                 
              , negro_urbano = "...6"                                                                                                 
              , negro_rural = "...7"                                                                                                 
              , amarelo_urbano = "...8"                                                                                                 
              , amarelo_rural = "...9"                                                                                                 
              , pardo_urbano = "...10"                                                                                                
              , pardo_rural = "...11"                                                                                                
              , indigena_urbano = "...12"                                                                                                
              , indigena_rural = "...13"                                                                                                
              , nao_declarado_urbano = "...14"                                                                                                
              , nao_declarado_rural = "...15"                                                                                                
        ) 
# Check nomes
colnames(df_tabela3175)

# Remove linhas iniciais com cabeçalho agrupado
# Remove 5 primeiras linhas ja agrupadas no nome dos dados
df_tabela3175 = tail(df_tabela3175, -5)
nrow(df_tabela3175)
head(df_tabela3175)

# Variavel municipio agrupado apenas para amenizar o processamento do arquivo
df_municipio = na.omit(df_tabela3175$municipio, cols=municipio)
head(df_municipio)
df_sexo = na.omit(df_tabela3175$sexo, cols=sexo)
head(df_sexo)

# Variaveis Referencia
municipio_atual = NA
municipio_referencia = NA
sexo_atual = NA
sexo_referencia = NA
for(i in 1:nrow(df_tabela3175)) {
  municipio_atual = df_tabela3175[i,'municipio']
  if(is.na(df_tabela3175[i,1])) { df_tabela3175[i,1] <- municipio_referencia }
  municipio_referencia = df_tabela3175[i,'municipio']
  
  sexo_atual = df_tabela3175[i,'sexo']
  if(is.na(df_tabela3175[i,2])) { df_tabela3175[i,2] <- sexo_referencia }
  sexo_referencia = df_tabela3175[i,'sexo']
  
  # Opção por indice, tbm funciona
  # if(is.na(df[i,2])) { df[i,2] <- df[(i-1),2]  } 
}
rm(municipio_atual)
rm(municipio_referencia)
rm(sexo_atual)
rm(sexo_referencia)
rm(df_municipio)
rm(df_sexo)

head(df_tabela3175)
# save(df_tabela3175,file="dataset/df_tabela3175.Rda")
load("dataset/df_tabela3175.Rda")
)
head(df_tabela3175)
# Separa Municipio e UF
df_tabela3175 = df_tabela3175 %>%
  separate(municipio, c("municipio", "uf"), " \\(", extra = "merge")

# Remove caracter da UF
df_tabela3175$uf = gsub(")", "", df_tabela3175$uf)

# Remove a linha nula
df_tabela3175 = na.omit(df_tabela3175)


df_tabela3175 %>% 
  select(  municipio, uf, sexo, faixa_etaria, branco_urbano, branco_rural, negro_urbano, negro_rural
           , amarelo_urbano, amarelo_rural, pardo_urbano, pardo_rural, indigena_urbano, indigena_rural
           , nao_declarado_urbano, nao_declarado_rural)
          mutate(branco_urbano = case_when(branco_urbano == '-' ~ '0', TRUE  ~ branco_urbano)) %>%
          mutate(branco_rural = case_when(branco_rural == '-' ~ '0', TRUE  ~ branco_rural)) %>%
          mutate(negro_urbano = case_when(negro_urbano == '-' ~ '0', TRUE  ~ negro_urbano)) %>%
          mutate(negro_rural = case_when(negro_rural == '-' ~ '0', TRUE  ~ negro_rural)) %>%
          mutate(amarelo_urbano = case_when(amarelo_urbano == '-' ~ '0', TRUE  ~ amarelo_urbano)) %>%
          mutate(amarelo_rural = ifelse(amarelo_rural == "-",0,amarelo_rural)) %>%
          mutate(pardo_urbano = case_when(pardo_urbano == '-' ~ '0', TRUE  ~ pardo_urbano)) %>%
          mutate(pardo_rural = case_when(pardo_rural == '-' ~ '0', TRUE  ~ pardo_rural)) %>%
          mutate(indigena_urbano = case_when(indigena_urbano == '-' ~ '0', TRUE  ~ indigena_urbano)) %>%
          mutate(indigena_rural = case_when(indigena_rural == '-' ~ '0', TRUE  ~ indigena_rural)) %>%
          mutate(nao_declarado_urbano = case_when(nao_declarado_urbano == '-' ~ '0', TRUE  ~ nao_declarado_urbano)) %>%
          mutate(nao_declarado_rural = case_when(nao_declarado_rural == '-' ~ '0', TRUE  ~ nao_declarado_rural))

save(df_tabela3175,file="dataset/df_tabela3175v2.Rda")
