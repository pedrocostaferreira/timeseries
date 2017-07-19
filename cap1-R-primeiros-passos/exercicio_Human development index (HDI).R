###########################################################################################################
################################## Análise de Séries Temporais em R: um curso introdutório  ###############
################################### CAPÍTULO 1. INTRODUÇÃO AO R    ########################################
### ######## Autores: Anna Carolina BArros, Daiane Marcolino e Pedro Costa Ferreira  ######################
############# Exercício: Human development index (HDI).csv ################################################
###########################################################################################################

#### 1) Crie uma função que classifique os países (em uma coluna extra na tabela) em 2014 de acordo com a tabela:

idh<-read.csv2("Human development index (HDI).csv",dec=".",stringsAsFactors = FALSE)

summary(idh)

idh$classificacao<-NULL

for(i in 1:nrow(idh)){
  
  idh[i,"classificacao"]<-calculo_idh(idh[i,"Ano_2014"])
  
}

#### 2) Quantas observações tem a tabela?

## 188

#### 3) Qual país cresceu mais em relação à 2013?

idh$idh_evolucao<-(idh$Ano_2014-idh$Ano_2013)/idh$Ano_2013

maior_evolucao<-max(idh$idh_evolucao)

idh[which(idh$idh_evolucao==maior_evolucao),"Country"]

#### 4) Qual país caiu mais em relação à 2013?

menor_evolucao<-min(idh$idh_evolucao)

idh[which(idh$idh_evolucao==menor_evolucao),"Country"]

#### 6) Quantos países estão com classificação baixa? 

nrow(idh[which(idh$classificacao=="Baixo"),])

