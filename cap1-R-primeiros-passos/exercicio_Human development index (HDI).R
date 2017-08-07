###########################################################################################################
################################## Análise de Séries Temporais em R: um curso introdutório  ###############
################################### CAPÍTULO 1. INTRODUÇÃO AO R    ########################################
### ######## Autores: Anna Carolina BArros, Daiane Marcolino e Pedro Costa Ferreira  ######################
############# Exercício: Human development index (HDI).csv ################################################
###########################################################################################################

#### 1) Crie uma função que classifique os países (em uma coluna extra na tabela) em 2014 de acordo com a tabela:

# lendo a base
idh<-read.csv2("Human development index (HDI).csv",dec=".",stringsAsFactors = FALSE)
summary(idh)
idh$classificacao<-NULL

# criando a função que calcula o idh
calculo_idh<-function(IDH){
  
  if(IDH<= 0.534){
    classificacao<-"Baixo"
  } else if(IDH> 0.534 & IDH <= 0.710){
    
    classificacao<-"medio"
    
  }else if(IDH>0.710 & IDH <=0.796){
    
    classificacao<-"Alto"
    
  }else{
    
    classificacao<-"Muito Alto"
  }
  
  return(classificacao)
  
}


# calculando o idh
for(i in 1:nrow(idh)){
  
  idh[i,"classificacao"]<-calculo_idh(idh[i,"Ano_2014"])
  
}

idh


#### 2) Qual país cresceu mais em relação à 2013?

idh$idh_evolucao<-(idh$Ano_2014-idh$Ano_2013)/idh$Ano_2013

maior_evolucao<-max(idh$idh_evolucao)

idh[which(idh$idh_evolucao==maior_evolucao),"Country"]


#### 3) Qual país caiu mais em relação à 2013?

menor_evolucao<-min(idh$idh_evolucao)

idh[which(idh$idh_evolucao==menor_evolucao),"Country"]


#### 4) Quantos países estão com classificação baixa? 

nrow(idh[which(idh$classificacao=="Baixo"),])


#### 5) Qual é a posição do Brasil?

idh[which(idh$Country ==  " Brazil"),"HDI.Rank"]

