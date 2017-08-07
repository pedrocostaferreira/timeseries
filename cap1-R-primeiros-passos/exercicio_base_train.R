###########################################################################################################
################################## Análise de Séries Temporais em R: um curso introdutório  ###############
################################### CAPÍTULO 1. INTRODUÇÃO AO R    ########################################
############ Autores: Anna Carolina BArros, Daiane Marcolino e Pedro Costa Ferreira  ######################
############# Exercício: base_train.csv####################################################################
###########################################################################################################

#Defina o seu diretório de trabalho para o local onde se encontra a base de dados.

setwd("caminho_do_diretorio")

# Importe a base de dados base\.csv para o R.

titanic <- read.csv("train.csv")


# 1. Quantas variáveis possui o arquivo? Quantas observações o arquivo tem?

#variáveis
ncol(titanic)

#observações
nrow(titanic)


# 2. Quais são as classes das variáveis?

str(titanic)


# 3. Qual é a média das dos preços dos tickets?

media<-mean(titanic$Fare)


# 4. Faça um filtro na tabela e crie dois outros data frames. Um para o gênero masculino e o outro para o gênero feminino.

homens<-titanic[which(titanic$Sex=="male"),]
homens

mulheres<-titanic[which(titanic$Sex=="female"),]
mulheres


# 5. Crie duas listas uma para informações do data frame do gênero feminino e outro para o genero masculino. Cada lista deve ser composta:
    # ==> Número total de Passageiros
    # ==> Número de Sobreviventes
    # ==> Numero de passageiros na primeira classe
    # ==> preço médio Preço do ticket
    # ==> numero de parentesnfilhos

#lista homens

total_passageiros<-nrow(homens)
sobreviventes<-nrow(homens[which(homens$Survived==1),])
primeira_classe<-nrow(homens[which(homens$Pclass==1),])
preco_ticket<-mean(homens$Fare)
parentes<-(sum(homens$SibSp)/nrow(homens))+(sum(homens$parch)/nrow(homens))

lista_homens<-list(total_passageiros=total_passageiros,sobreviventes=sobreviventes,
                   primeira_classe=primeira_classe,
                   preco_ticket=preco_ticket,parentes=parentes)
lista_homens

#lista mulheres

total_passageiros<-nrow(mulheres)
sobreviventes<-nrow(mulheres[which(mulheres$Survived==1),])
primeira_classe<-nrow(mulheres[which(mulheres$Pclass==1),])
preco_ticket<-mean(mulheres$Fare)
parentes<-(sum(mulheres$SibSp)/nrow(mulheres))+(sum(mulheres$parch)/nrow(mulheres))

lista_mulheres<-list(total_passageiros=total_passageiros,sobreviventes=sobreviventes,
                     primeira_classe=primeira_classe,
                     preco_ticket=preco_ticket,parentes=parentes)
lista_mulheres

# 6. Qual gênero teve o maior número de pessoas embarcadas?

ifelse(lista_mulheres$total_passageiros>lista_homens$total_passageiros,
       "Mulhereses embarcaram mais","Homens embarcaram mais")


# 7. Qual genero sobreviveu mais?

ifelse(lista_mulheres$sobreviventes>lista_homens$sobreviventes,
       "Mulheres sobreviveram mais","Homens sobreviveram mais")


# 8. Qual genero teve a maior média do número de parentes?

ifelse(lista_mulheres$parentes>lista_homens$parentes,
       "Mulheres tiveram mais parentes","Homens tiveram mais parentes")
