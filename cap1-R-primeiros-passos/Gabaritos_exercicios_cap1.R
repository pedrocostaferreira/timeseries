###########################################################################################################
################################## Análise de Séries Temporais em R: um curso introdutório  ###############
################################### CAPÍTULO 1. INTRODUÇÃO AO R    ########################################
############ Autores: Anna Carolina BArros, Daiane Marcolino e Pedro Costa Ferreira  ######################
###########################################################################################################


###################
## exercicios 1.4.1/pags -16/17
####################


# Crie um vetor V = [105;106;150;135;120;147]:

v = c(105,106,150,135,120,147)

# (a) Concatene com o vetor v2 = [105;250;300;175;157;147;134].

v2 = c(105,250,300, 175,157,147,134)

v3=c(v,v2)

# (b) Remova as duplicatas.

v3=unique(v3)


#(c) Qual é o terceiro elemento?

v3[3]

#(d) Qual é o o tamanho do vetor?
length(v3)

#(e) Qual é a média dos valores?
media = mean(v3)

#(f) Qual é o maior elemento?

maior= max(v3)


###################
## exercicios 1.5.1/pag -21
####################

#Crie uma tabela, chamada de tabela_pessoas e responda as questões:

nome<-c("Paulo",	"Anna","Pedro",	"Viviane",	"Ricardo",	"Diego",	"Marcos","Renata","Victor","Bruno",	"Juliana",	"Adriana",	"Juliana",	"Beatriz","Vanessa",	"Ingrid","Mariana")
genero<-c(1	,2	,1	,2,	1	,1,	1,	2	,1	,1,	2,	2,	2,	2,	2	,1,2)
regiao<-c("Região 2",	"Região 3",	"Região 3",	"Região 2",	"Região 3","Região 5",	"Região 2",	"Região 5",	"Região 5",	"Região 1","Região 1","Região 1","Região 2","Região 1",	"Região 2",	"Região 3",	"Região 3")
idade<-c(36,32,	30,	32,	31,	29,	35,	33,	25,	27,	27,	19,	31,	22,	36,	34	,33)

tabelas_pessoas<-data.frame(nome,genero,regiao,idade)
tabelas_pessoas

#1. Quantas observações tem a tabela?

observacoes<-nrow(tabelas_pessoas)
observacoes

#2. Quais são as médias das idades?
media_idade<-mean(tabelas_pessoas$idade)
media_idade

#3. Quais são as classes de cada uma das colunas?
str(tabelas_pessoas)

# 4.Faça um resumo da tabela.
summary(tabelas_pessoas)

###################
## exercicios 1.7.1/pag -27
####################

#Tome os vetores: nomes = [Anna, Paula,Roberta, Ingrid, fernanda, João],pesos = [52, 65, 70, 58, 48, 70] e
#alturas = [1.54, 1.76, 1.65, 1.60, 1.68, 1.70]. Criem uma lista com esses vetores chamado lista_pessoas.Depois
#crie um quarto objeto chamado IMC de acordo coma equação, depois responda as questões:

nomes = c("Anna", "Paula","Roberta", "Ingrid", "fernanda", "João")
pesos = c(52, 65, 70, 58, 48, 70)
alturas = c(1.54, 1.76, 1.65, 1.60, 1.68, 1.70)

lista_pessoas<-list(nomes=nomes,pesos=pesos,alturas=alturas)
lista_pessoas

lista_pessoas$IMC<-lista_pessoas$pesos/(lista_pessoas$alturas**2)
lista_pessoas

#2. Qual é o primeiro elemento da lista?
lista_pessoas[[1]]

#3. Quais são as classes dos objetos na lista?
str(lista_pessoas)

#4. Qual é o maior IMC?

maior_imc<-max(lista_pessoas$IMC)
maior_imc

###################
## exercicios 1.10.1/pag - 41
####################
#1. Crie uma função que calcule a área e o perímetro de um triângulo retângulo, com os as entradas:
#====>a. Cateto 1
#====>b. Cateto 2
#O output será uma lista com o primeiro ojeto sendo a àrea e o segundo sendo o perímetro.

triangulo<-function(c1,c2){
  
  h<-sqrt(c1**2+c2**2)
  area=c1*c2/2
  perimetro=c1+c2+h
  
  
  return(list(area=area,perimetro=perimetro))
  
}

#Exemplo: Aplicando a função em um triangulo com c1=4 e c2=3
triangulo(3,4)

