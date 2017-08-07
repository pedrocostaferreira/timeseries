###########################################################################################################
################################## Análise de Séries Temporais em R: um curso introdutório  ###############
################################### CAPÍTULO 1. INTRODUÇÃO AO R    ########################################
### ######## Autores: Anna Carolina BArros, Daiane Marcolino e Pedro Costa Ferreira  ######################
############# Exercício: base_ipea.csv#####################################################################
###########################################################################################################

# 1) Defina o seu diretório de trabalho para o local onde se encontra a base de dados.

setwd("caminho_do_diretorio")


# 2) Importe a base de dados base\_ipea.csv para o R.

base <- read.csv2("base_ipea.csv")


# 3) A base é composta por quantas linhas e colunas?

dimensao <- dim(base)
dimensao


# 4) Calcule a frequência de pessoas em cada região do Brasil. 

table(base[,1])


# 5) Qual é a região mais frequente (moda)?

paste0(names(which(table(base[,1])==max(table(base[,1])))),": ",
       max(table(base[,1])))


# 6) Qual é a idade da pessoa mais nova nessa amostra? E da mais velha?

paste0("Idade da pessoa mais nova: ",min(base[,2]))
paste0("Idade da pessoa mais velha: ",max(base[,2]))


# 7) Calcule a média, a mediana e a moda para a variável idade. A partir disso, o que você
#    pode dizer sobre a distribuição dessa variável (assimétrica positiva, assimétrica negativa
#    ou simétrica)?

paste0("Média: ",mean(base[,2]))
paste0("Mediana: " ,median(base[,2]))
paste0("Moda: ",names(which(table(base[,2])==max(table(base[,2])))))

#   como: moda < mediana < media conclui-se que a distribuição é assimétrica positiva


# 8) Classifique as idades de acordo com as faixas etárias a seguir. (Crie uma nova coluna
#    no data frame para essa classificação). A amostra é composta de mais Jovens, Adultos ou
#    Idosos?

base$faixa <- 0
n <- dimensao[1]

for(i in 1:n){
  ifelse(base[i,2] <= 29, base[i,41] <- "Jovens",
         ifelse(base[i,2] <= 59, base[i,41] <- "Adultos", base[i,41]
                <- "Idosos"))
}
table(base$faixa)

# É composta por mais adultos.


# 9) Calcule a média, a mediana, o primeiro quartil, o terceiro quartil e os valores máximo
#    e mínimo para a variável renda total de todos os moradores, parentes e agregados no último
#    mês. Comente os resultados.

summary(base[,10])


# 10)  Interprete o primeiro e o terceiro quartis encontrados no item anterior.

# Nota-se, através do primeiro quartil, que 25% dos moradores, parentes e agregados no último
# mês possuem a renda abaixo de 800 reais e através do terceiro quartil,
#  nota-se que 25% dos mesmos possuem a renda acima de 2150 reais. 


# 11)  Crie uma função que calcule o coeficiente de variação.

cv <- function(x){
  desvio <- sd(x, na.rm = T)
  media <- mean(x, na.rm = T)
  coef <- desvio/media
  return(coef)
}


# 12)  Calcule o coeficiente de variação para a variável idade e renda. 
#      Compare os dois coeficientes de variação.

cv(base[,2]) 
cv(base[,10]) 

# Como o coeficiente de variação para a idade foi menos que o coeficiente 
# de variação da renda, pode-se afirmar que os dados relativos à idade 
# são mais homogêneos que os dados da renda. 


# 13)  Calcule o desvio-padrão para a renda de acordo com cada região do Brasil. 
#      Qual é a região que possui um comportamento mais homogêneo em relação à
#      renda?

# desvio padrão
tapply(base[,10], base[,1], sd, na.rm = T)

# analisando o comportamento através do CV
tapply(base[,10], base[,1], cv)
paste0(names(which(tapply(base[,10], base[,1], cv)==min(tapply(
       base[,10],base[,1], cv)))),": ",min(tapply(base[,10], base[,1], cv)))
# Como o Cetro-Oeste possui o menor valor para CV, é considerada a região com o 
# comportamento mais homogêneo.


# 14) Histograma para a variável "renda total de todos os moradores,parentes e agregados
#     no último mês".

hist(base[,10], main = "Histograma para a renda total do domicílio", 
     xlab = "Renda total do domicílio")


# Assimétrica positiva


# 15) Boxplot para "número de moradores no domicílio, parentes e agregados". 

boxplot(base[,11], main = "Número de moradores no domicílio", 
        horizontal = T)

# mediana = 3, a partir de 7 moradores é outlier.


# 16) Gráfico de dispersão

plot(base[, 10], base[, 9], main = "Gráfico de Dispersão", 
     xlab = "Renda do domicílio", ylab = "Renda do chefe")

# É possível notar uma relação linear positiva, mesmo com os valores 
# bem dispersos.


# 17)  tabelas de frequência

sexo <- table(base[, 4])
sexo

religiao <- table(base[, 13])
religiao


# 18) Gráfico de pizza

pie(sexo, main ="Composição por sexo")

# A amostra é composta por mais mulheres. 


# 19) Gráfico de barras

barplot(religiao, main ="Composição por religião")

# A religião mais frequente na amostra é a católica.


# 20) Dois gráficos na mesma janela

par(mfrow = c(2, 1))
pie(sexo, main ="Composição por sexo")
barplot(religiao, main ="Composição por religião")

