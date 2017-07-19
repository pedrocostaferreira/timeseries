

#1ª Lista de Exercícios

#a) Defina o seu diretório de trabalho para o local onde se encontra a base de dados.

setwd("caminho_do_diretorio")

#b) Importe a base de dados base\_ipea.csv para o R.


base <- read.csv2("base_ipea.csv")




#c) A base é composta por quantas linhas e colunas?


dimensao <- dim(base)
dimensao


#d) Calcule a frequência de pessoas em cada região do Brasil. 


table(base[,1])

#e) Qual é a região mais frequente (moda)?


paste0(names(which(table(base[,1])==max(table(base[,1])))),": ",
       max(table(base[,1])))


#f) Qual é a idade da pessoa mais nova nessa amostra? E da mais velha?


paste0("Mínimo ",min(base[,2]))
paste0("Máximo ",max(base[,2]))

#g) Calcule a média, a mediana e a moda para a variável idade. A partir disso, o que você
# pode dizer sobre a distribuição dessa variável (assimétrica positiva, assimétrica negativa
# ou simétrica)?


paste0("média ",mean(base[,2]))
paste0("mediana " ,median(base[,2]))
paste0("moda ",names(which(table(base[,2])==max(table(base[,2])))))


#   como: moda < mediana < media conclui-se que a distribuição é assimétrica positiva




#h) Classifique as idades de acordo com as faixas etárias a seguir. (Crie uma nova coluna
# no data frame para essa classificação). A amostra é composta de mais Jovens, Adultos ou
# Idosos?


base$faixa <- 0
n <- dimensao[1]

for(i in 1:n){
  ifelse(base[i,2] <= 29, base[i,41] <- "Jovens",
         ifelse(base[i,2] <= 59, base[i,41] <- "Adultos", base[i,41]
                <- "Idosos"))
}
table(base$faixa)

# adultos



#i) Calcule a média, a mediana, o primeiro quartil, o terceiro quartil e os valores máximo
# e mínimo para a variável renda total de todos os moradores, parentes e agregados no último
# mês. Comente os resultados.


summary(base[,10])



#j)  Interprete o primeiro e o terceiro quartis encontrados no item anterior.



#k)  Crie uma função que calcule o coeficiente de variação.


cv <- function(x){
  desvio <- sd(x, na.rm = T)
  media <- mean(x, na.rm = T)
  coef <- desvio/media
  return(coef)
}



#l)  Calcule o coeficiente de variação para a variável idade e renda. 
# Compare os dois coeficientes de variação.

cv(base[,2])
cv(base[,10])


#m)  Calcule o desvio-padrão para a renda de acordo com cada região do Brasil. 
# Qual é a região que possui um comportamento mais homogêneo em relação à
# renda?

tapply(base[,10], base[,1], sd, na.rm = T)
tapply(base[,10], base[,1], cv)
paste0(names(which(tapply(base[,10], base[,1], cv)==min(tapply(
  base[,10],base[,1], cv)))),": ",min(tapply(base[,10], base[,1], cv)))

