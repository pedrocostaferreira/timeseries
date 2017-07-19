#2ª Lista de Exercícios

#Definir Diretório de Trabalho e Ler Base de Dados 

#Definindo o diretório de trabalho para o local onde se encontra a base de dados.

setwd(dir="caminho_do_diretorio")


#  Importe a base de dados base\_ipea.csv para o R.

base <- read.csv2("base_ipea.csv")





#1) Histograma para a variável "renda total de todos os moradores,parentes e agregados
# no último mês".


hist(base[,10], main = "Histograma para a renda total do domicílio", 
     xlab = "Renda total do domicílio")



#Assimétrica positiva



#2) Boxplot para "número de moradores no domicílio, parentes e agregados". 

boxplot(base[,11], main = "Número de moradores no domicílio", 
        horizontal = T)


#mediana = 3, a partir de 7 moradores é outlier.



#3) Gráfico de dispersão

plot(base[, 10], base[, 9], main = "Gráfico de Dispersão", 
     xlab = "Renda do domicílio", ylab = "Renda do chefe")


#4)  tabelas de frequência

sexo <- table(base[, 4])
religiao <- table(base[, 13])
sexo
religiao


#5) Gráfico de pizza

pie(sexo, main ="Composição por sexo")






#6) Gráfico de barras

barplot(religiao, main ="Composição por religião")


#7) Dois gráficos em uma janela

par(mfrow = c(2, 1))
pie(sexo, main ="Composição por sexo")
barplot(religiao, main ="Composição por religião")