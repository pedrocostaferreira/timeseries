# install.packages("prob")

#3ª Lista de Exercícios

#Definindo o diretório de trabalho para o local onde se encontra a base de dados.

setwd(dir="caminho_do_diretorio")

require(prob)
dado <- rolldie(times = 2, makespace = T)
head(dado) 


Prob(dado, X1 == 2) + Prob(dado, X1 == 4) + Prob(dado, X1 == 6)


Prob(dado, X1 > 4 & X2 < 3)

Prob(dado, X1 + X2 > 10)

cartas <- cards(makespace = T)
head(cartas)

Prob(cartas, suit == "Spade", rank == "A")

Prob(cartas, rank == "J", suit == "Heart")



dados <- read.delim("ex_con.txt")
tab <- table(dados[,2:3])           # tabela de frequência
tab <- as.data.frame(tab)           # tranformar em data.frame 
tab


sele_fem <- tab$genero == "feminino"   # selecionar genero
tab[sele_fem, ]
sele_fem_1 <- sele_fem & tab$fumante == 0  # selecionar se é ou não fumante do genero
tab[sele_fem_1, ]
P_fem <- tab$Freq[sele_fem_1] / sum(tab$Freq[sele_fem]) 
P_fem

n <- 10 
suce_1 <- array(0,n) 
for(cont in 1:n) { 
  resp <- sample(1:length(dados$respond), 1) 
  if(dados$fumante[resp] == 1) 
    suce_1[cont] <- 1 
} 
P_1 <- mean(suce_1) 
P_1


S <- c("cara", "coroa")
set.seed(2)
simula <- sample(S, size = 1000, replace = TRUE)
suce_cara <- simula == "cara" 
P_cara <- mean(suce_cara) 
P_cara

S <- c("B", "B", "B", "B", "B", "V", "V", "V", "V", "V", "V", "V")
set.seed(3)
simula <- sample(S, size = 1000, replace = TRUE)
suce_B <- simula == "B" 
P_B <- mean(suce_B) 
P_B

portas <- c("A", "B", "C")
x <- c()
n <- 1000
for (i in 1:n) {
  premio <- sample(portas)[1]
  escolha <- sample(portas)[1]
  abrir <-  sample(portas[which(portas != escolha & portas != premio)])[1]
  se_trocar <- portas[which(portas != escolha & portas != abrir)]
  if (escolha == premio) {x = c(x, "Não trocar")}
  if (se_trocar == premio) {x = c(x, "Trocar")}
}
length(which(x == "Trocar")) / n
length(which(x == "Não trocar")) / n