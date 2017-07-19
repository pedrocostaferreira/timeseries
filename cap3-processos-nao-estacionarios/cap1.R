# seç -> seção;
# sub -> subseção


##------------------------------------------------------------------- seç2_sub1 -----------------------------------------------------------------##


#
5+2            # soma: +
5-2            # subtração: -
5*2            # multiplicação: *
5/2            # divisão: /
5^2            # potência: ^
5**2           # potência: **
sqrt(5)        # raiz quadrada
exp(5)         # exponencial
log(5,base = 2)# logaritmo
factorial(5)   # fatorial
choose(5,2)    # combinação de 5, 2 a 2

#
x <- 5  # objeto x recebe valor 10
x

#
(a <- 5 + 27^2)# cria o objeto a e o mostra em seguida

#
ls()# lista os objetos criados

#
rm(x)

#
a <- 1; b <- 6; c <- 9 # cria os objetos a,b,c
ls()                   # lista os objetos existentes
rm(b)                  # remove o objeto b
ls()                   # lista os objetos existentes

#
vetor1 <- c(1,2,5,6,7,25,0,-3) # vetor com números
vetor2 <- c("verde","amarelo") # vetor com textos
vetor1; vetor2

#
# sequência de 1 a 30
(a <- 1:30) 

# sequência de 1 a 35 de 2 em 2
(b <- seq(from = 1, to = 35, by = 2))

# sequência de tamanho 9
(c <- seq(length = 9, from = 1, to = 5))

rep(1,15)        # repete o nº1 15 vezes
sequence(3)      # gera a sequência 123 
sequence(c(3,5)) # junta as sequências 123 e 12345
sequence(1:5)    # junta cinco sequências

#
# criando uma matriz de zeros 3x3
A <- matrix(0, nrow = 3, ncol = 3) 
A


#
# preenchendo a matriz por linha
A[1,] <- c(1,2,3)
A[2,] <- c(7,8,9)
A[3,] <- c(-10,-4,-6)
A


#
# preenchendo a matriz por coluna
A[,1] <- c(1,2,3)
A[,2] <- c(7,8,9)
A[,3] <- c(-10,-4,-6)
A


#
# criando uma matriz preenchida
B <- matrix(c(1,2,3,8,9,10), nrow = 2, ncol=3, byrow = T)
B


#
# criando uma matriz preenchida
(B <- matrix(c(1,2,3,8,9,10), nrow = 2, ncol=3, byrow = F))

#
# criando um data frame
tabela <- data.frame(Nome = c("André","João","Tiago"),
                     Idade = c(20,58,22))
tabela


#
# adicionando uma nova coluna ao data frame
tabela$Salário <- c(1200,5700,800)
tabela


#
tabela <- data.frame(Nome = c("André","João","Tiago"), Idade = c(20,58,22))
tabela

#
tabela <- cbind(tabela, data.frame(Salario=c(1200,5700,800)))
tabela

#
tabela <- rbind(tabela, data.frame(Nome="Joana", Idade=30, Salario=3100))
tabela

#
# exemplo da função melt
install.packages("reshape")
library(reshape)
dados <- data.frame(id = c(1,1,2,2), tempo = c(1,2,1,2), x1 = c(5,3,6,2),
                    x2 = c(6,5,1,4))
mdados <- melt(dados, id=c("id","tempo"))
# cast(dados, formula, função) 
mediaid <- cast(mdados, id~variable, mean)
mediatempo <- cast(mdados, tempo~variable, mean)

#
# exemplo da função melt
library(reshape)
dados <- data.frame(id = c(1,1,2,2), tempo = c(1,2,1,2), x1 = c(5,3,6,2), x2 = c(6,5,1,4))
mdados <- melt(dados, id=c("id","tempo"))
# cast(dados, formula, função)
mediaid <- cast(mdados, id~variable, mean)
mediatempo <- cast(mdados, tempo~variable, mean)

#
dados
mdados
mediaid
mediatempo


#
# Dois objetos numéricos
a <- 2
b <- 5

# Estrutura de condição
if(a > b){
  cat("O maior é: ",a)
}else{
  cat("O maior é: ",b)
}


#
# data frame de salários de 10 indivíduos de uma empresa
dados <- data.frame( individuo = c("A", "B", "C", "D", "E", "F", "G", "H",
                                   "I", "J"),
                     salario = c( 1250, 800, 8500, 900, 2010, 2200, 3600, 7580, 5100, 9400))


#
dados


#
# criando um contador
contador <- 0

# estrutura FOR
for (i in 1:10){
  if(dados[i,2] < 3000){contador <- contador + 1}
}
contador


#
# nova coluna para preencher com a soma acumulada
dados$salario_acum <- 0
dados

# o salário acumulado 1 é o primeiro salário
dados[1,3] <- dados[1,2]
dados


#
# estrutura de repetição para as somas acumuladas
for (i in 2:10){
  dados[i,3] <- dados[i-1,3] + dados[i,2]
}
dados

#
# Criando uma função que calcula a área de um triângulo
area <- function(b,h){
  A <- (b * h)/2
  return(A)
}

# Área de um triângulo de base 5 e altura 3
area(5,3)
area(h = 3, b = 5)



##------------------------------------------------------------------ seç2_sub2 -----------------------------------------------------------------##

#
getwd()#visualizar diretório de trabalho

#
setwd("C:\\Users\\diego.vilela\\Desktop\\Book\\Latex\\Comandos") #altera o diretório
getwd()#visualizar diretório de trabalho

#
#salvar
savehistory(file = "nomedoarquivo.txt")
#carregar
loadhistory(file = "nomedoarquivo.txt") 

#
#salvar histórico em outro local
savehistory(file = "V:/SUEP/NMEC/nomedoarquivo.txt")

##------------------------------------------------------------------ seç2_sub3 -----------------------------------------------------------------##

#
# O pacote xlsx permite a importação de arquivos .xlsx
install.packages("xlsx")# instalar pacote 
require(xlsx)          # carregar pacote

##------------------------------------------------------------------ seç2_sub5 -----------------------------------------------------------------##

#
dados <- read.table("arquivo.txt", header = T,sep = "\t", dec = ".")

#
tabela1 <- read.xlsx("arquivo.xlsx", sheetName="Plan1")

##------------------------------------------------------------------ seç2_sub6 -----------------------------------------------------------------##

#
# criando um data frame de idades
idades <- data.frame(Nome = c("Tabi", "Gabi", "Andressa", "Vanessa",
                              "Natália", "Natasha"), 
                     Idades = c(36, 25, 33, 48, 21, 24))
idades

#
# salvando o objeto idades em formato .txt
write.table(idades, "idades.txt",  quote = F, sep = "\t", row.names = F,
            col.names = T)

##------------------------------------------------------------------ seç3 -----------------------------------------------------------------##

#
# Ler a base de dados da POF♦
dados <- read.csv2("POF_capitais.csv")

##------------------------------------------------------------------ seç3_sub1 -----------------------------------------------------------------##

#
# resumo estrutural para a base de dados da POF
str(dados)

#
# As duas formas abaixo mostram a segunda coluna do 
# objeto dados
dados$CÓDIGO.DA.UF
dados[ ,2]

#
#dimensão: número de linhas e colunas 
dim(dados)  

#
#nome de cada coluna
names(dados) 

##------------------------------------------------------------------ seç4_sub1 -----------------------------------------------------------------##

#
# Uso da função example para histogramas
example(hist)
