###########################################################################################################
######################## Análise de Séries Temporais em R: um curso introdutório  #########################
###########################################################################################################
### Capítulo 8 - Modelo Vetorial Autorregressivo                                                        ###  
### Autor: Pedro Costa Ferreira                                                                         ###
###########################################################################################################


# pacotes necessários ------------------------------------------------------------
library(vars)
library(urca)
library(dse)

# Seção 8.2 - O Modelo VAR -------------------------------------------------------
# Subseção 8.2 - Estabilidade e estacionariedade ---------------------------------
#
# SIMULACAO E ESTIMACAO DO VAR ----

# Definição dos parâmetros para gerar os ruídos, simular e plotar o sistema (8.13)

n <- 500
r <- 0.7
set.seed(1)
Z1 <- rnorm(n)
Z2 <- rnorm(n)

E1 <- Z1
E2 <- r*Z1 + sqrt(1-r^2)*Z2

A <- matrix(c(.7,.2,.4,.3),2,2)

y_1 = y_2 <- rep(0,n)
for(t in 2:n){
  y_1[t] <- A[1,1]*y_1[t-1] + A[1,2]*y_2[t-1] + E1[t]
  y_2[t] <- A[2,1]*y_1[t-1] + A[2,2]*y_2[t-1] + E2[t]
}

# Figura 8.1
plot(y_1, type = "l", lty = 1, ylim = c(-6,6), ann = FALSE)
lines(y_2,lty = 3, col = "darkgrey")


# A outra  matriz "A" abaixo gera o processo do sistema (8.14).
# A = matrix(c(1,0,0,1),2,2)
# y_1 = y_2 <- rep(0,n)
# for(t in 2:n){
#   y_1[t] <- A[1,1]*y_1[t-1] + A[1,2]*y_2[t-1] + E1[t]
#   y_2[t] <- A[2,1]*y_1[t-1] + A[2,2]*y_2[t-1] + E2[t]
# }
# 
# # Figura 8.2
# plot(y_1, type = "l", lty = 1, ylim = c(-15,20), ann = FALSE)
# lines(y_2,lty = 3, col = "darkgrey")

# Seção 8.3 - Estimação, análise e previsão -----------------------------

# Estruturação dados com como um objeto "ts" e ajuste em um modelo VAR.
data <- as.ts(cbind(y_1,y_2))

# subseção 8.3.1 - estimação --------------------------------------------
# Estimação da número de lags e do processo
VARselect(data, lag.max = 6, type = "none")
(model1 <- VAR(data, p = 1, type = "none"))

# subseção 8.3.1 - diagnóstico ------------------------------------------
serial.test(model1)
arch.test(model1)
vars::roots(model1)

# guardar autovalores do processo
autoval <- vars::roots(model1)

# Figura (8.3)
x <- seq(-1,1,length = 1000)
y1 <- sqrt(1-x^2)
y2 <- -sqrt(1-x^2)
plot(c(x,x),c(y1,y2),xlab='Parte Real',ylab='Parte Complexa',type='l',main='Círculo Unitário',ylim=c(-2,2),xlim=c(-2,2))
abline(h = 0)
abline(v = 0)
points(autoval, Im(autoval),pch=19)
legend(-2.0,-1.5,legend="Autovalores",pch=19)

# Subseção 8.3.3 Função impulso-resposta -------------------------------------
##### FIR, PREVISAO E DECOMPOSICAO ----

# Elaboração da Figura 8.4 

# resposta do choque em y1
model1.irf <- irf(model1, impulse = "y_1", n.ahead = 40, boot = TRUE)
# resposta do choque em y2
model2.irf <- irf(model1, impulse = "y_2", n.ahead = 40, boot = TRUE)

par(mfcol=c(2,2), mar=c(0,4,0,0), oma=c(5,3,5,3))

# Figura 8.4 (a)
plot.ts(model1.irf$irf$y_1[,1], axes=F, oma.multi = c(0,0,5,0),
        ylab='y_1', ylim=c(-0,1))
lines(model1.irf$Lower$y_1[,1], lty=2, col='red')
lines(model1.irf$Upper$y_1[,1], lty=2, col='red')
axis(side=2, las=2, ylab='')
abline(h=0, col='red')
box()
mtext("Response from y_1", line = 2)

plot.ts(model1.irf$irf$y_1[,2], axes=F, oma.multi = c(0,0,5,0),
        ylab='y_2', ylim=c(-0,1))
lines(model1.irf$Lower$y_1[,2], lty=2, col='red')
lines(model1.irf$Upper$y_1[,2], lty=2, col='red')
axis(side=1, las=1)
axis(side=2, las=2)
abline(h=0, col='red')
box()
mtext("95% Bootstrap CI, 100 runs", side=1, line = 3)

# Figura 8.4 (b)
plot.ts(model2.irf$irf$y_2[,1], axes=F, oma.multi = c(0,0,5,0),
        ylab='', ylim=c(-0,1))
lines(model2.irf$Lower$y_2[,1], lty=2, col='red')
lines(model2.irf$Upper$y_2[,1], lty=2, col='red')
axis(side=2, las=2, ylab='')
abline(h=0, col='red')
box()
mtext("Response from y_2", line = 2)

plot.ts(model2.irf$irf$y_2[,2], axes=F, oma.multi = c(0,0,5,0),
        ylab='', ylim=c(-0,1))
lines(model2.irf$Lower$y_2[,2], lty=2, col='red')
lines(model2.irf$Upper$y_2[,2], lty=2, col='red')
axis(side=1, las=1)
axis(side=2, las=2)
abline(h=0, col='red')
box()
mtext("95% Bootstrap CI, 100 runs", side=1, line = 3)


# Elaboração da Figura 8.5

# resposta do choque em y1
model1.irf <- irf(model1, impulse = "y_1", n.ahead = 40, boot = TRUE, cumulative = T)
# resposta do choque em y2
model2.irf <- irf(model1, impulse = "y_2", n.ahead = 40, boot = TRUE, cumulative = T)

par(mfcol=c(2,2), mar=c(0,4,0,0), oma=c(5,3,5,3))

# Figura 8.4 (a)
plot.ts(model1.irf$irf$y_1[,1], axes=F, oma.multi = c(0,0,5,0),
        ylab='y_1', ylim=c(-0,8))
lines(model1.irf$Lower$y_1[,1], lty=2, col='red')
lines(model1.irf$Upper$y_1[,1], lty=2, col='red')
axis(side=2, las=2, ylab='')
abline(h=0, col='red')
box()
mtext("Cumulative Response from y_1", line = 2)

plot.ts(model1.irf$irf$y_1[,2], axes=F, oma.multi = c(0,0,5,0),
        ylab='y_2', ylim=c(-0,8))
lines(model1.irf$Lower$y_1[,2], lty=2, col='red')
lines(model1.irf$Upper$y_1[,2], lty=2, col='red')
axis(side=1, las=1)
axis(side=2, las=2)
abline(h=0, col='red')
box()
mtext("95% Bootstrap CI, 100 runs", side=1, line = 3)

# Figura 8.5 (b)
plot.ts(model2.irf$irf$y_2[,1], axes=F, oma.multi = c(0,0,5,0),
        ylab='', ylim=c(-0,8))
lines(model2.irf$Lower$y_2[,1], lty=2, col='red')
lines(model2.irf$Upper$y_2[,1], lty=2, col='red')
axis(side=2, las=2, ylab='')
abline(h=0, col='red')
box()
mtext("Cumulative Response from y_2", line = 2)

plot.ts(model2.irf$irf$y_2[,2], axes=F, oma.multi = c(0,0,5,0),
        ylab='', ylim=c(-0,8))
lines(model2.irf$Lower$y_2[,2], lty=2, col='red')
lines(model2.irf$Upper$y_2[,2], lty=2, col='red')
axis(side=1, las=1)
axis(side=2, las=2)
abline(h=0, col='red')
box()
mtext("95% Bootstrap CI, 100 runs", side=1, line = 3)

# Subseção 8.3.4 - Decomposição da variância ---------------------------------

# Figura 8.6
fevd.model1 <- fevd(model1, n.ahead = 7)
plot(fevd.model1, main="")

# Subseção 8.3.5 - Previsões -------------------------------------------------

#  previsão a partir de 'fanchart'
model1.y_1 <- predict(model1, n.ahead = 10, ci = 0.95)
fanchart(model1.y_1, xlim = c(450,505), main = c("Fanchart de y_1", "Fanchart de y_2"))

# Subseção 8.3.6 - Causalidade de Granger ------------------------------------
causality(model1)$Granger

# Seção 8.4 - VAR estrutural (SVAR) ------------------------------------------

# Subseção 8.4.2 - Exemplo ---------------------------------------------------

# criação do polinômio de defasagem
Apoly <- array(c(1.0,-0.5,0.3,0.8,
                 0.2,0.1,-0.7,-0.2,
                 0.7,1,0.5,-0.3),
               dim = c(3,2,2))
B <- diag(2)
Apoly
B

# estruturação do modelo de acordo com A e B
svarA <- ARMA(A=Apoly, B=B)

# simulação das observações
svarsim <- simulate(svarA, sampleT = 500,
                  rng = list(seed = c(123456)))

# reformatando observações em matriz
svardat <- matrix(svarsim$output, nrow = 500, ncol = 2)
colnames(svardat) <- c("y1","y2")

## estimando VAR para observações criadas
varest <- VAR(svardat, p=2, type="none")

# definindo matriz de restrições da especificação do VAR Estrutural
Amat <- diag(2)
Amat[2,1] <- NA
Amat[1,2] <- NA

# estimando VAR Estrutural
svar.A <- SVAR(varest, estmethod = "direct",
               Amat = Amat, hessian = TRUE)

# Seção 8.5 - Não estacionariedade e cointegração --------------------------

# Subseção 8.5.2 - VECM ---------------------------------------------------- 
set.seed(9)

e1 <- rnorm(250,0,0.5) 
e2 <- rnorm(250,0,0.5) 
e3 <- rnorm(250,0,0.5) 

u1.ar1 <- arima.sim(model = list(ar=0.75), innov = e1, n = 250)
u2.ar1 <- arima.sim(model = list(ar=0.3), innov = e2, n = 250)

y1 <- 0.8*y3 + u1.ar1
y2 <- -0.3*y3 + u2.ar1
y3 <- cumsum(e3)

y.mat <- data.frame(y1,y2,y3)

# Figura 8.8 
plot.ts(y1, lty = 1, ylim = c(-3,7), ylab = "")
lines(y2, lty = 2)
lines(y3, lty = 4)

vecm <- ca.jo(y.mat, type = c("trace"), ecdet = "const")

jo.results <- summary(vecm)
jo.results

vecm.r2 <- cajorls(vecm, r = 2)
vecm.r2

# Subseção 8.5.5 - Exemplo -------------------------------------

# leitura da base de dados
data <- read.csv2("data_exemplo8.5.5.csv")

# transformar em ts
data <- ts(data[,-1], start = c(2003,12), freq = 12)
plot(data) # Figura 8.9

# estimação VEC
VARselect(data, lag.max = 12)

vec <- ca.jo(data, type = "trace", ecdet = "const", K = 2, spec = "transitory")
vec2var <- vec2var(vec,r=1)
serial.test(vec2var)
arch.test(vec2var)

# estimação SVEC

SR <- matrix(NA, nrow = 4, ncol = 4)
rownames(SR)<- colnames(data)
colnames(SR)<- colnames(data)
SR[2,3] <- 0
SR[4,3] <- 0
SR[4,3] <- 0
LR <- matrix(NA, nrow = 4, ncol = 4)
rownames(LR)<- colnames(data)
colnames(LR)<- colnames(data)
LR[2,2:4] <- 0
LR[4,2] <- 0
SR;LR

svec <- SVEC(vec, r = 1, LR = LR, SR = SR,lrtest = F)
summary(svec)

# Figura 8.10 (a)
svecm.irf_IIEBR <- irf(svec, cumulative = F, runs = 100, n.ahead = 50, ci = .9,
                      impulse = "IIEBR", response = list("IIEBR", "SELIC"))
par(mfcol=c(2,1), mar=c(0,4,0,0), oma=c(5,3,5,3))
plot.ts(svecm.irf_IIEBR$irf$IIEBR[,1], axes=F, oma.multi = c(0,0,5,0),
        ylab='IIEBR', ylim=c(-4,6))
lines(svecm.irf_IIEBR$Lower$IIEBR[,1], lty=2, col='red')
lines(svecm.irf_IIEBR$Upper$IIEBR[,1], lty=2, col='red')
axis(side=2, las=2, ylab='')
abline(h=0, col='red')
box()

plot.ts(svecm.irf_IIEBR$irf$IIEBR[,2], axes=F, oma.multi = c(0,0,5,0),
        ylab='SELIC', ylim=c(-2,1))
lines(svecm.irf_IIEBR$Lower$IIEBR[,2], lty=2, col='red')
lines(svecm.irf_IIEBR$Upper$IIEBR[,2], lty=2, col='red')
axis(side=1, las=1)
axis(side=2, las=2)
abline(h=0, col='red')
box()

# Figura 8.10 (b)
svecm.irf_SELIC<- irf(svec, cumulative = F, runs = 100, n.ahead = 50, ci = .9, 
                      impulse = "SELIC", response = list("IIEBR", "PIB"))
par(mfcol=c(2,1), mar=c(0,4,0,0), oma=c(5,3,5,3))
plot.ts(svecm.irf_SELIC$irf$SELIC[,1], axes=F, oma.multi = c(0,0,5,0),
        ylab='IIEBR', ylim=c(-1,3))
lines(svecm.irf_SELIC$Lower$SELIC[,1], lty=2, col='red')
lines(svecm.irf_SELIC$Upper$SELIC[,1], lty=2, col='red')
axis(side=2, las=2, ylab='')
abline(h=0, col='red')
box()

plot.ts(svecm.irf_SELIC$irf$SELIC[,2], axes=F, oma.multi = c(0,0,5,0),
        ylab='PIB', ylim=c(-1,1))
lines(svecm.irf_SELIC$Lower$SELIC[,2], lty=2, col='red')
lines(svecm.irf_SELIC$Upper$SELIC[,2], lty=2, col='red')
axis(side=1, las=1)
axis(side=2, las=2)
abline(h=0, col='red')
box()

# impor restrição sobre a taxa de juros na incerteza
LR[1,2]<-0
svec <- update(svec, LR = LR, SR=SR, r=1, lrtest=T)
summary(svec)
svec$LRover
svecm.irf_SELIC<- irf(svec, cumulative = F, runs = 100, n.ahead = 50, ci = .9, 
                      impulse = "SELIC", response = list("IIEBR"))

# Figura 8.11
par(mfcol=c(1,1), mar=c(0,0,0,0), oma=c(5,3,5,3))
plot.ts(svecm.irf_SELIC$irf$SELIC[,1], axes=F, oma.multi = c(0,0,5,0),
        ylab='IIEBR', ylim=c(-1,3))
lines(svecm.irf_SELIC$Lower$SELIC[,1], lty=2, col='red')
lines(svecm.irf_SELIC$Upper$SELIC[,1], lty=2, col='red')
axis(side=2, las=2, ylab='')
abline(h=0, col='red')
axis(side=1, las=1)
axis(side=2, las=2)
abline(h=0, col='red')
box()



# Seção 8.6 - Investimento e confiança industrial -------------------------
library(mFilter)

# leitura da base de dados
data <- read.csv2("data_exemplo8.6.csv")

# criando proxy para hiato do produto através do filtro HP
data$hiato <- hpfilter(data$ibc.br, type = "lambda", freq = 14400)$cycle

data <- ts(cbind(data$bk, data$ici, data$nuci, data$hiato, data$iiebr), start = c(2005,9), freq = 12)
colnames(data) <- c("bk","ici","nuci", "hiato", "iiebr")
plot(data) # Figura 8.12

# identificando ordem de defasagem do var
(VARselect(data, lag.max = 12))

# estimando o modelo var
var_inv <- VAR(data, p = 3, type = c("const"))
summary(var_inv)

# guardar autovalores do processo
autoval <- vars::roots(var_inv)

# Figura (8.13)
x <- seq(-1,1,length = nrow(data))
y1 <- sqrt(1-x^2)
y2 <- -sqrt(1-x^2)
plot(c(x,x),c(y1,y2),xlab='Parte Real',ylab='Parte Complexa',type='l',main='Círculo Unitário',ylim=c(-2,2),xlim=c(-2,2))
abline(h = 0)
abline(v = 0)
points(autoval, Im(autoval),pch=19)
legend(-2.0,-1.5,legend="Autovalores",pch=19)

# diagnóstico
arch.test(var_inv)
serial.test(var_inv, type = "PT.adjusted")

# plot dos processos de flutuacao (Figura 8.14)
plot(vars::stability(var_inv))

# função resposta impulso pontual e acumulada, descomente a desejada.
# resposta pontual 
ri_inv <- irf(var_inv, cumulative = FALSE, ci = 0.90, boot = TRUE, n.ahead = 30)
# resposta acumulada
# ri_inv <- irf(var_inv, cumulative = TRUE, ci = 0.90, boot = TRUE, n.ahead = 30)

# plot das funções impulso-resposta para BK e ICI

# Figura 8.15
par(mfrow=c(5,1), mar=c(0,4,0,0), oma=c(5,3,5,3))

plot.ts(ri_inv$irf$ici[,1], axes=F, oma.multi = c(0,0,5,0),
        ylab='from ici', ylim=c(-2,3))
#
lines(ri_inv$Lower$ici[,1], lty=2, col='red')
lines(ri_inv$Upper$ici[,1], lty=2, col='red')
axis(side=2, las=2)
abline(h=0, col='red')
box()
grid(ny=0)         
mtext("VAR Impulse Response in BK", line = 2)

plot.ts(ri_inv$irf$bk[,1], axes=F, oma.multi = c(0,0,5,0),
        ylab='from bk', ylim=c(-3,4))
#
lines(ri_inv$Lower$bk[,1], lty=2, col='red')
lines(ri_inv$Upper$bk[,1], lty=2, col='red')
axis(side=2, las=2)
abline(h=0, col='red')
box()
grid(ny=0)        

plot.ts(ri_inv$irf$nuci[,1], axes=F, oma.multi = c(0,0,5,0),
        ylab='from nuci', ylim=c(-2,3))
#
lines(ri_inv$Lower$nuci[,1], lty=2, col='red')
lines(ri_inv$Upper$nuci[,1], lty=2, col='red')
axis(side=2, las=2)
abline(h=0, col='red')
box()
grid(ny=0) 

plot.ts(ri_inv$irf$hiato[,1], axes=F, oma.multi = c(0,0,5,0),
        ylab='from hiato', ylim=c(-7,2))
#
lines(ri_inv$Lower$hiato[,1], lty=2, col='red')
lines(ri_inv$Upper$hiato[,1], lty=2, col='red')
axis(side=2, las=2)
abline(h=0, col='red')
box()
grid(ny=0) 

plot.ts(ri_inv$irf$iiebr[,1], axes=F, oma.multi = c(0,0,5,0),
        ylab='from iiebr', ylim=c(-2,3))
#
lines(ri_inv$Lower$iiebr[,1], lty=2, col='red')
lines(ri_inv$Upper$iiebr[,1], lty=2, col='red')
axis(side=1, las=1)
axis(side=2, las=2)
abline(h=0, col='red')
box()
grid(ny=0) 


# Figura 8.16
par(mfrow=c(5,1), mar=c(0,4,0,0), oma=c(5,3,5,3))

plot.ts(ri_inv$irf$ici[,2], axes=F, oma.multi = c(0,0,5,0),
        ylab='from ici', ylim=c(-2,3))
#
lines(ri_inv$Lower$ici[,2], lty=2, col='red')
lines(ri_inv$Upper$ici[,2], lty=2, col='red')
axis(side=2, las=2)
abline(h=0, col='red')
box()
grid(ny=0)         
mtext("VAR Impulse Response in ICI", line = 2)

plot.ts(ri_inv$irf$bk[,2], axes=F, oma.multi = c(0,0,5,0),
        ylab='from bk', ylim=c(-3,2))
#
lines(ri_inv$Lower$bk[,2], lty=2, col='red')
lines(ri_inv$Upper$bk[,2], lty=2, col='red')
axis(side=2, las=2)
abline(h=0, col='red')
box()
grid(ny=0)        

plot.ts(ri_inv$irf$nuci[,2], axes=F, oma.multi = c(0,0,5,0),
        ylab='from nuci', ylim=c(-2,3))
#
lines(ri_inv$Lower$nuci[,2], lty=2, col='red')
lines(ri_inv$Upper$nuci[,2], lty=2, col='red')
axis(side=2, las=2)
abline(h=0, col='red')
box()
grid(ny=0) 

plot.ts(ri_inv$irf$hiato[,2], axes=F, oma.multi = c(0,0,5,0),
        ylab='from hiato', ylim=c(-7,2))
#
lines(ri_inv$Lower$hiato[,2], lty=2, col='red')
lines(ri_inv$Upper$hiato[,2], lty=2, col='red')
axis(side=2, las=2)
abline(h=0, col='red')
box()
grid(ny=0) 

plot.ts(ri_inv$irf$iiebr[,2], axes=F, oma.multi = c(0,0,5,0),
        ylab='from iiebr', ylim=c(-2,3))
#
lines(ri_inv$Lower$iiebr[,2], lty=2, col='red')
lines(ri_inv$Upper$iiebr[,2], lty=2, col='red')
axis(side=1, las=1)
axis(side=2, las=2)
abline(h=0, col='red')
box()
grid(ny=0)













