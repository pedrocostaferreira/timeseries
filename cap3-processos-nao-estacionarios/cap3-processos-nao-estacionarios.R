###########################################################################################################
################################## Análise de séries temporais em R: um curso introdutório  ###############
###########################################################################################################
### Capítulo 3 - Processos não estacionários ##############################################################
### Autor: Pedro Costa Ferreira ########################################################################### 
###########################################################################################################

# pacotes utilizados ao longo do capítulo
#install.packages("strucchange")
library(strucchange)
#install.packages("TSA")
library(TSA)
#install.packages("urca")
library(urca)
#install.packages("mFilter")
library(mFilter)

##### -------- Seção 3.1 -------- ##### 

# leitura dos dados
ibcbr <- read.csv("STI-20160413130025067.csv", header=FALSE, sep=";")
ibcts <- ts(data = ibcbr$V2, start = c(2005,1), frequency = 12)

# gráficos (figura 3.1)
plot(ibcts)
plot(TSA::acf(ibcts, plot=FALSE), main="")

##### -------- Seção 3.2 -------- #####
set.seed(1) 
a <- 1
wn <- rnorm(100)
rw <-a+ cumsum(wn)
t=1:100
rwd <- a+rw + t
tend <- t + wn

# gráficos (figura 3.2)
plot.ts(wn,ann=F)
plot.ts(rw,ann=F)
plot.ts(rwd,ann=F)
plot.ts(tend,ann=F)

# gráficos (figura 3.3)
plot(TSA::acf(wn, plot=FALSE), main="")
plot(TSA::acf(rw, plot=FALSE), main="")
plot(TSA::acf(rwd, plot=FALSE), main="")
plot(TSA::acf(tend, plot=FALSE), main="")

# gráficos (figura 3.4)
lmresid_tend <- resid(lm(tend ~ t))
diff_rwd <- diff(rwd)
plot.ts(tend)
plot.ts(diff_rwd)

# gráficos (figura 3.5)
lmresid_rwd <- lm(rwd ~ t)
det_rwd <- resid(lmresid_rwd)
plot.ts(rwd)
plot.ts(det_rwd)

# gráficos (figura 3.6)
diff_rwd <- diff(rwd)
plot(TSA::acf(diff_rwd, plot=FALSE), main="")
plot(TSA::acf(det_rwd, plot=FALSE), main="")

# gráficos (figura 3.7)
tibc <- 1:133
det_ibcts <- resid(lm(ibcts~tibc))
# dev.off()
# par(mfrow=c(2,2), mar=c(2, 2, 0.8, 0.8))
plot.ts(det_ibcts)
plot.ts(diff(ibcts))
plot(TSA::acf(det_ibcts, plot=FALSE), main="")
plot(TSA::acf(diff(ibcts), plot=FALSE), main="")

##### -------- Seção 3.5 -------- #####

# gráfico (figura 3.8)
ibc_cus <- efp(diff(ibcts)~lag(diff(ibcts)))
bound.ibc_cus <- boundary(ibc_cus, alpha=0.1)
plot(ibc_cus, boundary=T)
sctest(ibc_cus)

# gráfico (figura 3.9)
y<-rw + t
y[50:100]<-y[50:100]+mean(y[0:49])
plot.ts(y)

# gráficos (figura 3.10)
efp_y <- efp(diff(y)~lag(diff(y),1))
plot(efp_y)
plot(breakpoints(diff(y)~lag(diff(y),1), breaks = 6))

##### -------- Seção 3.6 -------- #####

#### Teste ADF ----
adf_ibc<- ur.df(ibcts, type = "trend", lags = 6, selectlags = "AIC")
summary(adf_ibc)@teststat
summary(adf_ibc)@cval

#### Teste KPSS ----
kpss_ibc<- ur.kpss(ibcts, type = "tau", lags = "short")
summary(kpss_ibc)@teststat
summary(kpss_ibc)@cval

#### Teste de Phillips-Perron ----
pp_ibc<- ur.pp(ibcts, type = "Z-tau", lags = "short")
summary(pp_ibc)@teststat
summary(pp_ibc)@cval

#### Teste DF-GLS ----
gls_ibc<- ur.ers(ibcts, type = "DF-GLS", model = "trend" ,lag.max = 6)
summary(gls_ibc)@teststat
summary(gls_ibc)@cval

