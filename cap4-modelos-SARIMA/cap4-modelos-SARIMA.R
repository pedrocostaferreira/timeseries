###########################################################################################################
################################## Análise de Séries Temporais em R: um curso introdutório  ###############
###########################################################################################################
### Capítulo 4 - Modelos SARIMA
### Autores: Pedro Costa Ferreira e Daiane Marcolino de Mattos
###########################################################################################################

# Seção 4.2 -------------------------------------------------
setwd("digitar o endereço neste espaço")

# Seção 4.3 -------------------------------------------------
# subseção 4.3.1 --------------------------------------------

# leitura da ST no R
data(AirPassengers)

# gráfico da ST - Figura 4.1
ts.plot(AirPassengers)

# subseção 4.3.2 --------------------------------------------

# monthplot
monthplot(AirPassengers)

# subseção 4.3.3 --------------------------------------------

# decomposição da ST
plot(decompose(AirPassengers))

# Seção 4.4 -------------------------------------------------
# subseção 4.4.1 --------------------------------------------

# FAC da ST
require(BETS)
BETS.corrgram(AirPassengers, lag.max = 36)

# teste ADF
require(urca)
adf.drift <- ur.df(y = AirPassengers, type = c("drift"), 
                   lags = 24, selectlags = "AIC")

# FAC dos resíduos do teste ADF
BETS.corrgram(adf.drift@res, lag.max = 36)

#
adf.drift@teststat  #estatística de teste

#
adf.drift@cval  #valores tabulados por MacKinnon (1996)

# mais informações sobre o teste RU
summary(adf.drift)

# Figura 4.6
ts.plot(diff(AirPassengers, lag = 1, differences = 1))

# Figura 4.7
BETS.corrgram(diff(AirPassengers, lag = 1, differences = 1),
              lag.max = 36)

# Figura 4.8
ts.plot(diff(log(AirPassengers),lag = 1,differences = 1))

# Figura 4.9
BETS.corrgram(diff(log(AirPassengers), lag = 1, differences = 1),
              lag.max=48)

# subseção 4.4.2 -------------------------------------------------

# Figura 4.10
BETS.corrgram(diff(diff(log(AirPassengers), lag = 1, differences = 1),
                   lag = 12, differences = 1), lag.max = 48)

# Teste de RU na ST com diferenças sazonal e não sazonal
adf.drift2 <- ur.df(y = diff(log(AirPassengers)), type = c("drift"), 
                    lags = 21)
adf.drift2@teststat  #estatística de teste

#
adf.drift2@cval  #valores tabulados por MacKinnon (1996)

# Figura 4.11
BETS.corrgram(adf.drift2@res, lag.max = 36)

# Seção 4.5 --------------------------------------------------------------
# subseção 4.5.1 ---------------------------------------------------------

BETS.corrgram(diff(diff(log(AirPassengers), lag = 1, differences = 1),
              lag = 12, differences = 1), lag.max = 48)
BETS.corrgram(diff(diff(log(AirPassengers), lag = 1, differences = 1),
              lag = 12, differences = 1), type = "partial", lag.max = 48)

# subseção 4.5.2 ---------------------------------------------------------

# estimação
library("forecast")
fit.air <- Arima(AirPassengers, order = c(1,1,1), seasonal = c(1,1,1),
                 method = "ML", lambda = 0)
summary(fit.air)

# teste de significância para o modelo SARIMA(1,1,1)(1,1,1)12
BETS.t_test(fit.air)

#
fit.air <- Arima(AirPassengers, order = c(0,1,1), seasonal = c(0,1,1),
                 method = "ML", lambda=0)

#
BETS.t_test(fit.air)

# subseção 4.5.3 ---------------------------------------------------------
#
diag <- tsdiag(fit.air, gof.lag = 20)

#
Box.test(x = fit.air$residuals, lag = 24,
         type = "Ljung-Box", fitdf = 2)

#
require(FinTS)
ArchTest(fit.air$residuals,lags = 12)

#
require(normtest)
jb.norm.test(fit.air$residuals, nrepl=2000)

# subseção 4.5.4 ---------------------------------------------------------

# previsão
require(forecast)
prev <- forecast(object = fit.air, h=12, level = 0.95)
plot(prev)

#
accuracy(fit.air)

# Seção 4.6 ---------------------------------------------------------

# exportando previsões
# csv
write.csv2(data.frame(prev),"previsao.csv")

# xlsx
require(xlsx)
write.xlsx(data.frame(prev),"previsao.xlsx")
