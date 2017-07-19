###########################################################################################################
################################## Análise de Séries Temporais em R: um curso introdutório  ###############
###########################################################################################################
### Capítulo 3 - Modelos SARIMA
### Autores: Pedro Costa Ferreira e Daiane Marcolino de Mattos
###########################################################################################################

# Seção 3.2 -------------------------------------------------
setwd("digitar o endereço neste espaço")

# Seção 3.3 -------------------------------------------------
# subseção 3.3.1 --------------------------------------------

# leitura da ST no R
data(AirPassengers)

# gráfico da ST - Figura 3.1
ts.plot(AirPassengers)

# subseção 3.3.2 --------------------------------------------

# monthplot
monthplot(AirPassengers)

# subseção 3.3.3 --------------------------------------------

# decomposição da ST
plot(decompose(AirPassengers))

# Seção 3.4 -------------------------------------------------
# subseção 3.4.1 --------------------------------------------

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

# Figura 3.6
ts.plot(diff(AirPassengers, lag = 1, differences = 1))

# Figura 3.7
BETS.corrgram(diff(AirPassengers, lag = 1, differences = 1),
              lag.max = 36)

# Figura 3.8
ts.plot(diff(log(AirPassengers),lag = 1,differences = 1))

# Figura 3.9
BETS.corrgram(diff(log(AirPassengers), lag = 1, differences = 1),
              lag.max=48)

# subseção 3.4.2 -------------------------------------------------

# Figura 3.10
BETS.corrgram(diff(diff(log(AirPassengers), lag = 1, differences = 1),
                   lag = 12, differences = 1), lag.max = 48)

# Teste de RU na ST com diferenças sazonal e não sazonal
adf.drift2 <- ur.df(y = diff(log(AirPassengers)), type = c("drift"), 
                    lags = 21)
adf.drift2@teststat  #estatística de teste

#
adf.drift2@cval  #valores tabulados por MacKinnon (1996)

# Figura 3.11
BETS.corrgram(adf.drift2@res, lag.max = 36)

# Seção 3.5 --------------------------------------------------------------


# subseção 3.5.1 ---------------------------------------------------------

BETS.corrgram(diff(diff(log(AirPassengers), lag = 1, differences = 1),
              lag = 12, differences = 1), lag.max = 48)
BETS.corrgram(diff(diff(log(AirPassengers), lag = 1, differences = 1),
              lag = 12, differences = 1), type = "partial", lag.max = 48)

# subseção 3.5.2 ---------------------------------------------------------

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

# subseção 3.5.3 ---------------------------------------------------------
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

# subseção 3.5.4 ---------------------------------------------------------

# previsão
require(forecast)
prev <- forecast(object = fit.air, h=12, level = 0.95)
plot(prev)

#
accuracy(fit.air)

# Seção 3.6 ---------------------------------------------------------

# exportando previsões
# csv
write.csv2(data.frame(prev),"previsao.csv")

# xlsx
require(xlsx)
write.xlsx(data.frame(prev),"previsao.xlsx")