###########################################################################################################
################################## Análise de Séries Temporais em R: um curso introdutório  ###############
###########################################################################################################
### Capítulo 5 - Box & Jenkins com função de transferência
### Autores: Daiane Marcolino de Mattos e Pedro Costa Ferreira  
###########################################################################################################

# Seção 5.3 -------------------------------------------------------

gas <- ts(read.csv("https://git.io/vyDXC"))
plot(gas, main = "")

# Seção 5.4 -------------------------------------------------------

library(BETS)

# subseção 5.4.1 ------------------------------------------

# (a) ajustar um modelo ARIMA para a série independente Xt

# FAC e FACP
BETS.corrgram(gas[,"InputGasRate"], lag.max = 36)
BETS.corrgram(gas[,"InputGasRate"], lag.max = 36, type = "partial")

# modelo ARIMA
library("forecast")
(modelo_x <- Arima(gas[,"InputGasRate"], 
                   order = c(3,0,0), include.mean = F))

# (b) filtrar Yt pelo modelo encontrado em (a)
(modelo_y <- Arima(gas[,"CO2"], model = modelo_x))

# (c) salvar os resíduos dos dois modelos
alpha <- resid(modelo_x)
beta <- resid(modelo_y)

# (d) CCF entre os resíduos obtidos em (c) 
# Figura 5.3
ccf(beta,alpha, xlim = c(0,20))

# subseção 5.4.3 ------------------------------------------

# FAC e FACP para Yt
BETS.corrgram(gas[,"CO2"], lag.max = 36)
BETS.corrgram(gas[,"CO2"], lag.max = 36, type = "partial")

# pacote para estimar f(Xt)
library("TSA")

# defasar Xt
x_novo <- lag(gas[,"InputGasRate"], k = -3)

# arrumar conjunto de dados
gas_novo <- na.omit(cbind(x_novo, gas[,"CO2"]))
colnames(gas_novo) <- c("InputGasRate", "CO2")
head(gas_novo)

# modelo com função de transferência
(modelo_ft <- arimax(x = gas_novo[,"CO2"], order = c(2,0,0),
                     xtransf = gas_novo[,"InputGasRate"],
                     transfer = list(c(1,2))) )

# subseção 5.4.4 ------------------------------------------
# verificar se o modelo adequado
residuos <- resid(modelo_ft)
BETS.corrgram(residuos, lag.max = 36)
ccf(residuos, alpha, na.action = na.omit)

# teste de autocorrelação de Ljung-Box
Box.test(residuos, type = "Ljung-Box", lag = 24)

# Figura 5.6
modelo_y <- Arima(dados.ts[,"CO2"], order = c(2,0,0), include.mean = T)
ajustados <- fitted(modelo_y)
ajustados_ft <- fitted(modelo_ft)
ts.plot(dados.ts[,"CO2"], ajustados, ajustados_ft, lty = c(1,3,2),
        lwd = c(1,3,2), col = c(1, "orangered","dodgerblue"))
legend("bottomright", col = c(1,"dodgerblue", "orangered"), 
       legend = c("Observados", "Ajustados c/ FT", "Ajustados s/ FT"),
       lty = c(1,2,3), lwd = c(1,2,2), cex = 0.7)