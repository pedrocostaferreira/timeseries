###########################################################################################################
######################## Análise de Séries Temporais em R: um curso introdutório  #########################
###########################################################################################################
### Capítulo 6 - Regressão Dinâmica                                                                     ###  
### Autores: Ingrid Christyne Luquett de Oliveira e Pedro Costa Ferreira                                ###
###########################################################################################################

# Instalando pacotes necessários ao capítulo
# Uma vez instalados os pacotes, os comandos a seguir não precisam mais ser executados.
# install.packages("lmtest")
# install.packages("orcutt")
# install.packages("prais")
# install.packages("chron")
# install.packages("lubridate")
# install.packages("xts")
# install.packages("urca")
# install.packages("tseries")
# install.packages("BETS")
# install.packages("dynlm")


# Seção 6.3 - Correlação serial ---------------------------------------------------------------------------

  # Subseção 6.3.3 - Exemplo com dados artificiais

    # Inserindo os dados
    price<-c( 0.27, 0.28, 0.28, 0.28, 0.27, 0.26, 0.28 ,0.27 ,0.26 ,0.28, 0.28 ,0.27, 0.27, 0.29, 0.28,0.29, 0.28, 0.28, 0.28, 0.28, 0.29, 0.29, 0.28, 0.28, 0.28, 0.26, 0.26 ,0.26, 0.27 ,0.26)
    cons<-c(0.39, 0.37, 0.39, 0.42, 0.41, 0.34, 0.33, 0.29, 0.27, 0.26, 0.29 ,0.30, 0.33, 0.32, 0.38,0.38, 0.47, 0.44, 0.39, 0.34, 0.32, 0.31, 0.28, 0.33, 0.31, 0.36, 0.38 ,0.42, 0.44 ,0.55)
    income<-c(78, 79, 81, 80 ,76 ,78, 82, 79, 76, 79, 82, 85, 86, 83, 84, 82, 80, 78, 84, 86, 85, 87, 94, 92, 95, 96, 94, 96, 91, 90)
    temp<-c(41,56,63,68,69,65,61,47,32,24,28,26,32,40,55,63,72,72,67,60,44,40,32,27,28,33,41,52,64,71)
  
    # Agrupando os dados em um data.frame
    dados = data.frame(cons, price, income, temp)
    rm(list = c("cons", "price", "income", "temp"))
    
    # Conhecendo os dados
    head(dados)
  
    # Estimando a regressão linear clássica
    reg<-lm(cons ~ price + income + temp, data = dados)
  
    # Testando correlação serial de primeira ordem
    library(lmtest)
    
      # Durbin-Watson
      dw_reg = dwtest(cons ~ price + income + temp, data = dados)
      dw_reg
    
      # Breusch-Godfrey
      bg_reg = bgtest(cons ~ price + income + temp, data = dados)
      bg_reg
    
    # Estimando modelos com estrutura no erro
  
      # Cochrane-Orcutt 
      library(orcutt)
      co_reg = cochrane.orcutt(reg)
      co_reg
      
      # Prais-Winsten
      library(prais)
      pw_reg = prais.winsten(cons ~ price + income + temp, data = dados)
      pw_reg


# Seção 6.6 - Aplicação à expectativa de inflação dos consumidores ----------------------------------------

  # Lendo os dados diretamente do github
  url_dados <- 'https://raw.githubusercontent.com/pedrocostaferreira/Analise-de-Series-Temporais-em-R/master/cap6-regressao-dinamica/cap6_dados.csv'
  aux_dados <- read.csv2(url_dados)

  # Carregando pacotes
  library(chron)
  library(lubridate)
  library(xts)
  library(urca)
  library(tseries)
  library(BETS)
  
  # Transformando os dados de expectativa de inflação e IPCA em séries temporais
  dados = ts(aux_dados[,-1], start = c(year(as.Date(aux_dados[1,1])), month(as.Date(aux_dados[1,1]))), freq = 12)
  
  # Testes de raiz unitária

    # Expectativa de Inflação
    adf_expinf = ur.df(dados[,"Exp_Cons"], type = "none", lags = 13, selectlags = "AIC")
    summary(adf_expinf) 
    BETS.corrgram(adf_expinf@res, lag.max = 15)
    
    # IPCA
    adf_ipca = ur.df(dados[,"IPCA"], type = "none", lags = 12, selectlags = "AIC")
    summary(adf_ipca) 
    BETS.corrgram(adf_ipca@res, lag.max = 15)
    
    # Expectativa de Inflação x IPCA (cointegração)
    ajuste_coin1 = lm(dados[,"Exp_Cons"] ~ dados[,"IPCA"] - 1)
    summary(ajuste_coin1)
    adf_coin1 = ur.df(ajuste_coin1$residuals, "none", lags = 12, selectlags = "AIC")
    summary(adf_coin1)
    BETS.corrgram(adf_coin1@res, lag.max = 15)
      
  # Estimando o modelo de correção de erro
  library(dynlm)
      
    # Procedimento em duas etapas
    reg1 = lm(dados[,"Exp_Cons"] ~ dados[,"IPCA"] - 1)
    res = ts(reg1$residuals, start = c(2005,09), freq = 12)
    reg2 = dynlm(d(dados[,"Exp_Cons"], 1) ~ d(dados[,"IPCA"], 1) + L(res, 1) -1 )
    summary(reg1)
    summary(reg2)
      
    # Procedimento em única etapa
    reg = dynlm(d(dados[,"Exp_Cons"], 1) ~ d(dados[,"IPCA"], 1) + L(dados[,"Exp_Cons"], 1) + L(dados[,"IPCA"], 1) -1)
    summary(reg)
    
    # Os coeficientes da Tabela 6.3 são obtidos da seguinte forma:
      # Coluna 1 - Única Equação  
      beta_1 = reg$coefficients['d(dados[, "IPCA"], 1)']
      gamma_1 = reg$coefficients['L(dados[, "Exp_Cons"], 1)']
      phi_1 = - reg$coefficients['L(dados[, "IPCA"], 1)']/gamma_1
      
      # Coluna 2 - Duas Equações
      beta_2 = reg2$coefficients['d(dados[, "IPCA"], 1)']
      gamma_2 = reg2$coefficients['L(res, 1)']
      phi_2 = reg1$coefficients['dados[, "IPCA"]']
    
