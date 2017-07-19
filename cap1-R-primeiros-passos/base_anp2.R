###########################################################################################################
################################## Análise de Séries Temporais em R: um curso introdutório  ###############
################################### CAPÍTULO 1. INTRODUÇÃO AO R    ########################################
### ######## Autores: Anna Carolina BArros, Daiane Marcolino e Pedro Costa Ferreira  ######################
############# Exercício: base_anp2.csv ####################################################################
###########################################################################################################


anp<-read.csv2("dados_anp2.csv",stringsAsFactors = FALSE)
str(anp)
anp$PRECO_COMPRA<-as.numeric(anp$PRECO_COMPRA)

anp[which(is.null(anp$PRECO_COMPRA)),]<-NA

# 1) Faça o summary para entender a sua base:

summary(anp)

# 2) Quantos preços foram coletados? 

nrow(anp)


# 3) Crie uma tabela com a frequência de postos por combústivel, atribua essa tabela à variável “quantidade_postos”:

quantidade_postos<-ddply(anp, .(COMBUSTIVEL), summarize,freq=length(COMBUSTIVEL))


# 4) Qual combustível teve menos preços coletados? Isso faz sentido?

# analise a tabela frequência

# 5) Qual é o posto com menor preço de venda? É confiável essa fonte (dica: olhe para o fornecedor e a bandeira.)

menor_preco<-min(anp$PRECO_VENDA)
anp[which(anp$PRECO_VENDA==menor_preco),]

# 6) Crie a tabela dados_etanol, que é um filtro do data frame anp. Sumarize dados_etanol por UF e média dos preços de venda do etanol

dados_etanol<-anp[which(anp$COMBUSTIVEL=="Etanol"),]


quantidade_etanol<-ddply(dados_etanol, .(UF), summarize,PRECO=mean(PRECO_VENDA))


#7) Qual é o estado com a menor média de preços de venda do etanol. Isso faz sentido?

preco_pequeno<-min(quantidade_etanol$PRECO)

quantidade_etanol[which(quantidade_etanol$PRECO==preco_pequeno),"UF"]

# 8) Exporte para o mesmo arquivo em excel os data frames

# Anp

write.xlsx(anp,"anp.xlsx",sheetName = "Anp", row.names = FALSE)

#dados_etanol
write.xlsx(dados_etanol,"anp.xlsx",sheetName = "Anp", row.names = FALSE,append = TRUE)


