###########################################################################################################
################################## Análise de Séries Temporais em R: um curso introdutório  ###############
################################### CAPÍTULO 1. INTRODUÇÃO AO R    ########################################
### ######## Autores: Anna Carolina BArros, Daiane Marcolino e Pedro Costa Ferreira  ######################
############# Exercício: base_anp2.csv ####################################################################
###########################################################################################################

# leitura dos dados
anp<-read.csv2("dados_anp2.csv",stringsAsFactors = FALSE)
str(anp)
anp$PRECO_COMPRA<-as.numeric(anp$PRECO_COMPRA)
anp[which(is.null(anp$PRECO_COMPRA)),]<-NA

# 1) Faça o summary para entender a sua base:

summary(anp)

# 2) Quantos preços foram coletados? 

nrow(anp)


# 3) Qual é o posto com menor preço de venda?

menor_preco<-min(anp$PRECO_VENDA)
anp[which(anp$PRECO_VENDA==menor_preco),]


# 4) Crie a tabela dados_etanol, que é um filtro do data frame anp.

dados_etanol<-anp[which(anp$COMBUSTIVEL=="Etanol"),]
dados_etanol


# 5) Qual é o estado com a menor média de preços de venda do etanol.

preco_pequeno<-min(dados_etanol$PRECO_VENDA)
preco_pequeno

dados_etanol[which(dados_etanol$PRECO_VENDA==preco_pequeno),"UF"]


# 6) Exporte para o mesmo arquivo em excel os data frames

# Anp

write.xlsx(anp,"anp.xlsx",sheetName = "Anp", row.names = FALSE)

#dados_etanol
write.xlsx(dados_etanol,"anp.xlsx",sheetName = "Anp", row.names = FALSE,append = TRUE)



