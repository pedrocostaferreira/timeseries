###########################################################################################################
################################## Análise de Séries Temporais em R: um curso introdutório  ###############
###########################################################################################################
### Capítulo 5 - Ajuste Sazonal ###########################################################################
### Autores: Daiane Marcolino de Mattos e Pedro Costa Ferreira ############################################ 
###########################################################################################################

# Seção 5.3 -----------------------------------------

# Instalação dos pacotes necessários
install.packages("seasonal")
library(seasonal)

# Verificar se instalação ocorreu adequadamente
checkX13()

# Seção 5.5 - -----------------------------------------
# Aplicação no Índice de Produção Industrial

# leitura dos dados
pim <- read.csv2("https://git.io/vyDPQ")
pim.ts <- ts(pim, start = c(2002,1), freq = 12)

# análise gráfica
plot(pim.ts)
monthplot(pim.ts, col.base = 2, lty.base = 2)
legend("topleft", legend = c("pim", "média"),
       lty = c(1,2), col = c(1,2), bty = "n")


# execução do ajuste no modo automático
(ajuste <- seas(pim.ts))

# avaliação do ajuste automático
summary(ajuste)

# gráfico espectral
spec.orig <- data.frame(series(ajuste, "sp0"))
library(ggplot2)
ggplot(aes(x=0:60,y = X10.Log.Spectrum_AdjOri.), 
                 data = spec.orig, colour = "black") +
     geom_line() +
     geom_vline(colour = "red", xintercept = c(10, 20, 30, 40, 50), 
                               linetype = 5) +
     geom_vline(colour = "blue", xintercept = c(42, 52),  linetype = 3) +
     ylab(" ") + xlab(" ") + theme_bw() +
     ggtitle("Spectral plot of the first-differenced original series") +
     theme(plot.title = element_text(lineheight=2, face="bold",size = 16))

# verificação de sazonalidade
qs(ajuste)

# gráfico SI ratio
monthplot(ajuste, col.base = 1, lty.base = 2, labels = month.abb, lwd.base = 2)
legend("topleft", legend = c("SI", "FS", "Mean FS"), 
       cex = 0.7, lty = c(1,1,2), col = c(4,2,1), lwd = c(1,2,2))

# subseção 5.5.4 - correção do ajuste automático --------------------------------
dates <- c("02/12/2002","03/04/2003","02/24/2004","02/08/2005",
           "02/28/2006","02/20/2007","02/05/2008","02/24/2009",
           "02/16/2010","03/08/2011","02/21/2012","02/12/2013",
           "03/04/2014","02/17/2015","02/09/2016","02/28/2017")
carnaval.date <- as.Date(dates,  "%m/%d/%Y")
carnaval <- genhol(carnaval.date, start = -3, end = 1, frequency = 12)

# novo ajuste sazonal 
ajuste_novo <- seas(pim.ts, transform.function = "none",
                    xreg = carnaval, regression.variables = "td1coef")

# avaliação do ajuste
summary(ajuste_novo)
qs(ajuste_novo)

# gráfico do ajuste sazonal
plot(ajuste_novo, main = "")
legend("topleft", legend = c("Observada", "Com ajuste sazonal"),
       cex = 0.7, lty = 1, col = c(1,2), lwd = c(1,2), bty = "n")
