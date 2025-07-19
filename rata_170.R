#limpar area de trabalho
rm(list= ls())
gc(reset=T)
graphics.off()


# Estabelecer diretorio de trabalho
setwd("C:/Users/Andre/Meu Drive/Doutorado/Doutorado exercÃ­cio/Tese de doutorado/VO2/teste_vo2_pos/sem_cabecalho")

library(ggplot2)
library(dplyr)


# Abrir arquivo de dados do tipo csv
df1= read.table("df1.csv", sep= ";",header = TRUE)
df1$peso= df1$peso/1000

# Abrir arquivo de dados VO2
rata= read.table("VO2pos_ID170_05_11_2023.mtb", header = TRUE)

# substituir virgula por ponto
rata= data.frame(rata)
rata= rata %>% mutate(o2= gsub(",", ".", o2))
rata= rata %>% mutate(co2= gsub(",", ".", co2))

# transformar as variaveis em tipo numerico
rata$o2= as.numeric(rata$o2)
rata$co2= as.numeric(rata$co2)
rata$fluxo= as.numeric(rata$fluxo)
rata$vel= as.numeric(rata$vel)

# calcular vo2

c_o2e= mean(rata$o2[30:120])
c_co2e= mean(rata$co2[30:120])

vo2= ((rata$fluxo * (c_o2e/100)) - rata$fluxo * (1 - c_o2e/100) / (1 - rata$o2/100 - rata$co2/100) * (rata$o2/100))/ (df1$peso[df1$id==170]^0.75)

vco2= (rata$fluxo * ((1- c_o2e/100)/ (1- rata$o2/100 - rata$co2/100)) * (rata$co2/100) - rata$fluxo * c_co2e/100) / (df1$peso[df1$id==170]^0.75)

l= length(vo2)

vo2[l]

df2= data.frame(vo2, vco2, seg= 1:l)

plot(df2$seg, df2$vo2)




