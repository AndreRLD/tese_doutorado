#limpar ÃÂ¡rea de trabalho
rm(list= ls())
gc(reset=T)
graphics.off()



#randomizaÃÂ§ÃÂ£o acontece seguindo a ordem dos grupos (primeiro= sham, segundo= IM, terceiro= T60, quarto= T90, quinto= T180)
set.seed(12342) #o número final é o número do lote
lote2= 10:18

#ordem ECO1
VO2pico= sample(lote2, 9,replace= FALSE)
ECO2_ordem= sample(lote2, 9, replace= FALSE)
metabolismo_basal= sample(lote2, 9, replace= FALSE)

VO2pico
ECO2_ordem
metabolismo_basal

#tamanho dos grupos lote2 (sham=1, IM= 2, T60= 2, T90= 2, T180= 2)
random_lote2= sample(lote2, 9, replace= FALSE)
random_lote2
sham_lote2= c(13)
IM_lote2= c(10, 11)
T60_lote2= c(16, 15)
T90_lote2= c(18, 12)
T180_lote2= c(14,17)