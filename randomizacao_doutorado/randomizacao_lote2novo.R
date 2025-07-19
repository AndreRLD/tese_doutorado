#limpar area de trabalho
rm(list= ls())
gc(reset=T)
graphics.off()



#randomizacao acontece seguindo a ordem dos grupos (primeiro= sham, segundo= IM, terceiro= T60, quarto= T90, quinto= T180)
set.seed(12342) #o numero final representa o numero do lote
lote2= 10:18


VO2pico= sample(lote2, 9,replace= FALSE)

#ordem ECO1 (nome do objeto estÃ¡ errado)
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


#Confundi a ordem do ECO2 e IM. ECO2= 10, 12, 15, 13, 14, 18, 16, 17, 11
#IM= 18, 15, 16, 10, 13, 11, 12, 14, 17

vo2_2_ordem = sample(lote2, 9, replace= FALSE)

vo2_2_ordem

ECO_2_ordem= sample(lote2, 9, replace= FALSE)

ECO_2_ordem

IM_ordem= sample(lote2, 9, replace= FALSE)

IM_ordem

MET3_ordem= sample(lote2, 9, replace= FALSE)
MET3_ordem

ECO3_ordem= sample(lote2, 9, replace= FALSE)
ECO3_ordem

hemodinamica_ordem= sample(lote2, 9, replace= FALSE)
hemodinamica_ordem