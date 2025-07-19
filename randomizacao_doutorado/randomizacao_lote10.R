#limpar area de trabalho
rm(list= ls())
gc(reset=T)
graphics.off()



#randomizacao acontece seguindo a ordem dos grupos (primeiro= sham, segundo= IM, terceiro= T60, quarto= T90, quinto= T180)
set.seed(123410)
lote10= 103:114

#tamanho dos grupos lote10 (sham=2, IM= 2, T60= 3, T90= 3, T180= 2)
random_lote10= sample(lote10, 12, replace= FALSE)
random_lote10
sham_lote10= c(114, 113)
IM_lote10= c(111, 112)
T60_lote10= c(103, 105, 108)
T90_lote10= c(104, 107, 106)
T180_lote10= c(110, 109)


Vo2_1_ordem= sample(lote10, 12, replace= FALSE)

Vo2_1_ordem


vo2_2_ordem = sample(lote10, 12, replace= FALSE)

vo2_2_ordem


ECO2_ordem= sample(lote10, 12, replace= FALSE)

ECO2_ordem


IM_ordem= sample(lote10, 12, replace= FALSE)

IM_ordem



ECO3_ordem = sample(lote10, 12, replace= FALSE)

ECO3_ordem

hemodinamica_ordem = sample(lote10, 12, replace= FALSE)

hemodinamica_ordem