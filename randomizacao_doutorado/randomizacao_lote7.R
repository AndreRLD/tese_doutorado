#limpar area de trabalho
rm(list= ls())
gc(reset=T)
graphics.off()



#randomizacao acontece seguindo a ordem dos grupos (primeiro= sham, segundo= IM, terceiro= T60, quarto= T90, quinto= T180)
set.seed(12347)
lote7= 67:78

#tamanho dos grupos lote6 (sham=2, IM= 2, T60= 3, T90= 3, T180= 2)
random_lote7= sample(lote7, 12, replace= FALSE)
random_lote7
sham_lote7= c(67, 78)
IM_lote7= c(69, 73)
T60_lote7= c(68, 77, 74)
T90_lote7= c(71, 70, 75)
T180_lote7= c(76, 72)


Vo2_1_ordem= sample(lote7, 12, replace= FALSE)

Vo2_1_ordem


vo2_2_ordem = sample(lote7, 12, replace= FALSE)

vo2_2_ordem


ECO2_ordem= sample(lote7, 12, replace= FALSE)

ECO2_ordem


IM_ordem= sample(lote7, 12, replace= FALSE)

IM_ordem



ECO3_ordem = sample(lote7, 12, replace= FALSE)

ECO3_ordem

hemodinamica_ordem = sample(lote7, 12, replace= FALSE)

hemodinamica_ordem