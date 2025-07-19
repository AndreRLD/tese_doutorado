#limpar area de trabalho
rm(list= ls())
gc(reset=T)
graphics.off()

#randomizacao acontece seguindo a ordem dos grupos (primeiro= sham, segundo= IM, terceiro= T60, quarto= T90, quinto= T180)
set.seed(12341)
lote17= 160:170

#tamanho dos grupos lote17 (SHAM= 2 , IM= 2, T60= 7)
random_lote17= sample(lote17, 11, replace= FALSE)
random_lote17

SHAM_lote17= c(163, 170)
IM_lote17= c(167, 164)
T60_lote17= c(169, 166, 168, 162, 165, 161, 160)


Vo2_1_ordem= sample(lote17, 11, replace= FALSE)

Vo2_1_ordem


vo2_2_ordem = sample(lote17, 11, replace= FALSE)

vo2_2_ordem


ECO2_ordem= sample(lote17, 11, replace= FALSE)

ECO2_ordem


IM_ordem= sample(lote17, 11, replace= FALSE)

IM_ordem



ECO3_ordem = sample(lote17, 11, replace= FALSE)

ECO3_ordem

papilar_ordem = sample(lote17, 11, replace= FALSE)

papilar_ordem