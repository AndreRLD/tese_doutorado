#limpar area de trabalho
rm(list= ls())
gc(reset=T)
graphics.off()

#randomizacao acontece seguindo a ordem dos grupos (primeiro= sham, segundo= IM, terceiro= T60, quarto= T90, quinto= T180)
set.seed(123413)
lote13= 127:135

#tamanho dos grupos lote13 (T90= 4, T180= 5)
random_lote13= sample(lote13, 9, replace= FALSE)
random_lote13
T90_lote13= c(135, 130, 133, 129)
T180_lote13= c(131, 132, 134, 127, 128)


Vo2_1_ordem= sample(lote13, 9, replace= FALSE)

Vo2_1_ordem


vo2_2_ordem = sample(lote13, 9, replace= FALSE)

vo2_2_ordem


ECO2_ordem= sample(lote13, 9, replace= FALSE)

ECO2_ordem


IM_ordem= sample(lote13, 9, replace= FALSE)

IM_ordem



ECO3_ordem = sample(lote13, 9, replace= FALSE)

ECO3_ordem

hemodinamica_ordem = sample(lote13, 9, replace= FALSE)

hemodinamica_ordem