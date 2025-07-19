#limpar area de trabalho
rm(list= ls())
gc(reset=T)
graphics.off()



#randomizacao acontece seguindo a ordem dos grupos (primeiro= sham, segundo= IM, terceiro= T60, quarto= T90, quinto= T180)
set.seed(12343)
lote4= 31:42

#tamanho dos grupos lote4 (sham=2, IM= 2, T60= 2, T90= 3, T180= 3)
random_lote4= sample(lote4, 12, replace= FALSE)
random_lote4
sham_lote4= c(42, 35)
IM_lote3= c(31, 38)
T60_lote3= c(34, 32)
T90_lote3= c(41, 36, 39)
T180_lote3= c(37, 33, 40)

ECO_1_ordem= sample(lote4, 12, replace= FALSE)

ECO_1_ordem


Vo2_1_ordem= sample(lote4, 12, replace= FALSE)

Vo2_1_ordem


vo2_2_ordem = sample(lote4, 12, replace= FALSE)

vo2_2_ordem


ECO2_ordem= sample(lote4, 12, replace= FALSE)

ECO2_ordem

IM_ordem= sample(lote4, 12, replace= FALSE)

IM_ordem

MET3_ordem= sample(lote4, 12, replace= FALSE)

MET3_ordem

ECO3_ordem = sample(lote4, 12, replace= FALSE)

ECO3_ordem

hemodinamica_ordem = sample(lote4, 12, replace= FALSE)

hemodinamica_ordem