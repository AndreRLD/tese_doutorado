#limpar area de trabalho
rm(list= ls())
gc(reset=T)
graphics.off()



#randomizacao acontece seguindo a ordem dos grupos (primeiro= sham, segundo= IM, terceiro= T60, quarto= T90, quinto= T180)
set.seed(12346)
lote6= 55:66

#tamanho dos grupos lote6 (sham=2, IM= 2, T60= 3, T90= 3, T180= 2)
random_lote6= sample(lote6, 12, replace= FALSE)
random_lote6
sham_lote6= c(58, 65)
IM_lote6= c(66, 64)
T60_lote6= c(59, 61, 55)
T90_lote6= c(62, 57, 60)
T180_lote6= c(63, 56)


Vo2_1_ordem= sample(lote6, 12, replace= FALSE)

Vo2_1_ordem


vo2_2_ordem = sample(lote6, 12, replace= FALSE)

vo2_2_ordem


ECO2_ordem= sample(lote6, 12, replace= FALSE)

ECO2_ordem


IM_ordem= sample(lote6, 12, replace= FALSE)

IM_ordem



ECO3_ordem = sample(lote6, 12, replace= FALSE)

ECO3_ordem

hemodinamica_ordem = sample(lote6, 12, replace= FALSE)

hemodinamica_ordem