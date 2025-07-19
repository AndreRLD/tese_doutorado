#limpar area de trabalho
rm(list= ls())
gc(reset=T)
graphics.off()



#randomizacao acontece seguindo a ordem dos grupos (primeiro= sham, segundo= IM, terceiro= T60, quarto= T90, quinto= T180)
set.seed(12349)
lote9= 91:102

#tamanho dos grupos lote9 (sham=2, IM= 2, T60= 2, T90= 3, T180= 3)
random_lote9= sample(lote9, 12, replace= FALSE)
random_lote9
sham_lote9= c(96, 93)
IM_lote9= c(98, 92)
T60_lote9= c(91, 99)
T90_lote9= c(95, 100, 97)
T180_lote9= c(102, 94, 101)


Vo2_1_ordem= sample(lote9, 12, replace= FALSE)

Vo2_1_ordem


vo2_2_ordem = sample(lote9, 12, replace= FALSE)

vo2_2_ordem


ECO2_ordem= sample(lote9, 12, replace= FALSE)

ECO2_ordem


IM_ordem= sample(lote9, 12, replace= FALSE)

IM_ordem



ECO3_ordem = sample(lote9, 12, replace= FALSE)

ECO3_ordem

hemodinamica_ordem = sample(lote9, 12, replace= FALSE)

hemodinamica_ordem