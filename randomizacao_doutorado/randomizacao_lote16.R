#limpar area de trabalho
rm(list= ls())
gc(reset=T)
graphics.off()

#randomizacao acontece seguindo a ordem dos grupos (primeiro= sham, segundo= IM, terceiro= T60, quarto= T90, quinto= T180)
set.seed(12341)
lote16= 151:159

#tamanho dos grupos lote16 (SHAM= 1 , IM= 2, T60= 6)
random_lote16= sample(lote16, 9, replace= FALSE)
random_lote16

SHAM_lote16= c(154)
IM_lote16= c(159, 152)
T60_lote16= c(157, 155, 158, 151, 153, 156)


Vo2_1_ordem= sample(lote16, 9, replace= FALSE)

Vo2_1_ordem


vo2_2_ordem = sample(lote16, 9, replace= FALSE)

vo2_2_ordem


ECO2_ordem= sample(lote16, 9, replace= FALSE)

ECO2_ordem


IM_ordem= sample(lote16, 9, replace= FALSE)

IM_ordem



ECO3_ordem = sample(lote16, 9, replace= FALSE)

ECO3_ordem

papilar_ordem = sample(lote16, 9, replace= FALSE)

papilar_ordem