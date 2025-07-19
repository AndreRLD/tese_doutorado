#limpar area de trabalho
rm(list= ls())
gc(reset=T)
graphics.off()



#randomizacao acontece seguindo a ordem dos grupos (primeiro= sham, segundo= IM, terceiro= T60, quarto= T90, quinto= T180)
set.seed(123410)
lote11= 115:120

#tamanho dos grupos lote10 (T90= 3, T180= 3)
random_lote11= sample(lote11, 6, replace= FALSE)
random_lote11
T90_lote11= c(118, 117, 115)
T180_lote11= c(120, 119, 116)


Vo2_1_ordem= sample(lote11, 6, replace= FALSE)

Vo2_1_ordem


vo2_2_ordem = sample(lote11, 6, replace= FALSE)

vo2_2_ordem


ECO2_ordem= sample(lote11, 6, replace= FALSE)

ECO2_ordem


IM_ordem= sample(lote11, 6, replace= FALSE)

IM_ordem



ECO3_ordem = sample(lote11, 6, replace= FALSE)

ECO3_ordem

hemodinamica_ordem = sample(lote11, 6, replace= FALSE)

hemodinamica_ordem
