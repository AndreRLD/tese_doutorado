#limpar area de trabalho
rm(list= ls())
gc(reset=T)
graphics.off()

#randomizacao acontece seguindo a ordem dos grupos (primeiro= sham, segundo= IM, terceiro= T60, quarto= T90, quinto= T180)
set.seed(123412)
lote12= 121:126

#tamanho dos grupos lote12 (T90= 3, T180= 3)
random_lote12= sample(lote12, 6, replace= FALSE)
random_lote12
T90_lote12= c(124, 122, 123)
T180_lote12= c(121, 126, 125)


Vo2_1_ordem= sample(lote12, 6, replace= FALSE)

Vo2_1_ordem


vo2_2_ordem = sample(lote12, 6, replace= FALSE)

vo2_2_ordem


ECO2_ordem= sample(lote12, 6, replace= FALSE)

ECO2_ordem


IM_ordem= sample(lote12, 6, replace= FALSE)

IM_ordem



ECO3_ordem = sample(lote12, 6, replace= FALSE)

ECO3_ordem

hemodinamica_ordem = sample(lote12, 6, replace= FALSE)

hemodinamica_ordem