'#limpar area de trabalho
rm(list= ls())
gc(reset=T)
graphics.off()



#randomizacao acontece seguindo a ordem dos grupos (primeiro= sham, segundo= IM, terceiro= T60, quarto= T90, quinto= T180)
set.seed(12345)
lote5= 43:54

#tamanho dos grupos lote4 (sham=2, IM= 2, T60= 3, T90= 3, T180= 2)
random_lote5= sample(lote5, 12, replace= FALSE)
random_lote5
sham_lote5= c(45, 52)
IM_lote5= c(50, 53)
T60_lote5= c(44, 48, 54)
T90_lote5= c(43, 49, 51)
T180_lote5= c(46, 47)

ECO_1_ordem= sample(lote5, 12, replace= FALSE)

ECO_1_ordem


Vo2_1_ordem= sample(lote5, 12, replace= FALSE)

Vo2_1_ordem


vo2_2_ordem = sample(lote5, 12, replace= FALSE)

vo2_2_ordem


ECO2_ordem= sample(lote5, 12, replace= FALSE)

ECO2_ordem

IM_ordem= sample(lote5, 12, replace= FALSE)

IM_ordem

MET3_ordem= sample(lote5, 12, replace= FALSE)

MET3_ordem

ECO3_ordem = sample(lote5, 12, replace= FALSE)

ECO3_ordem

hemodinamica_ordem = sample(lote5, 12, replace= FALSE)

hemodinamica_ordem