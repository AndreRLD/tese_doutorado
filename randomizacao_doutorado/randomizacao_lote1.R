#limpar area de trabalho
rm(list= ls())
gc(reset=T)
graphics.off()



#randomizacao acontece seguindo a ordem dos grupos (primeiro= sham, segundo= IM, terceiro= T60, quarto= T90, quinto= T180)
set.seed(1234)
lote1= 1:9

#ordem ECO1
ECO1_ordem= sample(lote1, 9, replace= FALSE)

ECO1_ordem
#tamanho dos grupos lote1 (sham=1, IM= 2, T60= 2, T90= 2, T180= 2)
random_lote1= sample(lote1, 9, replace= FALSE)
random_lote1
sham_lote1= c(6)
IM_lote1= c(7,9)
T60_lote1= c(4,8)
T90_lote1= c(5,1)
T180_lote1= c(2,3)

vo2_2_ordem = sample(lote1, 9, replace= FALSE)

vo2_2_ordem

ECO2_ordem= sample(lote1, 9, replace= FALSE)

ECO2_ordem

IM_ordem= sample(lote1, 9, replace= FALSE)

IM_ordem

MET3_ordem= sample(lote1, 9, replace= FALSE)

MET3_ordem

ECO3_ordem = sample(lote1, 9, replace= FALSE)

ECO3_ordem

hemodinamica_ordem = sample(lote1, 9, replace= FALSE)

hemodinamica_ordem
