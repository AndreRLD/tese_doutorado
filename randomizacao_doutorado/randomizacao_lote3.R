#limpar area de trabalho
rm(list= ls())
gc(reset=T)
graphics.off()



#randomizacao acontece seguindo a ordem dos grupos (primeiro= sham, segundo= IM, terceiro= T60, quarto= T90, quinto= T180)
set.seed(12343)
lote3= 19:30

# R não estava funcionando
#ordem ECO1 (25, 20, 29, 26, 19, 27, 30, 24, 21, 23, 22, 28) feita em https://sorteador.com.br/sorteio-de-numeros/resultado
# link https://resulta.do/qrWkQr8fxqpT_

Vo2_1_ordem= sample(lote3, 12, replace= FALSE)

Vo2_1_ordem

#tamanho dos grupos lote3 (sham=2, IM= 2, T60= 3, T90= 3, T180= 2)
random_lote3= sample(lote3, 12, replace= FALSE)
random_lote3
sham_lote3= c(29, 25)
IM_lote3= c(21, 19)
T60_lote3= c(22, 24, 20)
T90_lote3= c(23, 27, 30)
T180_lote3= c(26, 28)

vo2_2_ordem = sample(lote3, 12, replace= FALSE)

vo2_2_ordem

ECO2_ordem= sample(lote3, 12, replace= FALSE)

ECO2_ordem

IM_ordem= sample(lote3, 12, replace= FALSE)

IM_ordem

MET3_ordem= sample(lote3, 12, replace= FALSE)

MET3_ordem

ECO3_ordem = sample(lote3, 12, replace= FALSE)

ECO3_ordem

hemodinamica_ordem = sample(lote3, 12, replace= FALSE)

hemodinamica_ordem