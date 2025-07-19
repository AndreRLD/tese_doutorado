#limpar area de trabalho
rm(list= ls())
gc(reset=T)
graphics.off()



#randomizacao acontece seguindo a ordem dos grupos (primeiro= sham, segundo= IM, terceiro= T60, quarto= T90, quinto= T180)
set.seed(12348)
lote8= 79:90

#tamanho dos grupos lote8 (sham=2, IM= 3, T60= 2, T90= 3, T180= 2)
random_lote8= sample(lote8, 12, replace= FALSE)
random_lote8
sham_lote8= c(81, 88)
IM_lote8= c(84, 83, 79)
T60_lote8= c(87, 89)
T90_lote8= c(86, 80, 82)
T180_lote8= c(85, 90)


Vo2_1_ordem= sample(lote8, 12, replace= FALSE)

Vo2_1_ordem


vo2_2_ordem = sample(lote8, 12, replace= FALSE)

vo2_2_ordem


ECO2_ordem= sample(lote8, 12, replace= FALSE)

ECO2_ordem

#Troquei o sorteio do IM pela hemodinâmia sem querer.
IM_ordem= sample(lote8, 12, replace= FALSE)

IM_ordem



ECO3_ordem = sample(lote8, 12, replace= FALSE)

ECO3_ordem

hemodinamica_ordem = sample(lote8, 12, replace= FALSE)

hemodinamica_ordem