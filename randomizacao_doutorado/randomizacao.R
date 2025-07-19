#limpar área de trabalho
rm(list= ls())
gc(reset=T)
graphics.off()



#randomização acontece seguindo a ordem dos grupos (primeiro= sham, segundo= IM, terceiro= T60, quarto= T90, quinto= T180)
set.seed(123)
lote1= 1:9
#tamanho dos grupos lote1 (sham=1, IM= 2, T60= 2, T90= 2, T180= 2)
random_lote1= sample(lote1, 9, replace= FALSE)
random_lote1
sham_lote1= c(3)
IM_lote1= c(6,9)
T60_lote1= c(2,8)
T90_lote1= c(5,7)
T180_lote1= c(1,4)