#limpar area de trabalho
rm(list= ls())
gc(reset=T)
graphics.off()

#randomizacao acontece seguindo a ordem dos grupos (primeiro= sham, segundo= IM, terceiro= T60, quarto= T90, quinto= T180)
set.seed(123414)

# sorteio de 10 amostras por grupo para VEGF, endoglina, SOD e CAT

sham= c(25, 29, 35, 42, 45, 52, 58, 65, 67, 78, 81, 88)

IM= c(9, 10, 11, 19, 21, 31, 38, 53, 64, 66, 69, 73, 79, 83, 84)

T60= c(15, 16, 20, 22, 32, 44, 48, 55, 59, 61, 74, 77, 87, 89)

T90= c(5, 30, 39, 41, 43, 51, 70, 71, 75, 82)

T180= c(14, 17, 26, 28, 33, 37, 40, 63, 72, 76, 85, 90)


sham_sort= sample(sham, 10, replace= FALSE)

IM_sort= sample(IM, 10, replace = FALSE)

T60_sort= sample(T60, 10, replace = FALSE)

T90_sort= sample(T90, 10, replace = FALSE)

T180_sort= sample(T180, 10, replace = FALSE)

sham_sort
IM_sort
T60_sort
T90_sort
T180_sort


amostra_sort= c(sham_sort, IM_sort, T60_sort, T90_sort, T180_sort)
sort(amostra_sort)
