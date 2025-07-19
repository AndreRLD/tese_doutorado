#limpar area de trabalho
rm(list= ls())
gc(reset=T)
graphics.off()

#randomizacao acontece seguindo a ordem dos grupos (primeiro= sham, segundo= IM, terceiro= T60, quarto= T90, quinto= T180)
set.seed(123414)

# sorteio de 8 amostras por grupo da área infartada para as análises de western blot
# Amostras selecionadas com base nas amostras de GPX que deram certo tanto para área remota como para área infartada

sham= c(25, 29, 35, 42, 45, 52, 58, 65, 67, 78, 81, 88)

IM= c(9, 10, 11, 19, 21, 31, 38, 53, 64, 66, 69, 73, 79, 83, 84)

T60= c(15, 16, 20, 22, 32, 44, 48, 55, 59, 61, 74, 77, 87, 89)

T90= c(30, 39, 41, 43, 51, 70, 71, 75, 82)

T180= c(14, 17, 26, 28, 33, 37, 40, 63, 72, 76, 85, 90)


sham_blot= sample(sham, 8, replace= FALSE)

IM_blot= sample(IM, 8, replace = FALSE)

T60_blot= sample(T60, 8, replace = FALSE)

T90_blot= sample(T90, 8, replace = FALSE)

T180_blot= sample(T180, 8, replace = FALSE)

sham_blot
IM_blot
T60_blot
T90_blot
T180_blot


amostra_blot= c(sham_blot, IM_blot, T60_blot, T90_blot, T180_blot)
sort(amostra_blot)

