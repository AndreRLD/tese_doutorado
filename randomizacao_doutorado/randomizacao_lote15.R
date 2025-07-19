#limpar area de trabalho
rm(list= ls())
gc(reset=T)
graphics.off()

#randomizacao acontece seguindo a ordem dos grupos (primeiro= sham, segundo= IM, terceiro= T60, quarto= T90, quinto= T180)
set.seed(12341)
lote15= 145:150

#tamanho dos grupos lote14 (SHAM= 3 , IM= 3)
random_lote15= sample(lote15, 6, replace= FALSE)
random_lote15

# Na hora da cirurgia o animal 148 que era SHAM foi infartado por engano e por isso passou a ser do grupo IM. O animal 147 (era o próximo animal na sequência da cirugia) que iria ser infartado passou a ser do grupo SHAM.
SHAM_lote15= c(147, 150, 146)
IM_lote15= c(149, 148, 145)


Vo2_1_ordem= sample(lote15, 6, replace= FALSE)

Vo2_1_ordem


vo2_2_ordem = sample(lote15, 6, replace= FALSE)

vo2_2_ordem


ECO2_ordem= sample(lote15, 6, replace= FALSE)

ECO2_ordem


IM_ordem= sample(lote15, 6, replace= FALSE)

IM_ordem



ECO3_ordem = sample(lote15, 6, replace= FALSE)

ECO3_ordem

papilar_ordem = sample(lote15, 6, replace= FALSE)

papilar_ordem