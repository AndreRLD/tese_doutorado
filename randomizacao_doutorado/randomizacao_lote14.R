#limpar area de trabalho
rm(list= ls())
gc(reset=T)
graphics.off()

#randomizacao acontece seguindo a ordem dos grupos (primeiro= sham, segundo= IM, terceiro= T60, quarto= T90, quinto= T180)
set.seed(123414)
lote14= 136:144

#tamanho dos grupos lote14 (SHAM= 4 , IM= 5)
random_lote14= sample(lote14, 9, replace= FALSE)
random_lote14

# Na hora da cirurgia o animal 141 que era SHAM foi infartado por engano e por isso passou a ser do grupo IM. O animal 138 (era o próximo animal na sequência da cirugia) que iria ser infartado passou a ser do grupo SHAM.
SHAM_lote14= c(138, 143, 140, 137)
IM_lote14= c(141, 139, 142, 136, 144)


Vo2_1_ordem= sample(lote14, 9, replace= FALSE)

Vo2_1_ordem


vo2_2_ordem = sample(lote14, 9, replace= FALSE)

vo2_2_ordem


ECO2_ordem= sample(lote14, 9, replace= FALSE)

ECO2_ordem


IM_ordem= sample(lote14, 9, replace= FALSE)

IM_ordem



ECO3_ordem = sample(lote14, 9, replace= FALSE)

ECO3_ordem

papilar_ordem = sample(lote14, 9, replace= FALSE)

papilar_ordem