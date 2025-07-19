#limpar area de trabalho
rm(list= ls())
gc(reset=T)
graphics.off()

# Criar dados reprodutíveis
set.seed(1234)

# Estabelecer diretório de trabalho
setwd("G:/Meu Drive/Doutorado/Doutorado exercício/Tese de doutorado/Planilhas/Planilha_análise")

# Abrir arquivo de dados do tipo csv
doutorado= read.table("dados_doutorado_20-06-2023.csv", sep= ";", header = TRUE, fileEncoding = "ISO-8859-1")


# Abrir pacotes
library(ggplot2)
library(ggstatsplot)
library(rmarkdown)
library(tidyverse)
library(knitr)
library(dplyr)
library(WRS2)
library(cowplot)
library(rstatix)
library(RVAideMemoire)
library(car)
library(moments)
library(nortest)
library(ggsci)
library(RColorBrewer)
library(forcats)


# O arquivo Rallfun-v40.txt faz parte do pacote WRS2 e deve ser salvado dentro do diretório de trabalho
# O arquivo Rallfun-v40.txt foi utilizado para executar a função "winsd" que calcula o desvio padrão winsorizado
source("Rallfun-v40.txt")


# Tranformar variáveis no tipo "númerico"
doutorado$massa_pulmao_umido= as.numeric(doutorado$massa_pulmao_umido)
doutorado$massa_pulmao_seco= as.numeric(doutorado$massa_pulmao_seco)
doutorado$massa_ECO3= as.numeric(doutorado$massa_ECO3)
doutorado$massa_soleo= as.numeric(doutorado$massa_soleo)
doutorado$massa_VDE= as.numeric(doutorado$massa_VDE)
doutorado$massa_VE= as.numeric(doutorado$massa_VE)
doutorado$vo2_pico= as.numeric(doutorado$vo2_pico)
doutorado$tempo_VO2= as.numeric(doutorado$tempo_VO2)




# Criar uma planilha sem a definição das siglas

doutorado= doutorado[-1,]

# Criar uma nova variável

doutorado$grupo_4= doutorado$grupo


# Tranformar as variaveis em fatores

doutorado$grupo= factor(doutorado$grupo_4, level= c("SHAM", "NT+IM", "T60+IM", "T90+IM", "T180+IM"))

doutorado$grupo_4= factor(doutorado$grupo_4, level= c("SHAM", "NT+IM", "T60+IM", "T90+IM", "T180+IM"))



# Mudar os nomes dos grupos

levels(doutorado$grupo_4)= c("SHAM"="CT", "NT+MI"= "CT", "T60+MI", "T90+MI", "T180+MI")

levels(doutorado$grupo)= c("SHAM", "NT+MI", "T60+MI", "T90+MI", "T180+MI")



View(doutorado)




# Criar a variável "massa_soleo_corrigida"
doutorado$massa_soleo_corrigida= (doutorado$massa_soleo/ doutorado$massa_ECO3)*1000

# Criar a variável "massa_VE_corrigida"
doutorado$massa_VE_corrigida= (doutorado$massa_VE/ doutorado$massa_ECO3)*1000

# Criar a variável "massa_VD"
doutorado$massa_VD= (doutorado$massa_VDE - doutorado$massa_VE)

# Criar a variável "massa_VD_corrigida"
doutorado$massa_VD_corrigida= (doutorado$massa_VD/ doutorado$massa_ECO3)*1000

# Criar a variável "teor de água no pulmão"
doutorado$teor_agua_pulmao= (doutorado$massa_pulmao_umido - doutorado$massa_pulmao_seco) * 100 / doutorado$massa_pulmao_umido

# Criar a variável "economia_corrida"
doutorado$economia_corrida= (doutorado$vo2_pico/ ((((doutorado$tempo_VO2/60)/2)-1)*5.4+15))





# Tranformar variáveis no tipo "character"
doutorado$massa_pulmao_umido= as.character(doutorado$massa_pulmao_umido)
doutorado$massa_pulmao_seco= as.character(doutorado$massa_pulmao_seco)
doutorado$massa_ECO3= as.character(doutorado$massa_ECO3)
doutorado$massa_soleo= as.character(doutorado$massa_soleo)
doutorado$massa_VE= as.character(doutorado$massa_VE)
doutorado$massa_VDE= as.character(doutorado$massa_VDE)
doutorado$teor_agua_pulmao= as.character(doutorado$teor_agua_pulmao)
doutorado$vo2_pico= as.character(doutorado$vo2_pico)
doutorado$tempo_VO2= as.character(doutorado$tempo_VO2)


# Visualizar a definição das siglas na planilha
doutorado_transposto= t(doutorado)

View(doutorado_transposto)

doutorado1= doutorado

# Tornar a variável grupo um fator e ordenar os grupos
doutorado1$grupo = factor(doutorado1$grupo, c("SHAM", "NT+MI", "T60+MI", "T90+MI", "T180+MI"))

#transformar as variáveis para tipo numérico
doutorado1= mutate_if(doutorado1, is.character, as.numeric)

# Criar figura LVEDL_ex e extrair dados estatísticos

fig_massa_VE_corrigida= ggbetweenstats(doutorado1, x= grupo, y= massa_VE_corrigida,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = bquote(bold("VO"[2]*"peak")))
fig_massa_VE_corrigida= ggbetweenstats(doutorado1, x= grupo, y= massa_VE_corrigida,  centrality.point.args = list(size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= FALSE, bf.message= FALSE, xlab = "Groups", ylab = "LVM(mg/cm₂²)")
fig_massa_VE_corrigida= ggbetweenstats(doutorado1, x= grupo, y= massa_VE_corrigida,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "LVM(mg/cm²)")

fig_massa_VE_corrigida +theme(
  axis.text.x = element_text(size = 10),
  axis.title.x = element_text(size = 12),
  axis.title.y = element_text(size = 12),
  plot.title = element_text(size = 16)
)

fig_massa_VE_corrigida

# Cria um data frame pra cada grupo

dados_SHAM= data.frame(doutorado1[doutorado1$grupo== "SHAM",])
dados_MI= data.frame(doutorado1[doutorado1$grupo== "NT+MI",])
dados_T60= data.frame(doutorado1[doutorado1$grupo== "T60+MI",])
dados_T90= data.frame(doutorado1[doutorado1$grupo== "T90+MI",])
dados_T180= data.frame(doutorado1[doutorado1$grupo== "T180+MI",])


labels(doutorado1)

KS = function(x) {lillie.test(x)$p.value}

View(doutorado1)

# Hexadecimal color specification
brewer.pal(n = 8, name = "Paired")
brewer.pal(n = 8, name = "Greys")
brewer.pal(n = 8, name = "RdBu")




df_DTmax_92_100= data.frame(id= doutorado1$id, Groups= doutorado1$grupo, Dtmax_92_CSA= doutorado1$Dtmax_92_CSA, Dtmax_94_CSA=	doutorado1$Dtmax_94_CSA,	Dtmax_96_CSA= doutorado1$Dtmax_96_CSA,	Dtmax_98_CSA= doutorado1$Dtmax_98_CSA,	Dtmax_100_CSA= doutorado1$Dtmax_100_CSA)
df_DTmax_92_100= gather(df_DTmax_92_100, key = "L92_Lmax", value = "DTmax", c(-Groups, -id))
df_DTmax_92_100$L92_Lmax= factor(df_DTmax_92_100$L92_Lmax, c("Dtmax_92_CSA", "Dtmax_94_CSA", "Dtmax_96_CSA", "Dtmax_98_CSA", "Dtmax_100_CSA"))


fig_Dtmax_L92_Lmax= ggplot(df_DTmax_92_100, aes(x = L92_Lmax, y = DTmax, fill = grupo)) + geom_boxplot()
fig_Dtmax_L92_Lmax= fig_Dtmax_L92_Lmax + scale_fill_manual(values = c("SHAM"= "#D9D9D9", "NT+MI" = "#33A02C", "T90+MI" = "#1F78B4", "T180+MI" = "#A6CEE3"))
fig_Dtmax_L92_Lmax

# sumario ppp
summary_df_DTmax_92_100 <- df_DTmax_92_100 %>%
  group_by(L92_Lmax, Groups) %>%
  summarise(
    Media = mean(DTmax, na.rm= TRUE),
    ErroPadrao = sd(DTmax, na.rm= TRUE) / sqrt(n()))

View(df_DTmax_92_100)
summary_df_DTmax_92_100


# Slope DTmax sem erro padr?o e com os dados individuais
fig_Dtmax_L92_Lmax= ggplot() +
  geom_point(data= summary_df_DTmax_92_100, aes(L92_Lmax, Media, colour= Groups), size= 12) +
  geom_line(data= summary_df_DTmax_92_100, aes(L92_Lmax, Media, group= Groups, colour= Groups), size= 3) +
  geom_point(data= df_DTmax_92_100, aes(L92_Lmax, DTmax, colour= Groups), size= 6,shape= 18, position = position_dodge(width = 0.5)) +
  theme_minimal()+
  scale_color_manual(values = c("SHAM"= "grey50", "NT+MI" = "#33A02C", "T90+MI" = "#1F78B4", "T180+MI" = "#A6CEE3"))+
  labs( x = bquote(bold("Lmax")), y = bquote(bold(DTmax~(g/mm?))), fill= "Groups") +
  scale_x_discrete(labels = c("92%", "94%", "96%", "98%", "100%"))+
  theme(
    text = element_text(size = 28),           # Tamanho da fonte geral
    axis.title.x = element_text(size = 28),    # Tamanho da fonte do título do eixo x
    axis.title.y = element_text(size = 28),    # Tamanho da fonte do título do eixo y
    axis.text.x = element_text(size = 28),     # Tamanho da fonte dos rótulos do eixo x
    axis.text.y = element_text(size = 28),     # Tamanho da fonte dos rótulos do eixo y
    legend.title = element_text(size = 28),    # Tamanho da fonte do título da legenda
    legend.text = element_text(size = 28),    # Tamanho da fonte do texto da legenda
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )

fig_Dtmax_L92_Lmax


# Slope DTmax com erro padr?o e sem os dados individuais
fig_Dtmax_L92_Lmax= ggplot() +
  geom_point(data= summary_df_DTmax_92_100, aes(L92_Lmax, Media, colour= Groups), size= 12) +
  geom_line(data= summary_df_DTmax_92_100, aes(L92_Lmax, Media, group= Groups, colour= Groups), size= 3) +
  geom_errorbar(data= summary_df_DTmax_92_100, aes(L92_Lmax, Media, ymin = Media - ErroPadrao, ymax = Media + ErroPadrao, colour = Groups), size= 3, width = 0.2, alpha = 5) +
    theme(panel.background = element_rect(fill = "white")) +
  scale_color_manual(values = c("SHAM"= "grey50", "NT+MI" = "#33A02C", "T90+MI" = "#1F78B4", "T180+MI" = "#A6CEE3"))+
  labs( x = bquote(bold("Lmax")), y = bquote(bold(DTmax~(g/mm?))), fill= "Groups") +
  scale_x_discrete(labels = c("92%", "94%", "96%", "98%", "100%"))+
  theme(
    text = element_text(size = 28),           # Tamanho da fonte geral
    axis.title.x = element_text(size = 28),    # Tamanho da fonte do título do eixo x
    axis.title.y = element_text(size = 28),    # Tamanho da fonte do título do eixo y
    axis.text.x = element_text(size = 28),     # Tamanho da fonte dos rótulos do eixo x
    axis.text.y = element_text(size = 28),     # Tamanho da fonte dos rótulos do eixo y
    legend.title = element_text(size = 28),    # Tamanho da fonte do título da legenda
    legend.text = element_text(size = 28),    # Tamanho da fonte do texto da legenda
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )

fig_Dtmax_L92_Lmax


df_RT_92_100= data.frame(id= doutorado1$id, Groups= doutorado1$grupo, RT_92_CSA= doutorado1$RT_92_CSA, RT_94_CSA=	doutorado1$RT_94_CSA,	RT_96_CSA= doutorado1$RT_96_CSA,	RT_98_CSA= doutorado1$RT_98_CSA,	RT_100_CSA= doutorado1$RT_100_CSA)
df_RT_92_100= gather(df_RT_92_100, key = "L92_Lmax", value = "RT", c(-Groups, -id))
df_RT_92_100$L92_Lmax= factor(df_RT_92_100$L92_Lmax, c("RT_92_CSA", "RT_94_CSA", "RT_96_CSA", "RT_98_CSA", "RT_100_CSA"))

# sum?rio ppp
summary_df_RT_92_100 <- df_RT_92_100 %>%
  group_by(L92_Lmax, Groups) %>%
  summarise(
    Media = mean(RT, na.rm= TRUE),
    ErroPadrao = sd(RT, na.rm= TRUE) / sqrt(n()))

View(df_RT_92_100)
summary_df_RT_92_100

# RT comprimento-tensao sem erro padr?o e com os dados individuais
fig_RT_L92_Lmax= ggplot() +
  geom_point(data= summary_df_RT_92_100, aes(L92_Lmax, Media, colour= Groups), size= 12) +
  geom_line(data= summary_df_RT_92_100, aes(L92_Lmax, Media, group= Groups, colour= Groups), size= 3) +
  geom_point(data= df_RT_92_100, aes(L92_Lmax, RT, colour= Groups), size= 6,shape= 18, position = position_dodge(width = 0.85)) +
  theme_minimal() +
  scale_color_manual(values = c("SHAM"= "grey50", "NT+MI" = "#33A02C", "T90+MI" = "#1F78B4", "T180+MI" = "#A6CEE3"))+
  labs( x = bquote(bold("Lmax")), y = bquote(bold(RT~(g/mm?))), fill= "Groups") +
  scale_x_discrete(labels = c("92%", "94%", "96%", "98%", "100%"))+
  theme(
    text = element_text(size = 28),           # Tamanho da fonte geral
    axis.title.x = element_text(size = 28),    # Tamanho da fonte do título do eixo x
    axis.title.y = element_text(size = 28),    # Tamanho da fonte do título do eixo y
    axis.text.x = element_text(size = 28),     # Tamanho da fonte dos rótulos do eixo x
    axis.text.y = element_text(size = 28),     # Tamanho da fonte dos rótulos do eixo y
    legend.title = element_text(size = 28),    # Tamanho da fonte do título da legenda
    legend.text = element_text(size = 28),    # Tamanho da fonte do texto da legenda
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )

fig_RT_L92_Lmax



# boxplot RT
fig_RT_L92_Lmax= ggplot(df_RT_92_100, aes(x = L92_Lmax, y = RT, fill = grupo)) + geom_boxplot() +
  theme_minimal() +
  labs( x = "Lmax", y = "RT (g/mm?)", fill= "Groups") +
  scale_x_discrete(labels = c("92%", "94%", "96%", "98%", "100%"))
fig_RT_L92_Lmax= fig_RT_L92_Lmax + scale_fill_manual(values = c("SHAM"= "#D9D9D9", "NT+MI" = "#33A02C", "T90+MI" = "#1F78B4", "T180+MI" = "#A6CEE3"))
fig_RT_L92_Lmax

# boxplot RT normalizado

df_RT_normalizado_92_100= data.frame(grupo= doutorado1$grupo, RT_92_CSA_normalizado= doutorado1$RT_92_CSA_normalizado, RT_94_CSA_normalizado=	doutorado1$RT_94_CSA_normalizado,	RT_96_CSA_normalizado= doutorado1$RT_96_CSA_normalizado,	RT_98_CSA_normalizado= doutorado1$RT_98_CSA_normalizado,	RT_100_CSA_normalizado= doutorado1$RT_100_CSA_normalizado)
df_RT_normalizado_92_100= gather(df_RT_normalizado_92_100, key = "L92_Lmax", value = "RT", -grupo)
df_RT_normalizado_92_100$L92_Lmax= factor(df_RT_normalizado_92_100$L92_Lmax, c("RT_92_CSA_normalizado", "RT_94_CSA_normalizado", "RT_96_CSA_normalizado", "RT_98_CSA_normalizado", "RT_100_CSA_normalizado"))

fig_RT_normalizado_L92_Lmax= ggplot(df_RT_normalizado_92_100, aes(x = L92_Lmax, y = RT, fill = grupo)) + geom_boxplot() +
  theme(panel.background = element_rect(fill = "white"), panel.grid.major.y = element_line(color = "grey90", size = 0.5),) +
  labs( x = "Lmax", y = "RT (g/mm?)", fill= "Groups") +
  scale_x_discrete(labels = c("92%", "94%", "96%", "98%", "100%"))
fig_RT_normalizado_L92_Lmax= fig_RT_normalizado_L92_Lmax + scale_fill_manual(values = c("SHAM"= "#D9D9D9", "NT+MI" = "#33A02C", "T90+MI" = "#1F78B4", "T180+MI" = "#A6CEE3"))
fig_RT_normalizado_L92_Lmax

# Data frame ppp
df_ppp_10_120= data.frame(id= doutorado1$id, Groups= doutorado1$grupo, Dtmax_dif_ppp10= doutorado1$Dtmax_dif_ppp10, Dtmax_dif_ppp20=	doutorado1$Dtmax_dif_ppp20,	Dtmax_dif_ppp30= doutorado1$Dtmax_dif_ppp30,	Dtmax_dif_ppp60= doutorado1$Dtmax_dif_ppp60,	Dtmax_dif_ppp120= doutorado1$Dtmax_dif_ppp120)
df_ppp_10_120= gather(df_ppp_10_120, key = "ppp", value = "DT", c(-Groups, -id))
df_ppp_10_120$ppp= factor(df_ppp_10_120$ppp, c("Dtmax_dif_ppp10", "Dtmax_dif_ppp20", "Dtmax_dif_ppp30", "Dtmax_dif_ppp60", "Dtmax_dif_ppp120"))

# sum?rio ppp
summary_df_ppp <- df_ppp_10_120 %>%
  group_by(ppp, Groups) %>%
  summarise(
    Media = mean(DT, na.rm= TRUE),
    ErroPadrao = sd(DT, na.rm= TRUE) / sqrt(n()))

View(df_ppp_10_120)
summary_df_ppp


#boxplot PPP

fig_ppp_10_120= ggplot(df_ppp_10_120, aes(x = ppp, y = DT, fill = Groups)) + geom_boxplot() +
  theme_minimal()+
  labs( x = "PPP", y = "Dif DT (g/mm?)", fill= "Groups") +
  scale_x_discrete(labels = c("10s", "20s", "30s", "60s", "120s")) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "grey20", position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c("SHAM"= "grey60", "NT+MI" = "#33A02C", "T90+MI" = "#1F78B4", "T180+MI" = "#A6CEE3"))
fig_ppp_10_120


# PPP sem erro padr?o e com os dados individuais
fig_ppp_10_120= ggplot() +
  geom_point(data= summary_df_ppp, aes(ppp, Media, colour= Groups), size= 12) +
  geom_line(data= summary_df_ppp, aes(ppp, Media, group= Groups, colour= Groups), size= 3) +
  geom_point(data= df_ppp_10_120, aes(ppp, DT, colour= Groups), size= 6,shape= 18, position = position_dodge(width = 0.5)) +
  theme_minimal() +
  scale_color_manual(values = c("SHAM"= "grey50", "NT+MI" = "#33A02C", "T90+MI" = "#1F78B4", "T180+MI" = "#A6CEE3"))+
  labs( x = bquote(bold("Electrical stimulus pause")), y = bquote(bold(Delta~DT~(g/mm?))), fill= "Groups") +
  scale_x_discrete(labels = c("10s", "20s", "30s", "60s", "120s"))+
  theme(
    text = element_text(size = 28),           # Tamanho da fonte geral
    axis.title.x = element_text(size = 28),    # Tamanho da fonte do título do eixo x
    axis.title.y = element_text(size = 28),    # Tamanho da fonte do título do eixo y
    axis.text.x = element_text(size = 28),     # Tamanho da fonte dos rótulos do eixo x
    axis.text.y = element_text(size = 28),     # Tamanho da fonte dos rótulos do eixo y
    legend.title = element_text(size = 28),    # Tamanho da fonte do título da legenda
    legend.text = element_text(size = 28),    # Tamanho da fonte do texto da legenda
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )


fig_ppp_10_120

# PPP sem dados individuais e com erro padr?o
fig_ppp_10_120= ggplot() +
  geom_point(data= summary_df_ppp, aes(ppp, Media, colour= Groups), size= 12) +
  geom_line(data= summary_df_ppp, aes(ppp, Media, group= Groups, colour= Groups), size= 3) +
  geom_errorbar(data= summary_df_ppp, aes(ppp, Media, ymin = Media - ErroPadrao, ymax = Media + ErroPadrao, colour = Groups), size= 3, width = 0.2, alpha = 5) +
  theme_minimal() +
  scale_color_manual(values = c("SHAM"= "grey50", "NT+MI" = "#33A02C", "T90+MI" = "#1F78B4", "T180+MI" = "#A6CEE3"))+
  labs( x = bquote(bold("Electrical stimulus pause")), y = bquote(bold(Delta~DT~(g/mm?))), fill= "Groups") +
  scale_x_discrete(labels = c("10s", "20s", "30s", "60s", "120s")) +
  theme(
    text = element_text(size = 28),           # Tamanho da fonte geral
    axis.title.x = element_text(size = 28),    # Tamanho da fonte do título do eixo x
    axis.title.y = element_text(size = 28),    # Tamanho da fonte do título do eixo y
    axis.text.x = element_text(size = 28),     # Tamanho da fonte dos rótulos do eixo x
    axis.text.y = element_text(size = 28),     # Tamanho da fonte dos rótulos do eixo y
    legend.title = element_text(size = 28),    # Tamanho da fonte do título da legenda
    legend.text = element_text(size = 28),    # Tamanho da fonte do texto da legenda
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )


fig_ppp_10_120






# CSA_papilar
df= split(doutorado1$CSA_papilar, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(CSA_papilar~grupo, data = doutorado1)
fig_CSA_papilar= ggbetweenstats(doutorado1, x= grupo, y= CSA_papilar,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "CSA")
fig_CSA_papilar

# peso_papilar
df= split(doutorado1$peso_papilar, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(peso_papilar~grupo, data = doutorado1)
fig_peso_papilar= ggbetweenstats(doutorado1, x= grupo, y= peso_papilar,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "Peso papilar")
fig_peso_papilar

# Dtmax_starling_pre_pos_CSA
df= split(doutorado1$Dtmax_starling_pre_pos_CSA, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(Dtmax_starling_pre_pos_CSA~grupo, data= doutorado1)
leveneTest(Dtmax_starling_pre_pos_CSA~grupo, data= doutorado1, center= mean)
fig_Dtmax_starling_pre_pos_CSA= ggbetweenstats(doutorado1, x= grupo, y= Dtmax_starling_pre_pos_CSA,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "Starling DTmax")
fig_Dtmax_starling_pre_pos_CSA

# RT_starling_pre_pos_CSA
df= split(doutorado1$RT_starling_pre_pos_CSA, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(RT_starling_pre_pos_CSA~grupo, data= doutorado1)
leveneTest(RT_starling_pre_pos_CSA~grupo, data= doutorado1, center= mean)
fig_RT_starling_pre_pos_CSA= ggbetweenstats(doutorado1, x= grupo, y= RT_starling_pre_pos_CSA,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "Starling RT")
fig_RT_starling_pre_pos_CSA

# x1_ppp
df= split(doutorado1$x1_ppp, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(x1_ppp~grupo, data= doutorado1)
leveneTest(x1_ppp~grupo, data= doutorado1, center= mean)
fig_x1_ppp= ggbetweenstats(doutorado1, x= grupo, y= x1_ppp,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "X1-ppp")
fig_x1_ppp

# x2_ppp
df= split(doutorado1$x2_ppp, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(x2_ppp~grupo, data= doutorado1)
leveneTest(x2_ppp~grupo, data= doutorado1, center= mean)
fig_x2_ppp= ggbetweenstats(doutorado1, x= grupo, y= x2_ppp,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "X2-ppp")
fig_x2_ppp


# Dtmax_CSA
df= split(doutorado1$Dtmax_CSA, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(Dtmax_CSA~grupo, data= doutorado1)
leveneTest(Dtmax_CSA~grupo, data= doutorado1, center= mean)
fig_Dtmax_CSA= ggbetweenstats(doutorado1, x= grupo, y= Dtmax_CSA,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "DTmax (g/mm?)") + theme(
text = element_text(size = 12),           # Tamanho da fonte geral
axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
legend.text = element_text(size = 12),     # Tamanho da fonte do texto da legenda
plot.margin = unit(c(0, 5, 0, 6), "cm")
)
plot_grid(fig_Dtmax_CSA, labels= "A", label_size = 24)



# RT_CSA
df= split(doutorado1$RT_CSA, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(RT_CSA~grupo, data= doutorado1)
leveneTest(RT_CSA~grupo, data= doutorado1, center= mean)
fig_RT_CSA= ggbetweenstats(doutorado1, x= grupo, y= RT_CSA,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= FALSE, bf.message= FALSE, xlab = "Groups", ylab = "RT (g/mm?)") + theme(
text = element_text(size = 12),           # Tamanho da fonte geral
axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
legend.text = element_text(size = 12),     # Tamanho da fonte do texto da legenda
plot.margin = unit(c(0, 5, 0, 6), "cm")
)
plot_grid(fig_RT_CSA, labels= "B", label_size = 24)
extract_stats(fig_RT_CSA)


# dt_dt_mais_CSA
df= split(doutorado1$dt_dt_mais_CSA, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(dt_dt_mais_CSA~grupo, data= doutorado1)
leveneTest(dt_dt_mais_CSA~grupo, data= doutorado1, center= mean)
fig_dt_dt_mais_CSA= ggbetweenstats(doutorado1, x= grupo, y= dt_dt_mais_CSA,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "+dT/dt (g/s/mm?)") + theme(
text = element_text(size = 12),           # Tamanho da fonte geral
axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
legend.text = element_text(size = 12),     # Tamanho da fonte do texto da legenda
plot.margin = unit(c(0, 5, 0, 6), "cm")
)

plot_grid(fig_dt_dt_mais_CSA, labels= "C", label_size = 24)



# dt_dt_menos_CSA
df= split(doutorado1$dt_dt_menos_CSA, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(dt_dt_menos_CSA~grupo, data= doutorado1)
leveneTest(dt_dt_menos_CSA~grupo, data= doutorado1, center= mean)
fig_dt_dt_menos_CSA= ggbetweenstats(doutorado1, x= grupo, y= dt_dt_menos_CSA,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= FALSE, bf.message= FALSE, xlab = "Groups", ylab = "-dT/dt (g/s/mm?)") + theme(
text = element_text(size = 12),           # Tamanho da fonte geral
axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
legend.text = element_text(size = 12),     # Tamanho da fonte do texto da legenda
plot.margin = unit(c(0, 5, 0, 6), "cm")
)

plot_grid(fig_dt_dt_menos_CSA, labels= "D", label_size = 24)


# coef_Dtmax_comprimento_tensao
df= split(doutorado1$coef_Dtmax_comprimento_tensao, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(coef_Dtmax_comprimento_tensao~grupo, data= doutorado1)
leveneTest(coef_Dtmax_comprimento_tensao~grupo, data= doutorado1, center= mean)
fig_coef_Dtmax_comprimento_tensao= ggbetweenstats(doutorado1, x= grupo, y= coef_Dtmax_comprimento_tensao,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "Slope DTmax (g/mm?/%Lmax)") + theme(
text = element_text(size = 12),           # Tamanho da fonte geral
axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
legend.text = element_text(size = 12),     # Tamanho da fonte do texto da legenda
plot.margin = unit(c(0, 5, 0, 6), "cm")
)

plot_grid(fig_coef_Dtmax_comprimento_tensao, labels= "D", label_size = 24)


# coef_RT_comprimento_tensao
df= split(doutorado1$coef_RT_comprimento_tensao, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(coef_RT_comprimento_tensao~grupo, data= doutorado1)
leveneTest(coef_RT_comprimento_tensao~grupo, data= doutorado1, center= mean)
fig_coef_RT_comprimento_tensao= ggbetweenstats(doutorado1, x= grupo, y= coef_RT_comprimento_tensao,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "coef RT length-tension") + theme(
text = element_text(size = 12),           # Tamanho da fonte geral
axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
legend.text = element_text(size = 12),     # Tamanho da fonte do texto da legenda
plot.margin = unit(c(0, 5, 0, 6), "cm")
)

plot_grid(fig_coef_RT_comprimento_tensao, labels= "D", label_size = 24)




# coef_RT_comprimento_tensao_c
df= split(doutorado1$coef_RT_comprimento_tensao_c, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(coef_RT_comprimento_tensao_c~grupo, data= doutorado1)
leveneTest(coef_RT_comprimento_tensao_c~grupo, data= doutorado1, center= mean)
fig_coef_RT_comprimento_tensao_c= ggbetweenstats(doutorado1, x= grupo, y= coef_RT_comprimento_tensao_c,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= FALSE, bf.message= FALSE, xlab = "Groups", ylab = "AC RT (g/mm?/%Lmax)") + theme(
text = element_text(size = 12),           # Tamanho da fonte geral
axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
legend.text = element_text(size = 12),     # Tamanho da fonte do texto da legenda
plot.margin = unit(c(0, 5, 0, 6), "cm")
)

plot_grid(fig_coef_RT_comprimento_tensao_c, labels= "D", label_size = 24)


fig_coef_RT_comprimento_tensao_c


# Dtmax_dif_ppp10
df= split(doutorado1$Dtmax_dif_ppp10, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(Dtmax_dif_ppp10~grupo, data= doutorado1)
leveneTest(Dtmax_dif_ppp10~grupo, data= doutorado1, center= mean)
fig_Dtmax_dif_ppp10= ggbetweenstats(doutorado1, x= grupo, y= Dtmax_dif_ppp10,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = bquote(bold(Delta~DTmax~"PPP10s (g/mm?)"))) + theme(
text = element_text(size = 12),           # Tamanho da fonte geral
axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
legend.text = element_text(size = 12),     # Tamanho da fonte do texto da legenda
plot.margin = unit(c(0, 5, 0, 6), "cm")
)


fig_Dtmax_dif_ppp10


# Dtmax_dif_ppp20
df= split(doutorado1$Dtmax_dif_ppp20, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(Dtmax_dif_ppp20~grupo, data= doutorado1)
leveneTest(Dtmax_dif_ppp20~grupo, data= doutorado1, center= mean)
fig_Dtmax_dif_ppp20= ggbetweenstats(doutorado1, x= grupo, y= Dtmax_dif_ppp20,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = bquote(bold(Delta~DTmax~"PPP20s (g/mm?)"))) + theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12),     # Tamanho da fonte do texto da legenda
  plot.margin = unit(c(0, 5, 0, 6), "cm")
)

fig_Dtmax_dif_ppp20


# Dtmax_dif_ppp30
df= split(doutorado1$Dtmax_dif_ppp30, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(Dtmax_dif_ppp30~grupo, data= doutorado1)
leveneTest(Dtmax_dif_ppp30~grupo, data= doutorado1, center= mean)
fig_Dtmax_dif_ppp30= ggbetweenstats(doutorado1, x= grupo, y= Dtmax_dif_ppp30,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = bquote(bold(Delta~DTmax~"PPP30s (g/mm?)"))) + theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12),     # Tamanho da fonte do texto da legenda
  plot.margin = unit(c(0, 5, 0, 6), "cm")
)

fig_Dtmax_dif_ppp30

# Dtmax_dif_ppp60
df= split(doutorado1$Dtmax_dif_ppp60, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(Dtmax_dif_ppp60~grupo, data= doutorado1)
leveneTest(Dtmax_dif_ppp60~grupo, data= doutorado1, center= mean)
fig_Dtmax_dif_ppp60= ggbetweenstats(doutorado1, x= grupo, y= Dtmax_dif_ppp60,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = bquote(bold(Delta~DTmax~"PPP60s (g/mm?)"))) + theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12),     # Tamanho da fonte do texto da legenda
  plot.margin = unit(c(0, 5, 0, 6), "cm")
)


fig_Dtmax_dif_ppp60

# Dtmax_dif_ppp120
df= split(doutorado1$Dtmax_dif_ppp120, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(Dtmax_dif_ppp120~grupo, data= doutorado1)
leveneTest(Dtmax_dif_ppp120~grupo, data= doutorado1, center= mean)
fig_Dtmax_dif_ppp120= ggbetweenstats(doutorado1, x= grupo, y= Dtmax_dif_ppp120,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = bquote(bold(Delta~DTmax~"PPP120s (g/mm?)"))) + theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12),     # Tamanho da fonte do texto da legenda
  plot.margin = unit(c(0, 5, 0, 6), "cm")
)

fig_Dtmax_dif_ppp120

# Dtmax_dif_calcio
df= split(doutorado1$Dtmax_dif_calcio, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(Dtmax_dif_calcio~grupo, data= doutorado1)
leveneTest(Dtmax_dif_calcio~grupo, data= doutorado1, center= mean)
fig_Dtmax_dif_calcio= ggbetweenstats(doutorado1, x= grupo, y= Dtmax_dif_calcio,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "dif calcio 1.5-2.5")
fig_Dtmax_dif_calcio

# FC_ex
df= split(doutorado1$FC_ex, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(FC_ex~grupo, data= doutorado1)
leveneTest(FC_ex~grupo, data= doutorado1, center= mean)
fig_FC_ex= ggbetweenstats(doutorado1, x= grupo, y= FC_ex,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", violin.args = list(width = 0, linewidth = 0), pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "HR (bpm)")
fig_FC_ex

# FC_ex
df= split(doutorado1$FC_ex, doutorado1$grupo_4)
lapply(df, KS)
byf.shapiro(FC_ex~grupo_4, data= doutorado1)
leveneTest(FC_ex~grupo_4, data= doutorado1, center= mean)
fig_FC_ex= ggbetweenstats(doutorado1, x= grupo_4, y= FC_ex,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", violin.args = list(width = 0, linewidth = 0), pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "HR (bpm)")
fig_FC_ex

# LVEDL_ex
df= split(doutorado1$LVEDL_ex, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(LVEDL_ex~grupo, data= doutorado1)
leveneTest(LVEDL_ex~grupo, data= doutorado1, center= mean)
fig_LVEDL_ex= ggbetweenstats(doutorado1, x= grupo, y= LVEDL_ex,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", violin.args = list(width = 0, linewidth = 0), pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "LVEDL (cm)")
fig_LVEDL_ex

# LVEDL_ex grupo_4
df= split(doutorado1$LVEDL_ex, doutorado1$grupo_4)
lapply(df, KS)
byf.shapiro(LVEDL_ex~grupo_4, data= doutorado1)
leveneTest(LVEDL_ex~grupo_4, data= doutorado1, center= mean)
fig_LVEDL_ex4= ggbetweenstats(doutorado1, x= grupo_4, y= LVEDL_ex,  centrality.point.args = list(shape= 15, size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", violin.args = list(width = 0, linewidth = 0), pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", bf.message= FALSE, xlab = "Groups", ylab = "LVEDL (cm)")
fig_LVEDL_ex4

# LVESL_ex
df= split(doutorado1$LVESL_ex, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(LVESL_ex~grupo, data= doutorado1)
leveneTest(LVESL_ex~grupo, data= doutorado1, center= mean)
fig_LVESL_ex= ggbetweenstats(doutorado1, x= grupo, y= LVESL_ex,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", violin.args = list(width = 0, linewidth = 0), pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", bf.message= FALSE, xlab = "Groups", ylab = "LVESL (cm)")
fig_LVESL_ex

# LVESL_ex grupo_4
df= split(doutorado1$LVESL_ex, doutorado1$grupo_4)
lapply(df, KS)
byf.shapiro(LVESL_ex~grupo_4, data= doutorado1)
leveneTest(LVESL_ex~grupo_4, data= doutorado1, center= mean)
fig_LVESL_ex4= ggbetweenstats(doutorado1, x= grupo_4, y= LVESL_ex,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", violin.args = list(width = 0, linewidth = 0), pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "LVESL (cm)")
fig_LVESL_ex4

# LVEDA_ex
df= split(doutorado1$LVEDA_ex, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(LVEDA_ex~grupo, data= doutorado1)
leveneTest(LVEDA_ex~grupo, data= doutorado1, center= mean)
fig_LVEDA_ex= ggbetweenstats(doutorado1, x= grupo, y= LVEDA_ex,  centrality.point.args = list(size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", violin.args = list(width = 0, linewidth = 0), pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= FALSE, bf.message= FALSE, xlab = "Groups", ylab = "LVEDA (cm²)")
fig_LVEDA_ex

# LVEDA_ex grupo_4
df= split(doutorado1$LVEDA_ex, doutorado1$grupo_4)
lapply(df, KS)
byf.shapiro(LVEDA_ex~grupo_4, data= doutorado1)
leveneTest(LVEDA_ex~grupo_4, data= doutorado1, center= mean)
fig_LVEDA_ex4= ggbetweenstats(doutorado1, x= grupo_4, y= LVEDA_ex,  centrality.point.args = list(size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", violin.args = list(width = 0, linewidth = 0), pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= FALSE, bf.message= FALSE, xlab = "Groups", ylab = "LVEDA (cm²)")
fig_LVEDA_ex4

# LVESA_ex
df= split(doutorado1$LVESA_ex, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(LVESA_ex~grupo, data= doutorado1)
leveneTest(LVESA_ex~grupo, data= doutorado1, center= mean)
fig_LVESA_ex= ggbetweenstats(doutorado1, x= grupo, y= LVESA_ex,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", violin.args = list(width = 0, linewidth = 0), pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "LVESA (cm²)")
fig_LVESA_ex

# LVESA_ex grupo_4
df= split(doutorado1$LVESA_ex, doutorado1$grupo_4)
lapply(df, KS)
byf.shapiro(LVESA_ex~grupo_4, data= doutorado1)
leveneTest(LVESA_ex~grupo_4, data= doutorado1, center= mean)
fig_LVESA_ex4= ggbetweenstats(doutorado1, x= grupo_4, y= LVESA_ex,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", violin.args = list(width = 0, linewidth = 0), pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "LVESA (cm²)")
fig_LVESA_ex4

# FAC_ex
df= split(doutorado1$FAC_ex, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(FAC_ex~grupo, data= doutorado1)
leveneTest(FAC_ex~grupo, data= doutorado1, center= mean)
fig_FAC_ex= ggbetweenstats(doutorado1, x= grupo, y= FAC_ex,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", violin.args = list(width = 0, linewidth = 0), pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", bf.message= FALSE, xlab = "Groups", ylab = "FAC (%)")
fig_FAC_ex

# FAC_ex grupo_4
df= split(doutorado1$FAC_ex, doutorado1$grupo_4)
lapply(df, KS)
byf.shapiro(FAC_ex~grupo_4, data= doutorado1)
leveneTest(FAC_ex~grupo_4, data= doutorado1, center= mean)
fig_FAC_ex4= ggbetweenstats(doutorado1, x= grupo_4, y= FAC_ex,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", violin.args = list(width = 0, linewidth = 0), pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "FAC (%)")
fig_FAC_ex4

# LAA_ex
df= split(doutorado1$LAA_ex, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(LAA_ex~grupo, data= doutorado1)
leveneTest(LAA_ex~grupo, data= doutorado1, center= mean)
fig_LAA_ex4= ggbetweenstats(doutorado1, x= grupo, y= LAA_ex,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", violin.args = list(width = 0, linewidth = 0), pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "LAESA (cm²)")
fig_LAA_ex4

# LAA_ex grupo_4
df= split(doutorado1$LAA_ex, doutorado1$grupo_4)
lapply(df, KS)
byf.shapiro(LAA_ex~grupo_4, data= doutorado1)
leveneTest(LAA_ex~grupo_4, data= doutorado1, center= mean)
fig_LAA_ex4= ggbetweenstats(doutorado1, x= grupo_4, y= LAA_ex,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", violin.args = list(width = 0, linewidth = 0), pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "LAESA (cm²)")
fig_LAA_ex4


# massa_VDE
df= split(doutorado1$massa_VDE, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(massa_VDE~grupo, data= doutorado1)
leveneTest(massa_VDE~grupo, data= doutorado1, center= mean)
fig_massa_VDE= ggbetweenstats(doutorado1, x= grupo, y= massa_VDE,  centrality.point.args = list(size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "Ventricle mass (g)")
fig_massa_VDE

# massa_VE
df= split(doutorado1$massa_VE, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(massa_VE~grupo, data= doutorado1)
leveneTest(massa_VE~grupo, data= doutorado1, center= mean)
fig_massa_VE= ggbetweenstats(doutorado1, x= grupo, y= massa_VE,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "LVM (g)")
fig_massa_VE

# massa_VD
df= split(doutorado1$massa_VD, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(massa_VD~grupo, data= doutorado1)
leveneTest(massa_VD~grupo, data= doutorado1, center= mean)
fig_massa_VD= ggbetweenstats(doutorado1, x= grupo, y= massa_VD,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "Right ventricle mass (g)")
fig_massa_VD

# massa_pulmao_umido
df= split(doutorado1$massa_pulmao_umido, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(massa_pulmao_umido~grupo, data= doutorado1)
leveneTest(massa_pulmao_umido~grupo, data= doutorado1, center= mean)
fig_massa_pulmao_umido= ggbetweenstats(doutorado1, x= grupo, y= massa_pulmao_umido,  centrality.point.args = list(size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "Wet lung mass (g)")
fig_massa_pulmao_umido

# massa_pulmao_seco
df= split(doutorado1$massa_pulmao_seco, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(massa_pulmao_seco~grupo, data= doutorado1)
leveneTest(massa_pulmao_seco~grupo, data= doutorado1, center= mean)
fig_massa_pulmao_seco= ggbetweenstats(doutorado1, x= grupo, y= massa_pulmao_seco,  centrality.point.args = list(size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "Dry lung mass (g)")
fig_massa_pulmao_seco

# massa_soleo
df= split(doutorado1$massa_soleo, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(massa_soleo~grupo, data= doutorado1)
leveneTest(massa_soleo~grupo, data= doutorado1, center= mean)
fig_massa_soleo= ggbetweenstats(doutorado1, x= grupo, y= massa_soleo,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "SMM (g)")
fig_massa_soleo

# peso_2VO2
df= split(doutorado1$peso_2VO2, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(peso_2VO2~grupo, data= doutorado1)
leveneTest(peso_2VO2~grupo, data= doutorado1, center= mean)
fig_peso_2VO2= ggbetweenstats(doutorado1, x= grupo, y= peso_2VO2,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "Weight in VO2peak (g)")
fig_peso_2VO2

# tempo_VO2
df= split(doutorado1$tempo_VO2, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(tempo_VO2~grupo, data= doutorado1)
leveneTest(tempo_VO2~grupo, data= doutorado1, center= mean)
fig_tempo_VO2= ggbetweenstats(doutorado1, x= grupo, y= tempo_VO2,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "Time to fatigue (s)")
fig_tempo_VO2

# tempo_VO2 grupo_4
df= split(doutorado1$tempo_VO2, doutorado1$grupo_4)
lapply(df, KS)
byf.shapiro(tempo_VO2~grupo_4, data= doutorado1)
leveneTest(tempo_VO2~grupo_4, data= doutorado1, center= mean)
fig_tempo_VO24= ggbetweenstats(doutorado1, x= grupo_4, y= tempo_VO2,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "Time to fatigue (s)")
fig_tempo_VO24

# vo2_pico
df= split(doutorado1$vo2_pico, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(vo2_pico~grupo, data= doutorado1)
leveneTest(vo2_pico~grupo, data= doutorado1, center= mean)
fig_vo2_pico= ggbetweenstats(doutorado1, x= grupo, y= vo2_pico,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = bquote(bold("VO"[2] * "peak" * "(ml/kg"^{0.75} * "/min)"))) +theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12),     # Tamanho da fonte do texto da legenda
  plot.margin = unit(c(0, 5, 0, 6), "cm")
)

fig_vo2_pico
plot_grid(fig_vo2_pico, labels = "A", label_size = 24)

# vo2_picoR
df= split(doutorado1$vo2_picoR, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(vo2_picoR~grupo, data= doutorado1)
leveneTest(vo2_picoR~grupo, data= doutorado1, center= mean)
fig_vo2_picoR= ggbetweenstats(doutorado1, x= grupo, y= vo2_picoR,  centrality.point.args = list(shape= 15, size= 4 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", violin.args = list(width = 0, linewidth = 0), point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), alpha =0.4, size = 2.5, stroke = 0, na.rm = TRUE), pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = bquote(bold("VO"[2] * "peak" * "(ml/kg"^{0.75} * "/min)"))) +theme(
  text = element_text(size = 9.5),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 12),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 11.5),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 9),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 10),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 12),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 10),     # Tamanho da fonte do texto da legenda
  plot.margin = unit(c(0, 5, 0, 6), "cm")
)

fig_vo2_picoR
plot_grid(fig_vo2_picoR, labels = "A", label_size = 24)



# vo2_pico grupo_4
df= split(doutorado1$vo2_pico, doutorado1$grupo_4)
lapply(df, KS)
byf.shapiro(vo2_pico~grupo_4, data= doutorado1)
leveneTest(vo2_pico~grupo_4, data= doutorado1, center= mean)
fig_vo2_pico4= ggbetweenstats(doutorado1, x= grupo_4, y= vo2_pico,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = bquote(bold("VO"[2]*"peak (ml/kg/min)")))
fig_vo2_pico4

# economia_corrida
df= split(doutorado1$economia_corrida, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(economia_corrida~grupo, data= doutorado1)
leveneTest(economia_corrida~grupo, data= doutorado1, center= mean)
fig_economia_corrida= ggbetweenstats(doutorado1, x= grupo, y= economia_corrida,  centrality.point.args = list(shape= 15, size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", bf.message= FALSE, xlab = "Groups", ylab = "Gross oxygen cost of running (ml/kg/m)")
fig_economia_corrida

# economia_corrida grupo_4
df= split(doutorado1$economia_corrida, doutorado1$grupo_4)
lapply(df, KS)
byf.shapiro(economia_corrida~grupo_4, data= doutorado1)
leveneTest(economia_corrida~grupo_4, data= doutorado1, center= mean)
fig_economia_corrida= ggbetweenstats(doutorado1, x= grupo_4, y= economia_corrida,  centrality.point.args = list(shape= 15, size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", bf.message= FALSE, xlab = "Groups", ylab = "Gross oxygen cost of running (ml/kg/m)")
fig_economia_corrida

# PVE_sist
df= split(doutorado1$PVE_sist, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(PVE_sist~grupo, data= doutorado1)
leveneTest(PVE_sist~grupo, data= doutorado1, center= mean)
fig_PVE_sist= ggbetweenstats(doutorado1, x= grupo, y= PVE_sist,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "LVSP (mmHg)") +theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12),     # Tamanho da fonte do texto da legenda
  plot.margin = unit(c(0, 5, 0, 6), "cm")
)
plot_grid(fig_PVE_sist, labels= "A", label_size = 24)

# PD1
df= split(doutorado1$PD1, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(PD1~grupo, data= doutorado1)
leveneTest(PD1~grupo, data= doutorado1, center= mean)
fig_PD1= ggbetweenstats(doutorado1, x= grupo, y= PD1,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "DP1 (mmHg)")
fig_PD1

# PD2
df= split(doutorado1$PD2, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(PD2~grupo, data= doutorado1)
leveneTest(PD2~grupo, data= doutorado1, center= mean)
fig_PD2= ggbetweenstats(doutorado1, x= grupo, y= PD2,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "LVEDP (mmHg)") +theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12),     # Tamanho da fonte do texto da legenda
  plot.margin = unit(c(0, 5, 0, 6), "cm")
)
plot_grid(fig_PD2, labels= "B", label_size = 24)

extract_stats(fig_PD2)

# FC_hem
df= split(doutorado1$FC_hem, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(FC_hem~grupo, data= doutorado1)
leveneTest(FC_hem~grupo, data= doutorado1, center= mean)
fig_FC_hem= ggbetweenstats(doutorado1, x= grupo, y= FC_hem,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", violin.args = list(width = 0, linewidth = 0), pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "HR (bpm)") +theme(
axis.text.y = element_text(size = 10), # Tamanho da fonte dos rótulos do eixo y
axis.text.x = element_text(size = 9),     # Tamanho da fonte dos rótulos do eixo x
legend.title = element_text(size = 12),    # Tamanho da fonte do título da legenda
legend.text = element_text(size = 6)     # Tamanho da fonte do texto da legenda
)

fig_FC_hem

extract_stats(fig_FC_hem)


# DP_dt_mais
df= split(doutorado1$DP_dt_mais, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(DP_dt_mais~grupo, data= doutorado1)
leveneTest(DP_dt_mais~grupo, data= doutorado1, center= mean)
fig_DP_dt_mais= ggbetweenstats(doutorado1, x= grupo, y= DP_dt_mais,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "+dP/dt (mmHg/s)") +theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12),     # Tamanho da fonte do texto da legenda
  plot.margin = unit(c(0, 5, 0, 6), "cm")
)
plot_grid(fig_DP_dt_mais, labels= "C", label_size = 24)

extract_stats(fig_DP_dt_mais)

# Dp_dt_menos
df= split(doutorado1$Dp_dt_menos, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(Dp_dt_menos~grupo, data= doutorado1)
leveneTest(Dp_dt_menos~grupo, data= doutorado1, center= mean)
fig_Dp_dt_menos= ggbetweenstats(doutorado1, x= grupo, y= Dp_dt_menos,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "-dP/dt (mmHg/s)") +theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12),     # Tamanho da fonte do texto da legenda
  plot.margin = unit(c(0, 5, 0, 6), "cm")
)
plot_grid(fig_Dp_dt_menos, labels= "D", label_size = 24)


# PAS
df= split(doutorado1$PAS, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(PAS~grupo, data= doutorado1)
leveneTest(PAS~grupo, data= doutorado1, center= mean)
fig_PAS= ggbetweenstats(doutorado1, x= grupo, y= PAS,  centrality.point.args = list(shape= 15, size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", bf.message= FALSE, xlab = "Groups", ylab = "SBP (mmHg)") +theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12),     # Tamanho da fonte do texto da legenda
  plot.margin = unit(c(0, 5, 0, 6), "cm")
)
plot_grid(fig_PAS, labels= "E", label_size = 24)
extract_stats(fig_PAS)

# PAD
df= split(doutorado1$PAD, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(PAD~grupo, data= doutorado1)
leveneTest(PAD~grupo, data= doutorado1, center= mean)
fig_PAD= ggbetweenstats(doutorado1, x= grupo, y= PAD,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "DBP (mmHg)") +theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12),     # Tamanho da fonte do texto da legenda
  plot.margin = unit(c(0, 5, 0, 6), "cm")
)
plot_grid(fig_PAD, labels= "F", label_size = 24)

# PAM
df= split(doutorado1$PAM, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(PAM~grupo, data= doutorado1)
leveneTest(PAM~grupo, data= doutorado1, center= mean)
fig_PAM= ggbetweenstats(doutorado1, x= grupo, y= PAM,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "MBP (mmHg)") +theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12),     # Tamanho da fonte do texto da legenda
  plot.margin = unit(c(0, 5, 0, 6), "cm")
)
plot_grid(fig_PAM, labels= "C", label_size = 24)

# IL_6_AR
df= split(doutorado1$IL_6_AR, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(IL_6_AR~grupo, data= doutorado1)
leveneTest(IL_6_AR~grupo, data= doutorado1, center= mean)
fig_IL_6_AR= ggbetweenstats(doutorado1, x= grupo, y= IL_6_AR,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "IL-6 (pg/mg prot)")
fig_IL_6_AR

# IL_6_AI
df= split(doutorado1$IL_6_AI, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(IL_6_AI~grupo, data= doutorado1)
leveneTest(IL_6_AI~grupo, data= doutorado1, center= mean)
fig_IL_6_AI= ggbetweenstats(doutorado1, x= grupo, y= IL_6_AI,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "IL-6 (pg/mg prot)")
fig_IL_6_AI

# IL_1_AR
df= split(doutorado1$IL_1_AR, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(IL_1_AR~grupo, data= doutorado1)
leveneTest(IL_1_AR~grupo, data= doutorado1, center= mean)
fig_IL_1_AR= ggbetweenstats(doutorado1, x= grupo, y= IL_1_AR,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "IL-1?? (pg/mg prot)")
fig_IL_1_AR

# IL_1_AI
df= split(doutorado1$IL_1_AI, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(IL_1_AI~grupo, data= doutorado1)
leveneTest(IL_1_AI~grupo, data= doutorado1, center= mean)
fig_IL_1_AI= ggbetweenstats(doutorado1, x= grupo, y= IL_1_AI,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "IL-1?? (pg/mg prot)")
fig_IL_1_AI

# IL_10_AR
df= split(doutorado1$IL_10_AR, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(IL_10_AR~grupo, data= doutorado1)
leveneTest(IL_10_AR~grupo, data= doutorado1, center= mean)
fig_IL_10_AR= ggbetweenstats(doutorado1, x= grupo, y= IL_10_AR,  centrality.point.args = list(shape= 15, size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "IL-10 (pg/mg prot)")
fig_IL_10_AR

# IL_10_AI
df= split(doutorado1$IL_10_AI, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(IL_10_AI~grupo, data= doutorado1)
leveneTest(IL_10_AI~grupo, data= doutorado1, center= mean)
fig_IL_10_AI= ggbetweenstats(doutorado1, x= grupo, y= IL_10_AI,  centrality.point.args = list(shape= 15, size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, xlab = "Groups", ylab = "IL-10 (pg/mg prot)")
fig_IL_10_AI

# TNF_AR
df= split(doutorado1$TNF_AR, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(TNF_AR~grupo, data= doutorado1)
leveneTest(TNF_AR~grupo, data= doutorado1, center= mean)
fig_TNF_AR= ggbetweenstats(doutorado1, x= grupo, y= TNF_AR,  centrality.point.args = list(size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "TNF-alfa (pg/mg prot)")
fig_TNF_AR

# TNF_AI
df= split(doutorado1$TNF_AI, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(TNF_AI~grupo, data= doutorado1)
leveneTest(TNF_AI~grupo, data= doutorado1, center= mean)
fig_TNF_AI= ggbetweenstats(doutorado1, x= grupo, y= TNF_AI,  centrality.point.args = list(size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= FALSE, bf.message= FALSE, xlab = "Groups", ylab = "TNF-alfa (pg/mg prot)")
fig_TNF_AI

# GPX_AR
df= split(doutorado1$GPX_AR, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(GPX_AR~grupo, data= doutorado1)
leveneTest(GPX_AR~grupo, data= doutorado1, center= mean)
fig_GPX_AR= ggbetweenstats(doutorado1, x= grupo, y= GPX_AR,  centrality.point.args = list(size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "GPX (U/mg prot)")
fig_GPX_AR

# GPX_AI
df= split(doutorado1$GPX_AI, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(GPX_AI~grupo, data= doutorado1)
leveneTest(GPX_AI~grupo, data= doutorado1, center= mean)
fig_GPX_AI= ggbetweenstats(doutorado1, x= grupo, y= GPX_AI,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "GPX (U/mg prot)")
fig_GPX_AI

# CAT_AR
df= split(doutorado1$CAT_AR, doutorado1$grupo)
lapply(df, KS)
shapiro.test(doutorado1$CAT_AR)
byf.shapiro(CAT_AR~grupo, data= doutorado1)
leveneTest(CAT_AR~grupo, data= doutorado1, center= mean)
fig_CAT_AR= ggbetweenstats(doutorado1, x= grupo, y= CAT_AR,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "Catalase (nmol/min/mg prot)")
fig_CAT_AR

# CAT_AI
df= split(doutorado1$CAT_AI, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(CAT_AI~grupo, data= doutorado1)
leveneTest(CAT_AI~grupo, data= doutorado1, center= mean)
fig_CAT_AI= ggbetweenstats(doutorado1, x= grupo, y= CAT_AI,  centrality.point.args = list(size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "Catalase (nmol/min/mg prot)")
fig_CAT_AI

# SOD_AR
df= split(doutorado1$SOD_AR, doutorado1$grupo)
lapply(df, KS)
shapiro.test(doutorado1$SOD_AR)
byf.shapiro(SOD_AR~grupo, data= doutorado1)
leveneTest(SOD_AR~grupo, data= doutorado1, center= mean)
fig_SOD_AR= ggbetweenstats(doutorado1, x= grupo, y= SOD_AR,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "SOD (U/mg prot)")
fig_SOD_AR

# SOD_AI
df= split(doutorado1$SOD_AI, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(SOD_AI~grupo, data= doutorado1)
leveneTest(SOD_AI~grupo, data= doutorado1, center= mean)
fig_SOD_AI= ggbetweenstats(doutorado1, x= grupo, y= SOD_AI,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "SOD (U/mg prot)")
fig_SOD_AI

# VEGF_AR
df= split(doutorado1$VEGF_AR, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(VEGF_AR~grupo, data= doutorado1)
leveneTest(VEGF_AR~grupo, data= doutorado1, center= mean)
fig_VEGF_AR= ggbetweenstats(doutorado1, x= grupo, y= VEGF_AR,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "VEGF (pg/mg prot)")
fig_VEGF_AR

# VEGF_AI
df= split(doutorado1$VEGF_AI, doutorado1$grupo)
lapply(df, KS)
shapiro.test(doutorado1$VEGF_AI)
byf.shapiro(VEGF_AI~grupo, data= doutorado1)
leveneTest(VEGF_AI~grupo, data= doutorado1, center= mean)
fig_VEGF_AI= ggbetweenstats(doutorado1, x= grupo, y= VEGF_AI,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "VEGF (pg/mg prot)")+theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12) # Tamanho da fonte do texto da legenda
)
plot_grid(fig_VEGF_AI, labels= "A", label_size = 24)



# endoglin_AR
df= split(doutorado1$endoglin_AR, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(endoglin_AR~grupo, data= doutorado1)
KS(dados_MI$endoglin_AR)
KS(dados_T60$endoglin_AR)
KS(dados_T90$endoglin_AR)
KS(dados_T180$endoglin_AR)
leveneTest(endoglin_AR~grupo, data= doutorado1, center= mean)
fig_endoglin_AR= ggbetweenstats(doutorado1, x= grupo, y= endoglin_AR,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "Endoglin (pg/mg prot)")
fig_endoglin_AR

# endoglin_AI
df= split(doutorado1$endoglin_AI, doutorado1$grupo)
lapply(df, KS)
shapiro.test(doutorado1$endoglin_AI)
byf.shapiro(endoglin_AI~grupo, data= doutorado1)
leveneTest(endoglin_AI~grupo, data= doutorado1, center= mean)
fig_endoglin_AI= ggbetweenstats(doutorado1, x= grupo, y= endoglin_AI,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "Endoglin (pg/mg prot)")+theme(
text = element_text(size = 12),           # Tamanho da fonte geral
axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
legend.text = element_text(size = 12)   # Tamanho da fonte do texto da legenda
)
plot_grid(fig_endoglin_AI, labels= "B", label_size = 24)
extract_stats(fig_endoglin_AI)

# COL1_GAPDH_RAZAO_AR
df= split(doutorado1$COL1_GAPDH_RAZAO_AR, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(COL1_GAPDH_RAZAO_AR~grupo, data= doutorado1)
leveneTest(COL1_GAPDH_RAZAO_AR~grupo, data= doutorado1, center= mean)
fig_COL1_GAPDH_RAZAO_AR= ggbetweenstats(doutorado1, x= grupo, y= COL1_GAPDH_RAZAO_AR,  centrality.point.args = list(shape= 15, size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", violin.args = list(width = 0, linewidth = 0), pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", bf.message= FALSE, xlab = "Groups", ylab = "COL1/GAPDH")+theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12),     # Tamanho da fonte do texto da legenda
)
plot_grid(fig_COL1_GAPDH_RAZAO_AR, labels= "A", label_size = 24)
extract_stats(fig_COL1_GAPDH_RAZAO_AR)

# COL3_GAPDH_RAZAO_AR
df= split(doutorado1$COL3_GAPDH_RAZAO_AR, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(COL3_GAPDH_RAZAO_AR~grupo, data= doutorado1)
leveneTest(COL3_GAPDH_RAZAO_AR~grupo, data= doutorado1, center= mean)
fig_COL3_GAPDH_RAZAO_AR= ggbetweenstats(doutorado1, x= grupo, y= COL3_GAPDH_RAZAO_AR,  centrality.point.args = list(shape= 15, size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", violin.args = list(width = 0, linewidth = 0), pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", bf.message= FALSE, xlab = "Groups", ylab = "COL3/GAPDH")+theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12),     # Tamanho da fonte do texto da legenda
)
plot_grid(fig_COL3_GAPDH_RAZAO_AR, labels= "A", label_size = 24)
extract_stats(fig_COL3_GAPDH_RAZAO_AR)

# COL1_GAPDH_RAZAO_AI
df= split(doutorado1$COL1_GAPDH_RAZAO_AI, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(COL1_GAPDH_RAZAO_AI~grupo, data= doutorado1)
leveneTest(COL1_GAPDH_RAZAO_AI~grupo, data= doutorado1, center= mean)
fig_COL1_GAPDH_RAZAO_AI= ggbetweenstats(doutorado1, x= grupo, y= COL1_GAPDH_RAZAO_AI,  centrality.point.args = list(shape= 15, size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", violin.args = list(width = 0, linewidth = 0), pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", bf.message= FALSE, xlab = "Groups", ylab = "COL1/GAPDH")+theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12),     # Tamanho da fonte do texto da legenda
)
plot_grid(fig_COL1_GAPDH_RAZAO_AI, labels= "A", label_size = 24)
extract_stats(fig_COL1_GAPDH_RAZAO_AI)

# COL3_GAPDH_RAZAO_AI
df= split(doutorado1$COL3_GAPDH_RAZAO_AI, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(COL3_GAPDH_RAZAO_AI~grupo, data= doutorado1)
leveneTest(COL3_GAPDH_RAZAO_AI~grupo, data= doutorado1, center= mean)
fig_COL3_GAPDH_RAZAO_AI= ggbetweenstats(doutorado1, x= grupo, y= COL3_GAPDH_RAZAO_AI,  centrality.point.args = list(shape= 15, size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", violin.args = list(width = 0, linewidth = 0), pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", bf.message= FALSE, xlab = "Groups", ylab = "COL3/GAPDH")+theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12),     # Tamanho da fonte do texto da legenda
)
plot_grid(fig_COL3_GAPDH_RAZAO_AI, labels= "A", label_size = 24)
extract_stats(fig_COL3_GAPDH_RAZAO_AI)

# alfa_MHC
df= split(doutorado1$alfa_MHC, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(alfa_MHC~grupo, data= doutorado1)
leveneTest(alfa_MHC~grupo, data= doutorado1, center= mean)
fig_alfa_MHC= ggbetweenstats(doutorado1, x= grupo, y= alfa_MHC,  centrality.point.args = list(shape= 15, size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", violin.args = list(width = 0, linewidth = 0), pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", bf.message= FALSE, xlab = "Groups", ylab = "alfa-MHC")+theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12),     # Tamanho da fonte do texto da legenda
)
plot_grid(fig_alfa_MHC, labels= "A", label_size = 24)
extract_stats(fig_alfa_MHC)

# PLBT_GAPDH_RAZAO_AR
df= split(doutorado1$PLBT_GAPDH_RAZAO_AR, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(PLBT_GAPDH_RAZAO_AR~grupo, data= doutorado1)
leveneTest(PLBT_GAPDH_RAZAO_AR~grupo, data= doutorado1, center= mean)
fig_PLBT_GAPDH_RAZAO_AR= ggbetweenstats(doutorado1, x= grupo, y= PLBT_GAPDH_RAZAO_AR,  centrality.point.args = list(shape= 15, size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", bf.message= FALSE, xlab = "Groups", ylab = "PLB/GAPDH")+theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12),     # Tamanho da fonte do texto da legenda
)
plot_grid(fig_PLBT_GAPDH_RAZAO_AR, labels= "A", label_size = 24)
extract_stats(fig_PLBT_GAPDH_RAZAO_AR)


# PLBT_GAPDH_RAZAO_AI
df= split(doutorado1$PLBT_GAPDH_RAZAO_AI, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(PLBT_GAPDH_RAZAO_AI~grupo, data= doutorado1)
leveneTest(PLBT_GAPDH_RAZAO_AI~grupo, data= doutorado1, center= mean)
fig_PLBT_GAPDH_RAZAO_AI= ggbetweenstats(doutorado1, x= grupo, y= PLBT_GAPDH_RAZAO_AI,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "PLB/GAPDH")+theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12)     # Tamanho da fonte do texto da legenda
)
plot_grid(fig_PLBT_GAPDH_RAZAO_AI, labels= "A", label_size = 24)


# PLBP_GAPDH_RAZAO_AR
df= split(doutorado1$PLBP_GAPDH_RAZAO_AR, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(PLBP_GAPDH_RAZAO_AR~grupo, data= doutorado1)
leveneTest(PLBP_GAPDH_RAZAO_AR~grupo, data= doutorado1, center= mean)
fig_PLBP_GAPDH_RAZAO_AR= ggbetweenstats(doutorado1, x= grupo, y= PLBP_GAPDH_RAZAO_AR,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "PLB(Thr17/Ser16)/GAPDH")+theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12)     # Tamanho da fonte do texto da legenda
)
plot_grid(fig_PLBP_GAPDH_RAZAO_AR, labels= "B", label_size = 24)

# PLBP_GAPDH_RAZAO_AI
df= split(doutorado1$PLBP_GAPDH_RAZAO_AI, doutorado1$grupo)
lapply(df, KS)
shapiro.test(doutorado1$PLBP_GAPDH_RAZAO_AI)
byf.shapiro(PLBP_GAPDH_RAZAO_AI~grupo, data= doutorado1)
leveneTest(PLBP_GAPDH_RAZAO_AI~grupo, data= doutorado1, center= mean)
fig_PLBP_GAPDH_RAZAO_AI= ggbetweenstats(doutorado1, x= grupo, y= PLBP_GAPDH_RAZAO_AI,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "PLB(Thr17/Ser16)/GAPDH") + theme(
text = element_text(size = 12),           # Tamanho da fonte geral
axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
legend.text = element_text(size = 12)     # Tamanho da fonte do texto da legenda
)
plot_grid(fig_PLBP_GAPDH_RAZAO_AI, labels= "B", label_size = 24)


fig_PLBP_GAPDH_RAZAO_AI
extract_stats(fig_PLBP_GAPDH_RAZAO_AI)


# NCX_GAPDH_RAZAO_AR
df= split(doutorado1$NCX_GAPDH_RAZAO_AR, doutorado1$grupo)
lapply(df, KS)
shapiro.test(doutorado1$NCX_GAPDH_RAZAO_AR)
byf.shapiro(NCX_GAPDH_RAZAO_AR~grupo, data= doutorado1)
leveneTest(NCX_GAPDH_RAZAO_AR~grupo, data= doutorado1, center= mean)
fig_NCX_GAPDH_RAZAO_AR= ggbetweenstats(doutorado1, x= grupo, y= NCX_GAPDH_RAZAO_AR,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "NCX1/GAPDH") + theme(
text = element_text(size = 12),           # Tamanho da fonte geral
axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
legend.text = element_text(size = 12)     # Tamanho da fonte do texto da legenda
)
plot_grid(fig_NCX_GAPDH_RAZAO_AR, labels= "C", label_size = 24)

fig_NCX_GAPDH_RAZAO_AR

# NCX_GAPDH_RAZAO_AI
df= split(doutorado1$NCX_GAPDH_RAZAO_AI, doutorado1$grupo)
lapply(df, KS)
shapiro.test(doutorado1$NCX_GAPDH_RAZAO_AI)
byf.shapiro(NCX_GAPDH_RAZAO_AI~grupo, data= doutorado1)
leveneTest(NCX_GAPDH_RAZAO_AI~grupo, data= doutorado1, center= mean)
fig_NCX_GAPDH_RAZAO_AI= ggbetweenstats(doutorado1, x= grupo, y= NCX_GAPDH_RAZAO_AI,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= FALSE, bf.message= FALSE, xlab = "Groups", ylab = "NCX1/GAPDH") + theme(
text = element_text(size = 12),           # Tamanho da fonte geral
axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
legend.text = element_text(size = 12)     # Tamanho da fonte do texto da legenda
)
plot_grid(fig_NCX_GAPDH_RAZAO_AI, labels= "C", label_size = 24)

extract_stats(fig_NCX_GAPDH_RAZAO_AI)

# HNE_GAPDH_RAZAO_AR
df= split(doutorado1$HNE_GAPDH_RAZAO_AR, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(HNE_GAPDH_RAZAO_AR~grupo, data= doutorado1)
leveneTest(HNE_GAPDH_RAZAO_AR~grupo, data= doutorado1, center= mean)
fig_HNE_GAPDH_RAZAO_AR= ggbetweenstats(doutorado1, x= grupo, y= HNE_GAPDH_RAZAO_AR,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "4-HNE/GAPDH")
fig_HNE_GAPDH_RAZAO_AR

# HNE_GAPDH_RAZAO_AI
df= split(doutorado1$HNE_GAPDH_RAZAO_AI, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(HNE_GAPDH_RAZAO_AI~grupo, data= doutorado1)
leveneTest(HNE_GAPDH_RAZAO_AI~grupo, data= doutorado1, center= mean)
fig_HNE_GAPDH_RAZAO_AI= ggbetweenstats(doutorado1, x= grupo, y= HNE_GAPDH_RAZAO_AI,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "4-HNE/GAPDH")
fig_HNE_GAPDH_RAZAO_AI

# SERCA_GAPDH_RAZAO_AR
df= split(doutorado1$SERCA_GAPDH_RAZAO_AR, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(SERCA_GAPDH_RAZAO_AR~grupo, data= doutorado1)
leveneTest(SERCA_GAPDH_RAZAO_AR~grupo, data= doutorado1, center= mean)
fig_SERCA_GAPDH_RAZAO_AR= ggbetweenstats(doutorado1, x= grupo, y= SERCA_GAPDH_RAZAO_AR,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "SERCA2a/GAPDH") + theme(
text = element_text(size = 12),           # Tamanho da fonte geral
axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
legend.text = element_text(size = 12)     # Tamanho da fonte do texto da legenda
)
plot_grid(fig_SERCA_GAPDH_RAZAO_AR, labels= "D", label_size = 24)

fig_SERCA_GAPDH_RAZAO_AR

# SERCA_GAPDH_RAZAO_AI
df= split(doutorado1$SERCA_GAPDH_RAZAO_AI, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(SERCA_GAPDH_RAZAO_AI~grupo, data= doutorado1)
leveneTest(SERCA_GAPDH_RAZAO_AI~grupo, data= doutorado1, center= mean)
fig_SERCA_GAPDH_RAZAO_AI= ggbetweenstats(doutorado1, x= grupo, y= SERCA_GAPDH_RAZAO_AI,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "SERCA2a/GAPDH") + theme(
text = element_text(size = 12),           # Tamanho da fonte geral
axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
legend.text = element_text(size = 12)     # Tamanho da fonte do texto da legenda
)
plot_grid(fig_SERCA_GAPDH_RAZAO_AI, labels= "D", label_size = 24)



# CAV_GAPDH_RAZAO_AR
df= split(doutorado1$CAV_GAPDH_RAZAO_AR, doutorado1$grupo)
lapply(df, KS)
shapiro.test(doutorado1$CAV_GAPDH_RAZAO_AR)
byf.shapiro(CAV_GAPDH_RAZAO_AR~grupo, data= doutorado1)
leveneTest(CAV_GAPDH_RAZAO_AR~grupo, data= doutorado1, center= mean)
fig_CAV_GAPDH_RAZAO_AR= ggbetweenstats(doutorado1, x= grupo, y= CAV_GAPDH_RAZAO_AR,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "LTCC/GAPDH") + theme(
text = element_text(size = 12),           # Tamanho da fonte geral
axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
legend.text = element_text(size = 12)     # Tamanho da fonte do texto da legenda
)
plot_grid(fig_CAV_GAPDH_RAZAO_AR, labels= "E", label_size = 24)

fig_CAV_GAPDH_RAZAO_AR

# CAV_GAPDH_RAZAO_AI
df= split(doutorado1$CAV_GAPDH_RAZAO_AI, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(CAV_GAPDH_RAZAO_AI~grupo, data= doutorado1)
leveneTest(CAV_GAPDH_RAZAO_AI~grupo, data= doutorado1, center= mean)
fig_CAV_GAPDH_RAZAO_AI= ggbetweenstats(doutorado1, x= grupo, y= CAV_GAPDH_RAZAO_AI,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "LTCC/GAPDH") + theme(
text = element_text(size = 12),           # Tamanho da fonte geral
axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
legend.text = element_text(size = 12)     # Tamanho da fonte do texto da legenda
)
plot_grid(fig_CAV_GAPDH_RAZAO_AI, labels= "E", label_size = 24)


fig_CAV_GAPDH_RAZAO_AI

# RyR_GAPDH_RAZAO_AR
df= split(doutorado1$RyR_GAPDH_RAZAO_AR, doutorado1$grupo)
lapply(df, KS)
shapiro.test(doutorado1$RyR_GAPDH_RAZAO_AR)
byf.shapiro(RyR_GAPDH_RAZAO_AR~grupo, data= doutorado1)
leveneTest(RyR_GAPDH_RAZAO_AR~grupo, data= doutorado1, center= mean)
fig_RyR_GAPDH_RAZAO_AR= ggbetweenstats(doutorado1, x= grupo, y= RyR_GAPDH_RAZAO_AR,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "RyR/GAPDH") + theme(
text = element_text(size = 12),           # Tamanho da fonte geral
axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
legend.text = element_text(size = 12)     # Tamanho da fonte do texto da legenda
)
plot_grid(fig_RyR_GAPDH_RAZAO_AR, labels= "F", label_size = 24)

fig_RyR_GAPDH_RAZAO_AR

# RyR_GAPDH_RAZAO_AI
df= split(doutorado1$RyR_GAPDH_RAZAO_AI, doutorado1$grupo)
lapply(df, KS)
shapiro.test(doutorado1$RyR_GAPDH_RAZAO_AI)
byf.shapiro(RyR_GAPDH_RAZAO_AI~grupo, data= doutorado1)
leveneTest(RyR_GAPDH_RAZAO_AI~grupo, data= doutorado1, center= mean)
fig_RyR_GAPDH_RAZAO_AI= ggbetweenstats(doutorado1, x= grupo, y= RyR_GAPDH_RAZAO_AI,  centrality.point.args = list(shape= 15, size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", bf.message= FALSE, xlab = "Groups", ylab = "RyR/GAPDH") + theme(
text = element_text(size = 12),           # Tamanho da fonte geral
axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
legend.text = element_text(size = 12)     # Tamanho da fonte do texto da legenda
)
plot_grid(fig_RyR_GAPDH_RAZAO_AI, labels= "F", label_size = 24)




# massa_soleo_corrigida
df= split(doutorado1$massa_soleo_corrigida, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(massa_soleo_corrigida~grupo, data= doutorado1)
leveneTest(massa_soleo_corrigida~grupo, data= doutorado1, center= mean)
fig_massa_soleo_corrigida= ggbetweenstats(doutorado1, x= grupo, y= massa_soleo_corrigida,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "SMM (mg/g body mass)")
fig_massa_soleo_corrigida

# massa_VE_corrigida
df= split(doutorado1$massa_VE_corrigida, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(massa_VE_corrigida~grupo, data= doutorado1)
leveneTest(massa_VE_corrigida~grupo, data= doutorado1, center= mean)
fig_massa_VE_corrigida= ggbetweenstats(doutorado1, x= grupo, y= massa_VE_corrigida,  centrality.point.args = list(size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "LVM (mg/g BM)")+theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12),     # Tamanho da fonte do texto da legenda
  plot.margin = unit(c(0, 5, 0, 6), "cm")
)
plot_grid(fig_massa_VE_corrigida, labels= "B", label_size = 24)

# massa_VD_corrigida
df= split(doutorado1$massa_VD_corrigida, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(massa_VD_corrigida~grupo, data= doutorado1)
leveneTest(massa_VD_corrigida~grupo, data= doutorado1, center= mean)
fig_massa_VD_corrigida= ggbetweenstats(doutorado1, x= grupo, y= massa_VD_corrigida,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "RVM (mg/g BM)")+theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12),     # Tamanho da fonte do texto da legenda
  plot.margin = unit(c(0, 5, 0, 6), "cm")
)
plot_grid(fig_massa_VD_corrigida, labels= "C", label_size = 24)


# teor_agua_pulmao
df= split(doutorado1$teor_agua_pulmao, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(teor_agua_pulmao~grupo, data= doutorado1)
leveneTest(teor_agua_pulmao~grupo, data= doutorado1, center= mean)
fig_teor_agua_pulmao= ggbetweenstats(doutorado1, x= grupo, y= teor_agua_pulmao,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "LWC (%)")+theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12),     # Tamanho da fonte do texto da legenda
  plot.margin = unit(c(0, 5, 0, 6), "cm")
)
plot_grid(fig_teor_agua_pulmao, labels= "D", label_size = 24)



### Teste de sensibilidade eco3 todos os dados

# massa_ECO3
df= split(doutorado1$massa_ECO3, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(massa_ECO3~grupo, data= doutorado1)
leveneTest(massa_ECO3~grupo, data= doutorado1, center= mean)
fig_massa_ECO3= ggbetweenstats(doutorado1, x= grupo, y= massa_ECO3,  centrality.point.args = list(shape= 15, size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "Massa corporal (g)")
fig_massa_ECO3

# FC
df= split(doutorado1$FC, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(FC~grupo, data= doutorado1)
leveneTest(FC~grupo, data= doutorado1, center= mean)
fig_FC_sens= ggbetweenstats(doutorado1, x= grupo, y= FC,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "HR eco (bpm)")
fig_FC_sens

extract_stats(fig_FC_sens)

# Ao_M
df= split(doutorado1$Ao_M, doutorado1$grupo)
lapply(df, KS)
shapiro.test(doutorado1$Ao_M)
byf.shapiro(Ao_M~grupo, data= doutorado1)
leveneTest(Ao_M~grupo, data= doutorado1, center= mean)
fig_Ao_M_sens= ggbetweenstats(doutorado1, x= grupo, y= Ao_M,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "AoD-M (cm)")
fig_Ao_M_sens

# AE_M
df= split(doutorado1$AE_M, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(AE_M~grupo, data= doutorado1)
leveneTest(AE_M~grupo, data= doutorado1, center= mean)
fig_AE_M_sens= ggbetweenstats(doutorado1, x= grupo, y= AE_M,  centrality.point.args = list(size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "LAESD-M (cm)")
fig_AE_M_sens

# FP
df= split(doutorado1$FP, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(FP~grupo, data= doutorado1)
leveneTest(FP~grupo, data= doutorado1, center= mean)
fig_FP_sens= ggbetweenstats(doutorado1, x= grupo, y= FP,  centrality.point.args = list(size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "Pumonary artery peak flow velocity (m/s)")
fig_FP_sens

# GP
df= split(doutorado1$GP, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(GP~grupo, data= doutorado1)
leveneTest(GP~grupo, data= doutorado1, center= mean)
fig_GP_sens= ggbetweenstats(doutorado1, x= grupo, y= GP,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "Pulmonary artery gradient (mmHg)")
fig_GP_sens

# E
df= split(doutorado1$E, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(E~grupo, data= doutorado1)
leveneTest(E~grupo, data= doutorado1, center= mean)
fig_E_sens= ggbetweenstats(doutorado1, x= grupo, y= E,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "E-wave (m/s)")
fig_E_sens

# A
df= split(doutorado1$A, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(A~grupo, data= doutorado1)
leveneTest(A~grupo, data= doutorado1, center= mean)
fig_A_sens= ggbetweenstats(doutorado1, x= grupo, y= A,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "A-wave (m/s)")
fig_A_sens

# E_A
df= split(doutorado1$E_A, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(E_A~grupo, data= doutorado1)
leveneTest(E_A~grupo, data= doutorado1, center= mean)
fig_E_A_sens= ggbetweenstats(doutorado1, x= grupo, y= E_A,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "E/A")
fig_E_A_sens

# TDE
df= split(doutorado1$TDE, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(TDE~grupo, data= doutorado1)
leveneTest(TDE~grupo, data= doutorado1, center= mean)
fig_TDE_sens= ggbetweenstats(doutorado1, x= grupo, y= TDE,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "DT (ms)")
fig_TDE_sens

# TRIV
df= split(doutorado1$TRIV, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(TRIV~grupo, data= doutorado1)
leveneTest(TRIV~grupo, data= doutorado1, center= mean)
fig_TRIV_sens= ggbetweenstats(doutorado1, x= grupo, y= TRIV,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "IVRT (ms)")
fig_TRIV_sens

# FA
df= split(doutorado1$FA, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(FA~grupo, data= doutorado1)
leveneTest(FA~grupo, data= doutorado1, center= mean)
fig_FA_sens= ggbetweenstats(doutorado1, x= grupo, y= FA,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "Aortic peak flow velocity (m/s)")
fig_FA_sens

# GA
df= split(doutorado1$GA, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(GA~grupo, data= doutorado1)
leveneTest(GA~grupo, data= doutorado1, center= mean)
fig_GA_sens= ggbetweenstats(doutorado1, x= grupo, y= GA,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "Aortic gradient (mmHg)")
fig_GA_sens

# Ao_B
df= split(doutorado1$Ao_B, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(Ao_B~grupo, data= doutorado1)
leveneTest(Ao_B~grupo, data= doutorado1, center= mean)
fig_Ao_B_sens= ggbetweenstats(doutorado1, x= grupo, y= Ao_B,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "AoD-B (cm)")
fig_Ao_B_sens

# AE_B
df= split(doutorado1$AE_B, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(AE_B~grupo, data= doutorado1)
leveneTest(AE_B~grupo, data= doutorado1, center= mean)
fig_AE_B_sens= ggbetweenstats(doutorado1, x= grupo, y= AE_B,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "LAESD-B (cm)")
fig_AE_B_sens

# area_AE
df= split(doutorado1$area_AE, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(area_AE~grupo, data= doutorado1)
leveneTest(area_AE~grupo, data= doutorado1, center= mean)
fig_area_AE_sens= ggbetweenstats(doutorado1, x= grupo, y= area_AE,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "LAESA (cm²)") +theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12),     # Tamanho da fonte do texto da legenda
  plot.margin = unit(c(0, 5, 0, 6), "cm")
)
fig_area_AE_sens

# ELD
df= split(doutorado1$ELD, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(ELD~grupo, data= doutorado1)
leveneTest(ELD~grupo, data= doutorado1, center= mean)
fig_ELD_sens= ggbetweenstats(doutorado1, x= grupo, y= ELD,  centrality.point.args = list(shape= 15, size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "LVFWT (cm)")
fig_ELD_sens

# ESD
df= split(doutorado1$ESD, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(ESD~grupo, data= doutorado1)
leveneTest(ESD~grupo, data= doutorado1, center= mean)
fig_ESD_sens= ggbetweenstats(doutorado1, x= grupo, y= ESD,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "ISWT (cm)")
fig_ESD_sens

# DVED
df= split(doutorado1$DVED, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(DVED~grupo, data= doutorado1)
leveneTest(DVED~grupo, data= doutorado1, center= mean)
fig_DVED_sens= ggbetweenstats(doutorado1, x= grupo, y= DVED,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "LVEDD (cm)")
fig_DVED_sens

# DVES
df= split(doutorado1$DVES, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(DVES~grupo, data= doutorado1)
leveneTest(DVES~grupo, data= doutorado1, center= mean)
fig_DVES_sens= ggbetweenstats(doutorado1, x= grupo, y= DVES,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "LVESD (cm)")
fig_DVES_sens

# Fenc
df= split(doutorado1$Fenc, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(Fenc~grupo, data= doutorado1)
leveneTest(Fenc~grupo, data= doutorado1, center= mean)
fig_Fenc_sens= ggbetweenstats(doutorado1, x= grupo, y= Fenc,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "SF (%)")
fig_Fenc_sens

# FEJ
df= split(doutorado1$FEJ, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(FEJ~grupo, data= doutorado1)
leveneTest(FEJ~grupo, data= doutorado1, center= mean)
fig_FEJ_sens= ggbetweenstats(doutorado1, x= grupo, y= FEJ,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "EF (%)")
fig_FEJ_sens

# IM
df= split(doutorado1$IM, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(IM~grupo, data= doutorado1)
leveneTest(IM~grupo, data= doutorado1, center= mean)
KS(dados_MI$IM)
KS(dados_T60$IM)
KS(dados_T90$IM)
KS(dados_T180$IM)

fig_IM_sens= ggbetweenstats(doutorado1, x= grupo, y= IM,  centrality.point.args = list(shape= 15, size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = " MIS (%)")
fig_IM_sens

# area_VE_diast
df= split(doutorado1$area_VE_diast, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(area_VE_diast~grupo, data= doutorado1)
leveneTest(area_VE_diast~grupo, data= doutorado1, center= mean)
fig_area_VE_diast_sens= ggbetweenstats(doutorado1, x= grupo, y= area_VE_diast,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "LVEDA (cm²)") +theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12),     # Tamanho da fonte do texto da legenda
  plot.margin = unit(c(0, 5, 0, 6), "cm")
)
fig_area_VE_diast_sens

# area_VE_sist
df= split(doutorado1$area_VE_sist, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(area_VE_sist~grupo, data= doutorado1)
leveneTest(area_VE_sist~grupo, data= doutorado1, center= mean)
fig_area_VE_sist_sens= ggbetweenstats(doutorado1, x= grupo, y= area_VE_sist,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "LVESA (cm²)") +theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12),     # Tamanho da fonte do texto da legenda
  plot.margin = unit(c(0, 5, 0, 6), "cm")
)

fig_area_VE_sist_sens

# FEAT
df= split(doutorado1$FEAT, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(FEAT~grupo, data= doutorado1)
leveneTest(FEAT~grupo, data= doutorado1, center= mean)
fig_FEAT_sens= ggbetweenstats(doutorado1, x= grupo, y= FEAT,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "FAC (%)")+theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12),     # Tamanho da fonte do texto da legenda
  plot.margin = unit(c(0, 5, 0, 6), "cm")
)

fig_FEAT_sens

# CDM
df= split(doutorado1$CDM, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(CDM~grupo, data= doutorado1)
leveneTest(CDM~grupo, data= doutorado1, center= mean)
fig_CDM_sens= ggbetweenstats(doutorado1, x= grupo, y= CDM,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "LVEDC-papillary (cm)")
fig_CDM_sens

# CSM
df= split(doutorado1$CSM, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(CSM~grupo, data= doutorado1)
leveneTest(CSM~grupo, data= doutorado1, center= mean)
fig_CSM_sens= ggbetweenstats(doutorado1, x= grupo, y= CSM,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "LVESC-papillary (cm)")
fig_CSM_sens

# AM
df= split(doutorado1$AM, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(AM~grupo, data= doutorado1)
leveneTest(AM~grupo, data= doutorado1, center= mean)
KS(dados_MI$AM)
KS(dados_T60$AM)
KS(dados_T90$AM)
KS(dados_T180$AM)

fig_AM_sens= ggbetweenstats(doutorado1, x= grupo, y= AM,  centrality.point.args = list(shape= 15, size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "MIEDL-pipillary (cm)")
fig_AM_sens

# CDA
df= split(doutorado1$CDA, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(CDA~grupo, data= doutorado1)
leveneTest(CDA~grupo, data= doutorado1, center= mean)
fig_CDA_sens= ggbetweenstats(doutorado1, x= grupo, y= CDA,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "LVEDC-apical (cm)")
fig_CDA_sens

# CSA
df= split(doutorado1$CSA, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(CSA~grupo, data= doutorado1)
leveneTest(CSA~grupo, data= doutorado1, center= mean)
fig_CSA_sens= ggbetweenstats(doutorado1, x= grupo, y= CSA,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "LVESC-apical (cm)")
fig_CSA_sens

# AA
df= split(doutorado1$AA, doutorado1$grupo)
lapply(df, KS)
byf.shapiro(AA~grupo, data= doutorado1)
leveneTest(AA~grupo, data= doutorado1, center= mean)
KS(dados_MI$AA)
KS(dados_T60$AA)
KS(dados_T90$AA)
KS(dados_T180$AA)

fig_AA_sens= ggbetweenstats(doutorado1, x= grupo, y= AA,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "MIEDL-apical (cm)")
fig_AA_sens




# Criar uma planilha sem a definição das siglas
doutorado2= doutorado[-1,]

doutorado2$lote= as.numeric(doutorado2$lote)
doutorado2= filter(doutorado2, lote < 10)
View(doutorado2)

# Tornar a variável grupo um fator e ordenar os grupos
doutorado2$grupo = factor(doutorado2$grupo, c("SHAM", "NT+MI", "T60+MI", "T90+MI", "T180+MI"))

#transformar as variáveis para tipo numérico
doutorado2= mutate_if(doutorado2, is.character, as.numeric)

# Cria um data frame pra cada grupo

dados_SHAM_10= data.frame(doutorado1[doutorado1$grupo== "SHAM",])
dados_MI_10= data.frame(doutorado1[doutorado1$grupo== "NT+MI",])
dados_T60_10= data.frame(doutorado1[doutorado1$grupo== "T60+MI",])
dados_T90_10= data.frame(doutorado1[doutorado1$grupo== "T90+MI",])
dados_T180_10= data.frame(doutorado1[doutorado1$grupo== "T180+MI",])

dados_SHAM_10 =filter(dados_SHAM_10, lote < 10)
dados_MI_10 =filter(dados_MI_10, lote < 10)
dados_T60_10 =filter(dados_T60_10, lote < 10)
dados_T90_10 =filter(dados_T90_10, lote < 10)
dados_T180_10 =filter(dados_T180_10, lote < 10)

View(doutorado2)
# ECO

# massa_ECO3
df= split(doutorado2$massa_ECO3, doutorado2$grupo)
lapply(df, KS)
byf.shapiro(massa_ECO3~grupo, data= doutorado2)
leveneTest(massa_ECO3~grupo, data= doutorado2, center= mean)
fig_massa_ECO3= ggbetweenstats(doutorado2, x= grupo, y= massa_ECO3,  centrality.point.args = list(shape= 15, size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "Massa corporal (g)")
fig_massa_ECO3

# FC
df= split(doutorado2$FC, doutorado2$grupo)
lapply(df, KS)
byf.shapiro(FC~grupo, data= doutorado2)
leveneTest(FC~grupo, data= doutorado2, center= mean)
fig_FC= ggbetweenstats(doutorado2, x= grupo, y= FC,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "HR eco (bpm)")
fig_FC

# Ao_M
df= split(doutorado2$Ao_M, doutorado2$grupo)
lapply(df, KS)
byf.shapiro(Ao_M~grupo, data= doutorado2)
leveneTest(Ao_M~grupo, data= doutorado2, center= mean)
fig_Ao_M= ggbetweenstats(doutorado2, x= grupo, y= Ao_M,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "AoD-M (cm)")
fig_Ao_M

# AE_M
df= split(doutorado2$AE_M, doutorado2$grupo)
lapply(df, KS)
byf.shapiro(AE_M~grupo, data= doutorado2)
leveneTest(AE_M~grupo, data= doutorado2, center= mean)
fig_AE_M= ggbetweenstats(doutorado2, x= grupo, y= AE_M,  centrality.point.args = list(size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "LAESD-M (cm)")
fig_AE_M

# FP
df= split(doutorado2$FP, doutorado2$grupo)
lapply(df, KS)
byf.shapiro(FP~grupo, data= doutorado2)
leveneTest(FP~grupo, data= doutorado2, center= mean)
fig_FP= ggbetweenstats(doutorado2, x= grupo, y= FP,  centrality.point.args = list(size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", var.equal= TRUE, ylab = "Pumonary artery peak flow velocity (m/s)")
fig_FP

# GP
df= split(doutorado2$GP, doutorado2$grupo)
lapply(df, KS)
byf.shapiro(GP~grupo, data= doutorado2)
leveneTest(GP~grupo, data= doutorado2, center= mean)
fig_GP= ggbetweenstats(doutorado2, x= grupo, y= GP,  centrality.point.args = list(size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", var.equal= TRUE, ylab = "Pulmonary artery gradient (mmHg)")
fig_GP

# E
df= split(doutorado2$E, doutorado2$grupo)
lapply(df, KS)
byf.shapiro(E~grupo, data= doutorado2)
leveneTest(E~grupo, data= doutorado2, center= mean)
fig_E= ggbetweenstats(doutorado2, x= grupo, y= E,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "E-wave (m/s)")
fig_E

# A
df= split(doutorado2$A, doutorado2$grupo)
lapply(df, KS)
byf.shapiro(A~grupo, data= doutorado2)
leveneTest(A~grupo, data= doutorado2, center= mean)
fig_A= ggbetweenstats(doutorado2, x= grupo, y= A,  centrality.point.args = list(size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= FALSE, bf.message= FALSE, xlab = "Groups", ylab = "A-wave (m/s)")
fig_A

# E_A
df= split(doutorado2$E_A, doutorado2$grupo)
lapply(df, KS)
byf.shapiro(E_A~grupo, data= doutorado2)
leveneTest(E_A~grupo, data= doutorado2, center= mean)
fig_E_A= ggbetweenstats(doutorado2, x= grupo, y= E_A,  centrality.point.args = list(size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= FALSE, bf.message= FALSE, xlab = "Groups", ylab = "E/A")
fig_E_A

# TDE
df= split(doutorado2$TDE, doutorado2$grupo)
lapply(df, KS)
leveneTest(TDE~grupo, data= doutorado2, center= mean)
fig_TDE= ggbetweenstats(doutorado2, x= grupo, y= TDE,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "DT (ms)")
fig_TDE

# TRIV
df= split(doutorado2$TRIV, doutorado2$grupo)
lapply(df, KS)
byf.shapiro(TRIV~grupo, data= doutorado2)
leveneTest(TRIV~grupo, data= doutorado2, center= mean)
fig_TRIV= ggbetweenstats(doutorado2, x= grupo, y= TRIV,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "IVRT (ms)")
fig_TRIV

# FA
df= split(doutorado2$FA, doutorado2$grupo)
lapply(df, KS)
byf.shapiro(FA~grupo, data= doutorado2)
leveneTest(FA~grupo, data= doutorado2, center= mean)
fig_FA= ggbetweenstats(doutorado2, x= grupo, y= FA,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "Aortic peak flow velocity (m/s)")
fig_FA

# GA
df= split(doutorado2$GA, doutorado2$grupo)
lapply(df, KS)
byf.shapiro(GA~grupo, data= doutorado2)
leveneTest(GA~grupo, data= doutorado2, center= mean)
fig_GA= ggbetweenstats(doutorado2, x= grupo, y= GA,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "Aortic gradient (mmHg)")
fig_GA

# Ao_B
df= split(doutorado2$Ao_B, doutorado2$grupo)
lapply(df, KS)
byf.shapiro(Ao_B~grupo, data= doutorado2)
leveneTest(Ao_B~grupo, data= doutorado2, center= mean)
fig_Ao_B= ggbetweenstats(doutorado2, x= grupo, y= Ao_B,  centrality.point.args = list(size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, xlab = "Groups", ylab = "AoD-B (cm)")
fig_Ao_B

# AE_B
df= split(doutorado2$AE_B, doutorado2$grupo)
lapply(df, KS)
byf.shapiro(AE_B~grupo, data= doutorado2)
leveneTest(AE_B~grupo, data= doutorado2, center= mean)
fig_AE_B= ggbetweenstats(doutorado2, x= grupo, y= AE_B,  centrality.point.args = list(shape= 15, size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "LAESD-B (cm)")
fig_AE_B

# area_AE
df= split(doutorado2$area_AE, doutorado2$grupo)
lapply(df, KS)
byf.shapiro(area_AE~grupo, data= doutorado2)
leveneTest(area_AE~grupo, data= doutorado2, center= mean)
fig_area_AE= ggbetweenstats(doutorado2, x= grupo, y= area_AE,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "LAESA (cm?)") + theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
    legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
legend.text = element_text(size = 12)     # Tamanho da fonte do texto da legenda
)
plot_grid(fig_area_AE, labels= "A", label_size = 24)

# ELD
df= split(doutorado2$ELD, doutorado2$grupo)
lapply(df, KS)
byf.shapiro(ELD~grupo, data= doutorado2)
leveneTest(ELD~grupo, data= doutorado2, center= mean)
fig_ELD= ggbetweenstats(doutorado2, x= grupo, y= ELD,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "LVFWT (cm)")
fig_ELD

# ESD
df= split(doutorado2$ESD, doutorado2$grupo)
lapply(df, KS)
byf.shapiro(ESD~grupo, data= doutorado2)
leveneTest(ESD~grupo, data= doutorado2, center= mean)
fig_ESD= ggbetweenstats(doutorado2, x= grupo, y= ESD,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "ISWT (cm)")
fig_ESD

# DVED
df= split(doutorado2$DVED, doutorado2$grupo)
lapply(df, KS)
byf.shapiro(DVED~grupo, data= doutorado2)
leveneTest(DVED~grupo, data= doutorado2, center= mean)
fig_DVED= ggbetweenstats(doutorado2, x= grupo, y= DVED,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "LVEDD (cm)")
fig_DVED

# DVES
df= split(doutorado2$DVES, doutorado2$grupo)
lapply(df, KS)
byf.shapiro(DVES~grupo, data= doutorado2)
leveneTest(DVES~grupo, data= doutorado2, center= mean)
fig_DVES= ggbetweenstats(doutorado2, x= grupo, y= DVES,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "LVESD (cm)")
fig_DVES

# Fenc
df= split(doutorado2$Fenc, doutorado2$grupo)
lapply(df, KS)
byf.shapiro(Fenc~grupo, data= doutorado2)
leveneTest(Fenc~grupo, data= doutorado2, center= mean)
fig_Fenc= ggbetweenstats(doutorado2, x= grupo, y= Fenc,  centrality.point.args = list(shape= 15, size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", bf.message= FALSE, xlab = "Groups", ylab = "SF (%)")
fig_Fenc

# FEJ
df= split(doutorado2$FEJ, doutorado2$grupo)
lapply(df, KS)
byf.shapiro(FEJ~grupo, data= doutorado2)
leveneTest(FEJ~grupo, data= doutorado2, center= mean)
fig_FEJ= ggbetweenstats(doutorado2, x= grupo, y= FEJ,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "EF (%)")
fig_FEJ

# IM
df= split(doutorado2$IM, doutorado2$grupo)
lapply(df, KS)
byf.shapiro(IM~grupo, data= doutorado2)
leveneTest(IM~grupo, data= doutorado2, center= mean)
KS(dados_MI_10$IM)
KS(dados_T60_10$IM)
KS(dados_T90_10$IM)
KS(dados_T180_10$IM)

fig_IM= ggbetweenstats(doutorado2, x= grupo, y= IM,  centrality.point.args = list(shape= 15, size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", violin.args = list(width = 0, linewidth = 0), pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = " MI size (%)") + theme(
axis.text.y = element_text(size = 10), # Tamanho da fonte dos rótulos do eixo y
axis.text.x = element_text(size = 9),     # Tamanho da fonte dos rótulos do eixo x
legend.title = element_text(size = 12),    # Tamanho da fonte do título da legenda
legend.text = element_text(size = 6)     # Tamanho da fonte do texto da legenda
)
fig_IM

# area_VE_diast
df= split(doutorado2$area_VE_diast, doutorado2$grupo)
lapply(df, KS)
byf.shapiro(area_VE_diast~grupo, data= doutorado2)
leveneTest(area_VE_diast~grupo, data= doutorado2, center= mean)
fig_area_VE_diast= ggbetweenstats(doutorado2, x= grupo, y= area_VE_diast,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "LVEDA (cm?)") + theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12)     # Tamanho da fonte do texto da legenda
)
plot_grid(fig_area_VE_diast, labels= "B", label_size = 24)

fig_area_VE_diast

# area_VE_sist
df= split(doutorado2$area_VE_sist, doutorado2$grupo)
lapply(df, KS)
byf.shapiro(area_VE_sist~grupo, data= doutorado2)
leveneTest(area_VE_sist~grupo, data= doutorado2, center= mean)
fig_area_VE_sist= ggbetweenstats(doutorado2, x= grupo, y= area_VE_sist,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "LVESA (cm?)") + theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12)     # Tamanho da fonte do texto da legenda
)
plot_grid(fig_area_VE_sist, labels= "C", label_size = 24)

fig_area_VE_sist


# FEAT
df= split(doutorado2$FEAT, doutorado2$grupo)
lapply(df, KS)
byf.shapiro(FEAT~grupo, data= doutorado2)
leveneTest(FEAT~grupo, data= doutorado2, center= mean)
fig_FEAT= ggbetweenstats(doutorado2, x= grupo, y= FEAT,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "FAC (%)") + theme(
text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 11),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12)     # Tamanho da fonte do texto da legenda
)
plot_grid(fig_FEAT, labels= "B", label_size = 24)

fig_FEAT


# CDM
df= split(doutorado2$CDM, doutorado2$grupo)
lapply(df, KS)
byf.shapiro(CDM~grupo, data= doutorado2)
leveneTest(CDM~grupo, data= doutorado2, center= mean)
fig_CDM= ggbetweenstats(doutorado2, x= grupo, y= CDM,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "LVEDC-papillary (cm)")
fig_CDM

# CSM
df= split(doutorado2$CSM, doutorado2$grupo)
lapply(df, KS)
byf.shapiro(CSM~grupo, data= doutorado2)
leveneTest(CSM~grupo, data= doutorado2, center= mean)
fig_CSM= ggbetweenstats(doutorado2, x= grupo, y= CSM,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "LVESC-papillary (cm)")
fig_CSM

# AM
df= split(doutorado2$AM, doutorado2$grupo)
lapply(df, KS)
byf.shapiro(AM~grupo, data= doutorado2)
leveneTest(AM~grupo, data= doutorado2, center= mean)
KS(dados_MI_10$AM)
KS(dados_T60_10$AM)
KS(dados_T90_10$AM)
KS(dados_T180_10$AM)

fig_AM= ggbetweenstats(doutorado2, x= grupo, y= AM,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, xlab = "Groups", ylab = "MIEDL-pipillary (cm)")
fig_AM

# CDA
df= split(doutorado2$CDA, doutorado2$grupo)
lapply(df, KS)
byf.shapiro(CDA~grupo, data= doutorado2)
leveneTest(CDA~grupo, data= doutorado2, center= mean)
fig_CDA= ggbetweenstats(doutorado2, x= grupo, y= CDA,  centrality.point.args = list(shape= 15, size= 5 , color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "LVEDC-apical (cm)")
fig_CDA

# CSA
df= split(doutorado2$CSA, doutorado2$grupo)
lapply(df, KS)
byf.shapiro(CSA~grupo, data= doutorado2)
leveneTest(CSA~grupo, data= doutorado2, center= mean)
fig_CSA= ggbetweenstats(doutorado2, x= grupo, y= CSA,  centrality.point.args = list(shape=15, size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "robust", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", xlab = "Groups", ylab = "LVESC-apical (cm)")
fig_CSA

# AA
df= split(doutorado2$AA, doutorado2$grupo)
lapply(df, KS)
byf.shapiro(AA~grupo, data= doutorado2)
leveneTest(AA~grupo, data= doutorado2, center= mean)
KS(dados_MI_10$AA)
KS(dados_T60_10$AA)
KS(dados_T90_10$AA)
KS(dados_T180_10$AA)

fig_AA= ggbetweenstats(doutorado2, x= grupo, y= AA,  centrality.point.args = list(size= 5, color= "darkred"), centrality.label.args = list(alpha= 0.0), type= "parametric", plot.type = "box", pairwise.comparisons = TRUE, pairwise.display = "significant", p.adjust.method = "holm", var.equal= TRUE, bf.message= FALSE, xlab = "Groups", ylab = "MIEDL-apical (cm)")
fig_AA




# Criar uma planilha sem o grupo SHAM
doutorado3= doutorado[-1,]
doutorado3= filter(doutorado3, grupo != "SHAM")
doutorado3= data.frame(id= doutorado3$id, grupo= doutorado3$grupo, mortalidade= doutorado3$mortalidade)
doutorado3$mortalidade= as.numeric(doutorado3$mortalidade)
doutorado3 = doutorado3[complete.cases(doutorado3), ]
doutorado3$mortalidade= replace(doutorado3$mortalidade, doutorado3$mortalidade== 0, "No")
doutorado3$mortalidade= replace(doutorado3$mortalidade, doutorado3$mortalidade== 1, "Yes")
doutorado3$grupo = factor(doutorado3$grupo, c("NT+MI", "T60+MI", "T90+MI", "T180+MI"))

# Criar uma planilha sem o grupo SHAM e ate o lote 10
doutorado4= doutorado[-1,]
doutorado4$lote= as.numeric(doutorado4$lote)
doutorado4= filter(doutorado4, lote < 11)
doutorado4= filter(doutorado4, grupo != "SHAM")
doutorado4= data.frame(id= doutorado4$id, grupo= doutorado4$grupo, mortalidade= doutorado4$mortalidade)
doutorado4$mortalidade= as.numeric(doutorado4$mortalidade)
doutorado4 = doutorado4[complete.cases(doutorado4), ]
doutorado4$mortalidade= replace(doutorado4$mortalidade, doutorado4$mortalidade== 0, "No")
doutorado4$mortalidade= replace(doutorado4$mortalidade, doutorado4$mortalidade== 1, "Yes")
doutorado4$grupo = factor(doutorado4$grupo, c("NT+MI", "T60+MI", "T90+MI", "T180+MI"))


# Avaliar a mortalidade todos os lotes, criar figura mortalidade e extrair dados estatísticos


fig_mortalidade= ggbarstats(doutorado3, x= mortalidade, y= grupo, type= "parametric", xlab = "Groups", ylab = "Percentage", legend.title= "Mortality", palette = "Set1", bf.message= FALSE) +theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12),    # Tamanho da fonte do texto da legenda
  plot.margin = unit(c(0, 4, 0, 4), "cm")
)

plot_grid(fig_mortalidade, labels = "A1", label_size = 24)


# Avaliar a mortalidade todos os lotes, inverter eixos, criar figura mortalidade e extrair dados estatísticos

fig_mortalidade_inv= ggbarstats(doutorado3, x= grupo, y= mortalidade, type= "parametric", xlab = "Mortality", legend.title= "Groups", ylab = "Percentage", palette = "Paired", bf.message= FALSE)+theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12),    # Tamanho da fonte do texto da legenda
  plot.margin = unit(c(0, 6, 0, 6), "cm")
)

extract_stats(fig_mortalidade_inv)

plot_grid(fig_mortalidade_inv, labels = "A2", label_size = 24)



# Avaliar a mortalidade até o lote 10 (possui todos os níveis de grupo), criar figura mortalidade e extrair dados estatísticos
fig_mortalidade2= ggbarstats(doutorado4, x= mortalidade, y= grupo, type= "parametric", xlab = "Groups", ylab = "Percentage", legend.title= "Mortality", palette = "Set1", bf.message= FALSE)

extract_stats(fig_mortalidade2)

plot_grid(fig_mortalidade2, labels = "B", label_size = 24)



# Avaliar a mortalidade até o lote 10 (possui todos os níveis de grupo), inverter eixos, criar figura mortalidade e extrair dados estatísticos

fig_mortalidade2_inv= ggbarstats(doutorado4, x= grupo, y= mortalidade, type= "parametric", xlab = "Mortality", legend.title= "Groups", ylab = "Percentage", palette = "Paired", bf.message= FALSE)

extract_stats(fig_mortalidade2_inv)

plot_grid(fig_mortalidade2_inv, labels = "B", label_size = 24)



# Mortalidade apenas os grups NT+MI e T60+MI

NT_MI= c(rep("No", 29), rep("Yes", 4))
T60_MI= c(rep("No", 25), rep("Yes", 11))
mortalidade= c(NT_MI, T60_MI)
grupo= c(rep("NT+MI", 33), rep("T60+MI", 36))

df= data.frame(grupo, mortalidade)

ggbarstats(df,mortalidade, grupo, type= "parametric", xlab = "Groups", ylab = "Percentage", legend.title= "Mortality", palette = "Set1", bf.message= FALSE) +theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12),      # Tamanho da fonte do texto da legenda
  plot.margin = unit(c(0, 4, 0, 4), "cm")
)


fot= ggbarstats(df, grupo, mortalidade, type= "parametric", xlab = "Mortality", legend.title= "Groups", ylab = "Percentage", palette = "Set1", bf.message= FALSE)+theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12),    # Tamanho da fonte do texto da legenda
  plot.margin = unit(c(0, 6, 0, 6), "cm")
  )

fot= fot + scale_fill_manual(values = c("NT+MI" = "#33A02C", "T60+MI" = "#B2DF8A"))
plot_grid(fot, labels = "B", label_size = 24)

brewer.pal(n = 4, name = "Paired")


# Perda de animais


Grupos= c("SHAM", rep("T60+MI", 2), rep("T90+MI", 6), rep("T180+MI", 3))
Motivo= c("Death by anesthesia", rep("Drowning death", 4), rep("Respiratory tract injury (pool drain)", 2), "Death by anesthesia", rep("Unable to swim", 2), "Illness", "Drowning death")
df5= data.frame(Grupos, Motivo)
df5$Grupos = factor(df5$Grupos, c("SHAM", "T60+MI", "T90+MI", "T180+MI"))

fig_motivo= ggbarstats(df5, x= Motivo, y= Grupos, type= "parametric", label= "counts", xlab = "Groups", legend.title= "Causes", ylab = "Percentage", palette = "Paired", bf.message= FALSE)+theme(
  text = element_text(size = 12),           # Tamanho da fonte geral
  axis.title.x = element_text(size = 14),    # Tamanho da fonte do título do eixo x
  axis.title.y = element_text(size = 14),    # Tamanho da fonte do título do eixo y
  axis.text.x = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo x
  axis.text.y = element_text(size = 12),     # Tamanho da fonte dos rótulos do eixo y
  legend.title = element_text(size = 14),    # Tamanho da fonte do título da legenda
  legend.text = element_text(size = 12),    # Tamanho da fonte do texto da legenda
  plot.margin = unit(c(0, 0, 0, 0), "cm")
)

plot_grid(fig_motivo)

