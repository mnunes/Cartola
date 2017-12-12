library(tidyverse)
theme_set(theme_bw())
library(broom)
library(GGally)
library(caret)

# leitura dos dados

scouts_2014 <- as_data_frame(read.csv(file="dados/2014_scouts.csv"))

dim(scouts_2014)

# leitura dos atletas e clubes

atletas_2014 <- read.csv(file="dados/2014_atletas.csv")
clubes_2014  <- read.csv(file="dados/2014_clubes.csv")

# colocar informacoes sobre atletas e clubes nos scouts

scouts_2014 <- left_join(scouts_2014, atletas_2014, by=c("atleta_id"="id"))
scouts_2014 <- left_join(scouts_2014, clubes_2014, by=c("clube_id.x"="id"))

# limpeza dos dados

names(scouts_2014)
scouts_2014 <- scouts_2014 %>%
  select(-clube_id.y, -posicao_id.y) %>%
  filter(rodada > 0) %>%
  na.omit()

summary(scouts_2014)

# jogadores com maior média

scouts_2014 %>%
  group_by(apelido, nome, posicao_id.x) %>%
  summarise(Media=mean(nota, na.rm=TRUE)) %>%
  arrange(desc(Media))

# filtro: jogadores com pelo menos k jogos

scouts_2014 %>%
  group_by(apelido, nome, posicao_id.x) %>%
  filter(n() >= 27) %>%
  summarise(Media=mean(nota, na.rm=TRUE)) %>%
  arrange(desc(Media))

# melhor time da competicao
# (por enquanto considera apenas a formacao 4-3-3)

source("melhor.jogador.posicao.R")
source("melhor.time.R")

melhor.time(scouts_2014)

# grafico do desempenho dos jogadores do melhor time 
# durante o torneio

time.do.campeonato <- melhor.time(scouts_2014)

time.do.campeonato.rodadas <- scouts_2014 %>% 
  filter(apelido %in% time.do.campeonato$apelido)

ggplot(time.do.campeonato.rodadas, aes(x=rodada, y=preco_num, group=apelido)) +
  geom_line(aes(colour=apelido)) +
  geom_point(aes(colour=apelido)) +
  labs(x="Rodada", y="Preço (Cartoletas)", colour="Jogador")

# jogadores valorizados

scouts_2014_regressao <- scouts_2014 %>%
  group_by(apelido, nome, posicao_id.x) %>%
  filter(n() >= 19) %>%
  do(regressao = lm(preco_num ~ rodada, data=.))
  
tidy(scouts_2014_regressao, regressao) %>%
  filter(term=="rodada") %>%
  filter(p.value < 0.05/245) %>%
  arrange(desc(estimate))
  
tidy(scouts_2014_regressao, regressao) %>%
  filter(term=="rodada") %>%
  filter(p.value < 0.05/245) %>%
  arrange(estimate)


##############
# clustering #
##############

names(scouts_2014)

scouts_2014_clustering <- scouts_2014 %>%
  #select(FS, PE, A, FT, FD, FF, G, I, PP, RB, FC, GC, CA, CV, SG, DD, DP, GS)
  select(FS, PE, A, FT, FD, FF, G, I, PP, RB, FC, GC, CA, CV)

# pre processamento

medias <- sapply(scouts_2014_clustering, mean)
sds    <- sapply(scouts_2014_clustering, sd)

scouts_2014_clustering_preprocess <- t((t(scouts_2014_clustering) - medias)/sds)

mean(scouts_2014_clustering_preprocess[, 1])
var(scouts_2014_clustering_preprocess[, 1])

# 

# pca 

scouts_2014_clustering_pca <- prcomp(scouts_2014_clustering, center=TRUE, scale.=TRUE)

summary(scouts_2014_clustering_pca)

scouts_2014_clustering_pca <- as_data_frame(scouts_2014_clustering_pca$x)

ggplot(scouts_2014_clustering_pca, aes(x=PC1, y=PC2)) +
  geom_point()

scouts_2014_clustering_kmeans <- kmeans(scouts_2014_clustering_pca, 5)

ggplot(scouts_2014_clustering_pca, aes(x=PC1, y=PC2)) +
  geom_point(aes(colour=as.factor(scouts_2014_clustering_kmeans$cluster))) +
  labs(x="PC1", y="PC2", colour="Cluster")

