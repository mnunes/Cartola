#####################
# pacotes necessarios

library(tidyverse)
theme_set(theme_bw())
library(broom)
library(GGally)
library(caret)
library(biotools)



###################
# leitura dos dados

scouts <- as_data_frame(read.csv(file="dados/2014_scouts.csv"))

dim(scouts)

# leitura dos atletas e clubes

atletas <- read.csv(file="dados/2014_atletas.csv")
clubes  <- read.csv(file="dados/2014_clubes.csv")

# colocar informacoes sobre atletas e clubes nos scouts

scouts <- left_join(scouts, atletas, by=c("atleta_id"="id"))
scouts <- left_join(scouts, clubes, by=c("clube_id.x"="id"))

# limpeza dos dados

names(scouts)
scouts <- scouts %>%
  select(-clube_id.y, -posicao_id.y, -nome) %>%
  filter(rodada > 0) %>%
  na.omit()

summary(scouts)



####################
# analise descritiva

# gols por jogador

scouts %>%
  group_by(apelido, slug) %>%
  summarise(Gols=sum(G)) %>%
  arrange(desc(Gols)) %>%
  filter(Gols >= 10)

scouts %>%
  group_by(apelido, slug) %>%
  summarise(Gols=sum(G)) %>%
  arrange(desc(Gols)) %>%
  filter(Gols >= 10) %>%
  ungroup() %>%
  mutate(apelido=factor(apelido, levels=unique(apelido))) %>%
  arrange(apelido) %>%
  ggplot(., aes(x=apelido, y=Gols, fill=toupper(slug))) +
  geom_col() +
  labs(x="Jogador", y="Gols", fill="Clube") +
  scale_fill_brewer(palette = "Dark2") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# assistencias por jogador

scouts %>%
  group_by(apelido, slug) %>%
  summarise(Assistencias=sum(A)) %>%
  arrange(desc(Assistencias)) %>%
  filter(Assistencias >= 8)

scouts %>%
  group_by(apelido, slug) %>%
  summarise(Assistencias=sum(A)) %>%
  arrange(desc(Assistencias)) %>%
  filter(Assistencias >= 8) %>%
  ungroup() %>%
  mutate(apelido=factor(apelido, levels=unique(apelido))) %>%
  arrange(apelido) %>%
  ggplot(., aes(x=apelido, y=Assistencias, fill=toupper(slug))) +
  geom_col() +
  labs(x="Jogador", y="Assistencias", fill="Clube") +
  scale_fill_brewer(palette = "Dark2") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  

# jogadores com maior média

scouts %>%
  group_by(apelido, nome, posicao_id.x) %>%
  summarise(Media=mean(nota, na.rm=TRUE)) %>%
  arrange(desc(Media))

# filtro: jogadores com pelo menos k jogos

scouts %>%
  group_by(apelido, nome, posicao_id.x) %>%
  filter(n() >= 27) %>%
  summarise(Media=mean(nota, na.rm=TRUE)) %>%
  arrange(desc(Media))

# melhor time da competicao
# (por enquanto considera apenas a formacao 4-3-3)

source("melhor.jogador.posicao.R")
source("melhor.time.R")

melhor.time(scouts)

# grafico do desempenho dos jogadores do melhor time 
# durante o torneio

time.do.campeonato <- melhor.time(scouts)

time.do.campeonato.rodadas <- scouts %>% 
  filter(apelido %in% time.do.campeonato$apelido)

ggplot(time.do.campeonato.rodadas, aes(x=rodada, y=preco_num, group=apelido)) +
  geom_line(aes(colour=apelido)) +
  geom_point(aes(colour=apelido)) +
  labs(x="Rodada", y="Preço (Cartoletas)", colour="Jogador")

# jogadores valorizados

scouts_regressao <- scouts %>%
  group_by(apelido, nome, posicao_id.x) %>%
  filter(n() >= 19) %>%
  do(regressao = lm(preco_num ~ rodada, data=.))
  
tidy(scouts_regressao, regressao) %>%
  filter(term=="rodada") %>%
  filter(p.value < 0.05/245) %>%
  arrange(desc(estimate))
  
tidy(scouts_regressao, regressao) %>%
  filter(term=="rodada") %>%
  filter(p.value < 0.05/245) %>%
  arrange(estimate)


##############
# clustering #
##############

names(scouts)

scouts_clustering <- scouts %>%
  #select(FS, PE, A, FT, FD, FF, G, I, PP, RB, FC, GC, CA, CV, SG, DD, DP, GS)
  select(FS, PE, A, FT, FD, FF, G, I, PP, RB, FC, GC, CA, CV)

# pre processamento

medias <- sapply(scouts_clustering, mean)
sds    <- sapply(scouts_clustering, sd)

scouts_clustering_preprocess <- t((t(scouts_clustering) - medias)/sds)

mean(scouts_clustering_preprocess[, 1])
var(scouts_clustering_preprocess[, 1])

# 

# pca 

scouts_clustering_pca <- prcomp(scouts_clustering, center=TRUE, scale.=TRUE)

summary(scouts_clustering_pca)

scouts_clustering_pca <- as_data_frame(scouts_clustering_pca$x)

ggplot(scouts_clustering_pca, aes(x=PC1, y=PC2)) +
  geom_point()

scouts_clustering_kmeans <- kmeans(scouts_clustering_pca, 5)

ggplot(scouts_clustering_pca, aes(x=PC1, y=PC2)) +
  geom_point(aes(colour=as.factor(scouts_clustering_kmeans$cluster))) +
  labs(x="PC1", y="PC2", colour="Cluster")

# clustering hierarquico 

D2.(scouts_clustering)




