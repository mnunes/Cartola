library(tidyverse)
theme_set(theme_bw())

# leitura dos dados

scouts_2014 <- as_data_frame(read.csv(file="2014_scouts.csv"))

dim(scouts_2014)

# leitura dos atletas e clubes

atletas_2014 <- read.csv(file="2014_atletas.csv")
clubes_2014  <- read.csv(file="2014_clubes.csv")

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




