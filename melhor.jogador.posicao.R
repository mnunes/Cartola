melhor.jogador.posicao <- function(scouts, posicao, qtde=1){

  tabela <- scouts %>%
    group_by(apelido, nome, posicao_id.x) %>%
    filter(n() >= 19) %>%
    summarise(Media=mean(preco_num, na.rm=TRUE)) %>%
    arrange(desc(Media))
  
  jogador <- tabela %>% 
    filter(posicao_id.x == posicao) %>%
    ungroup () %>%
    slice(1:qtde)
  
  return(jogador)
}
