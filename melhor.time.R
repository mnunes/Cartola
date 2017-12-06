melhor.time <- function(scouts){
  
  goleiro  <- melhor.jogador.posicao(scouts, 1, 1)
  
  lateral <- melhor.jogador.posicao(scouts, 2, 2)
  
  zagueiro <- melhor.jogador.posicao(scouts, 3, 2)
  
  meia <- melhor.jogador.posicao(scouts, 4, 3)
  
  atacante <- melhor.jogador.posicao(scouts, 5, 3)

  time <- bind_rows(goleiro, lateral[1, ], zagueiro, lateral[2, ], meia, atacante)
  
  return(time)
}


