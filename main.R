tabuleiro = matrix(rep('_', 9), nrow = 3, ncol = 3, byrow = TRUE)


posicao_ocupada <- function(linha, coluna, tabuleiro) {
  if (tabuleiro[linha, coluna] == 'x' || tabuleiro[linha, coluna] == 'o') {
    return(T)
  }
  return(F)
}


desenha_tabuleiro <- function(linha, coluna, tabuleiro) {
  cat('Dentro da função\n')
  if(posicao_ocupada(linha, coluna, tabuleiro) == T) {
    msg <- paste0('Posição existente na ', linha, ' e ', coluna, ' com ', tabuleiro[linha, coluna], '\n')
    return(cat(msg))  
    
  }else {
    if (tabuleiro[linha, coluna] == '_') {
      tabuleiro[linha, coluna] = 'x'
    }
  }
  
  print(tabuleiro)
  return(tabuleiro)
}



while (T) {
  cat('Quer jogar [S/N]')
  escolha = scan(what = as.character(), nmax = 1)
  if (escolha == 'N'){
     rm(list = ls())
     break()
  }
  
  
  cat('Digite a linha ')
  linha = as.integer(scan(nmax=1))
  coluna = as.array(scan(nmax=1))
  
  tabuleiro = desenha_tabuleiro(linha = linha, coluna = coluna, tabuleiro = tabuleiro)
  

}



