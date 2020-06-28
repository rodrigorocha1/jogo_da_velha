tabuleiro = matrix('_', nrow = 3, ncol = 3)

posicao_ocupada <- function(linha, coluna, tabuleiro) {
  if (tabuleiro[linha, coluna] == 'x' || tabuleiro[linha, coluna] == 'o') {
    return(T)
  }
  return(F)
}


desenha_tabuleiro <- function(linha, coluna, tabuleiro) {
  if(posicao_ocupada(linha, coluna, tabuleiro)) {
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


verificar_ganhador <- function(tabuleiro) {
  for(i in val){
    # diagonal principal
    if(tabuleiro[1] == i && tabuleiro[5] == i && tabuleiro[9] == i){
      return(i)
    } else if (tabuleiro[3] == i && tabuleiro[5] == i && tabuleiro[7] == i) { # diagonal se
      return(i)
    } else if (tabuleiro[1] == i && tabuleiro[2] == i && tabuleiro[3] == i) {
      return(i)
    } else if (tabuleiro[4] == i && tabuleiro[5] == i && tabuleiro[6] == i) {
      return(i)
    } else if (tabuleiro[7] == i && tabuleiro[8] == i && tabuleiro[9] == i) {
      return(i)
    } else if (tabuleiro[1] == i && tabuleiro[4] == i && tabuleiro[7] == i) {
      return(i)
    } else if (tabuleiro[2] == i && tabuleiro[5] == i && tabuleiro[8] == i) {
      return(i)
    } else if (tabuleiro[3] == i && tabuleiro[6] == i && tabuleiro[9] == i) {
      return(i)
    }
    return(NULL)
  }
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
  cat('Digite a coluna ')
  coluna = as.integer(scan(nmax=1))
  
  tabuleiro = desenha_tabuleiro(linha = linha, coluna = coluna, tabuleiro = tabuleiro)
  

}



