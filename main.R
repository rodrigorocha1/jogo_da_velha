tabuleiro <- matrix('_', nrow = 3, ncol = 3, byrow = F)



posicao_ocupada <- function(localizacao, tabuleiro) {
  if (tabuleiro[localizacao] == 'x' || tabuleiro[localizacao] == 'o') {
    return(T)
  }
  return(F)
}



brain_ia <- function(localizacao, tabuleiro) {
  if(localizacao %in% c(1, 3, 7, 9)) {
    pos <- c(2, 4, 5 , 6 , 8) 
    tabuleiro[sample(pos, size=1)] <- 'o'
  } else if (localizacao %in% c(2, 4, 6, 8)) {
    pos <- c(1, 7, 5, 3, 9) 
    tabuleiro[sample(pos, size=1)] <- 'o'
  } else {
    pos <- c(1, 2, 3, 4, 6, 7, 8, 9) 
    tabuleiro[sample(pos, size=1)] <- 'o'
  }
  if(posicao_ocupada(localizacao = localizacao, tabuleiro = tabuleiro)) {
    return(tabuleiro)
  }
  
}


desenha_tabuleiro <- function(localizacao, tabuleiro) {
    if (tabuleiro[localizacao] == '_') {
      tabuleiro[localizacao] <- 'x'
    }
  return(tabuleiro)
}


verificar_ganhador <- function(tabuleiro) {
  val = c('x', 'o')
  for(i in val){
    # diagonal principal
    if(tabuleiro[1] == i && tabuleiro[5] == i && tabuleiro[9] == i){
      return(TRUE)
    } else if (tabuleiro[3] == i && tabuleiro[5] == i && tabuleiro[7] == i) { # diagonal se
      return(TRUE)
    } else if (tabuleiro[1] == i && tabuleiro[2] == i && tabuleiro[3] == i) {
      return(TRUE)
    } else if (tabuleiro[4] == i && tabuleiro[5] == i && tabuleiro[6] == i) {
      return(TRUE)
    } else if (tabuleiro[7] == i && tabuleiro[8] == i && tabuleiro[9] == i) {
      return(TRUE)
    } else if (tabuleiro[1] == i && tabuleiro[4] == i && tabuleiro[7] == i) {
      return(TRUE)
    } else if (tabuleiro[2] == i && tabuleiro[5] == i && tabuleiro[8] == i) {
      return(TRUE)
    } else if (tabuleiro[3] == i && tabuleiro[6] == i && tabuleiro[9] == i) {
      return(TRUE)
    } else {
      return(FALSE)
    }
    
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
  
  if(posicao_ocupada(localizacao = linha, tabuleiro =  tabuleiro)) {
   cat('Posição existente na linha ', linha, ' com ', tabuleiro[linha], '\n')
  }else {
    tabuleiro <- desenha_tabuleiro(localizacao = linha, tabuleiro = tabuleiro)
    cat('Player 1 \n')
    print(tabuleiro)
  }

  cat('\n IA: \n')
  tabuleiro <- brain_ia(localizacao = linha, tabuleiro = tabuleiro)
  print(tabuleiro)

}



