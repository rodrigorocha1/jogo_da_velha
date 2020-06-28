
tabuleiro = matrix(rep('_', 9), nrow = 3, ncol = 3)
tabuleiro[1] = '1'
tabuleiro[2] = '2'
tabuleiro[3] = '3'
tabuleiro[4] = '4'
tabuleiro[5] = '5'
tabuleiro[6] = '6'
tabuleiro[7] = '7'
tabuleiro[8] = '8'
tabuleiro[9] = '9'


sample(tabuleiro, tabuleiro[2], size = 1)


sample(unique(tabuleiro), tabuleiro[2, 4, 5], size = 1))

tabuleiro[6]
val = c('x', 'o')
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
}

for(i in 1:length(tabuleiro)){
  cat('Pos [',i, '] -> Valor:', tabuleiro[5], '\n')
}



contador_posicao <- function(tabuleiro) {
  cont <- c()
  for(i in 1:length(tabuleiro)){
    if(tabuleiro[i] == 'x'){
      cont[i] = tabuleiro[i]
    }
  }
  return(length(cont))
}

(contador_posicao(tabuleiro = tabuleiro))