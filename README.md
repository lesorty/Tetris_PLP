# Tetris_PLP
Feito por: Erick Farias, Julio Hsu, Lucas Emanuel, Paulo Ricardo, Steylor Nobrega

Implementação em Haskell do clássico jogo Tetris com todas as features originais!!
Frontend implementado utilizado a biblioteca Gloss

Para Rodar o código basta usar o comando "cabal run" na raiz do diretório

Elas são:
- Movimentação das peças lateralmente
- Girar a peça
- Descida automatica da peça baseado em tempo
- Fazer a peça cair instântaneamente
- Sistema de pontuação
- Previsão de como a peça se encaixa no tabuleiro
- Comando para guardar peça
- Progressão de dificuldade

Comandos:
s <- descer a pesça mais rápido
w <- girar a peça
a <- mover para esquerda
d <- mover para a direita
c <- guardar a peça/ trocar para guardada
space <- descer a peça instantaneamente
