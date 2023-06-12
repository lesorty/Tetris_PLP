# Tetris_PLP
Feito por: Erick Farias, Julio Hsu, Lucas Emanuel, Paulo Ricardo, Steylor Nobrega

Implementação em Prolog do clássico jogo Tetris com todas as features originais!!
Um arquivo em python chamado helper.py também  foi implementado para deixar o jogo mais dinâmico, já que com ele não temos mais a necessidade de apertar enter a cada input.

# Pré-Requisitos
- SWI-Prolog
- python (opcional)
- modulos de python: keyboard e pygame (opcional)

# Execução

## Sem helper.py:
Para rodar o código, basta chamar 'swipl main.pl' no diretório Tetris_PLP/Prolog  e  chamar a  'main.'

## Com helper.py:
- Abra 2 terminais: no 1° deles basta chamar 'swipl main.pl' no diretório Tetris_PLP/Prolog
- No 2° terminal, chame python helper.py no diretório Tetris_PLP/Prolog e espere carregar por uns segundos
- Depois disso, basta apertar 'm' no 1° terminal

# Funcionalidades

Elas são:
- Movimentação das peças lateralmente
- Girar a peça
- Descida automatica da peça baseado em tempo
- Fazer a peça cair instântaneamente
- Sistema de pontuação
- Previsão de como a peça se encaixa no tabuleiro
- Comando para guardar peça
- Seleção de dificuldade
- Progressão de dificuldade
- Controlador de volume
- Highscore
- Persistência de configurações
- Música (Python)
- Atualização automática da tela (Python)

# Comandos
wasd <- Controles do menu
s <- Descer a peça mais rápido
w <- Girar a peça
a <- Mover para esquerda
d <- Mover para a direita
c <- Guardar a peça / Trocar para guardada
v <- Descer a peça instantaneamente
x <- Sair do jogo e finalizar o programa python
