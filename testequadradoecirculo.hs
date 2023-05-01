import Graphics.Gloss.Interface.Pure.Game

data GameState = GameState {
    squarePosX :: Float,
    squarePosY :: Float,
    circlePosX :: Float,
    circlePosY :: Float
}

initialState :: GameState
initialState = GameState 0 0 0 0

updateState :: Event -> GameState -> GameState
updateState (EventKey (Char 'w') Down _ _) state = newState { squarePosY = min 275 (squarePosY newState) }
  where newState = state { squarePosY = squarePosY state + 75 }
updateState (EventKey (Char 's') Down _ _) state = newState { squarePosY = max (-275) (squarePosY newState) }
  where newState = state { squarePosY = squarePosY state - 75 }
updateState (EventKey (Char 'a') Down _ _) state = newState { squarePosX = max (-275) (squarePosX newState) }
  where newState = state { squarePosX = squarePosX state - 75 }
updateState (EventKey (Char 'd') Down _ _) state = newState { squarePosX = min 275 (squarePosX newState) }
  where newState = state { squarePosX = squarePosX state + 75 }
updateState (EventKey (Char 'y') Down _ _) state = newState { circlePosY = min 275 (circlePosY newState) }
  where newState = state { circlePosY = circlePosY state + 75 }
updateState (EventKey (Char 'h') Down _ _) state = newState { circlePosY = max (-275) (circlePosY newState) }
  where newState = state { circlePosY = circlePosY state - 75 }
updateState (EventKey (Char 'g') Down _ _) state = newState { circlePosX = max (-275) (circlePosX newState) }
  where newState = state { circlePosX = circlePosX state - 75 }
updateState (EventKey (Char 'j') Down _ _) state = newState { circlePosX = min 275 (circlePosX newState) }
  where newState = state { circlePosX = circlePosX state + 75 }
updateState (EventKey (SpecialKey KeyEsc) Down _ _) _ = error "Encerrando o programa"
updateState _ state = state

drawState :: GameState -> Picture
drawState state = pictures [
  translate (squarePosX state) (squarePosY state) $ color blue $ rectangleSolid 50 50,
  translate (circlePosX state) (circlePosY state) $ color yellow $ circleSolid 25,
  color red $ line [(-300,-300),(-300,300),(300,300),(300,-300),(-300,-300)]
  ]

main :: IO ()
main = play window black 30 initialState drawState updateState (\_ state -> state)

window :: Display
window = InWindow "Game" (1920, 1080) (0, 0)
