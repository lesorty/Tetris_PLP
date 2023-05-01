{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Main where

import DrawGrid
import Graphics.Gloss

-- Configurações básicas para inicializar a tela do jogo
main :: IO ()
main = play window black fps initialState drawScene updateState pure'
  where
    window = InWindow "Tetris De Iraponildo" (900, 900) (100, 100)
    fps = 60
    initialState = State (1, 1)
    pure' _ s = s