module GameTypes (GameState(..), gameInit) where

import Linear
import Linear.Affine
import Foreign.C.Types

data GameState = GameState {cursor :: Point V2 CInt}
gameInit = GameState (P (V2 0 0))
