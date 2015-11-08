module Magic where

import Hexagon

data Magic =
    Fire
  | Ice
  | Earth
  | Warp
  | Twin Magic Magic
  | Rotate Int Magic
  | Forward Int Magic
  | Delay Int Magic
  | BranchCollision Magic Magic

data Spell = Spell Magic Position Direction

data Perk = Perk

data Card = Card [Perk] Payload
data Payload = Complete Magic | Incomplete (Card -> Int -> Card)
