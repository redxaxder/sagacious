module Magic where

import Hexagon

data Terminal = Fire | Ice | Earth | Lightning
data MagicF a =
  --Casts both spells
    Twin a a
  | Rotate Int a
  | Forward a
  --Recurses the spell descriptor into itself
  | Collider a a
  --Casts the spell one turn later
  | Delay a

data Magic = Basic Terminal | Meta (MagicF Magic)

data Spell = Spell Magic Position Direction

delay :: Magic -> Magic
delay = Meta . Delay
collider :: Magic -> Magic -> Magic
collider yes no = Meta $ Collider yes no
forward = Meta . Forward
rotate n = Meta . Rotate n
twin s t = Meta $ Twin s t

data Perk = Perk

data Proxy a = Proxy
data Card = Card [Perk] Payload
data Payload = Complete Magic | Incomplete (Card -> Int -> Card)
