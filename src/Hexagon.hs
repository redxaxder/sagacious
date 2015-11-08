module Hexagon where

import Linear.Matrix
import Linear.Vector
import Linear.Affine
import Linear.V3
import Linear.V2

import Control.Lens (view,(^.))

type HexCubicV = V3 Int
type HexCubicP = Point V3 Int
--       ( -1, 1, 0)  ( 0, 1,-1)
--  ( -1, 0, 1) ( 0, 0, 0) ( 1, 0,-1)
--       ( 0,-1, 1)  ( 1,-1, 0)
right :: HexCubicV
right = V3 1 0 (-1)
left :: HexCubicV
left = V3 (-1) 0 1
downLeft :: HexCubicV
downLeft = V3 0 (-1) 1
upRight :: HexCubicV
upRight = V3 0 1 (-1)
downRight :: HexCubicV
downRight = V3 1 (-1) 0
upLeft :: HexCubicV
upLeft = V3 (-1) 1 0

direction :: Int -> HexCubicV
direction n = case index of
  0 -> right
  1 -> upRight
  2 -> upLeft
  3 -> left
  4 -> downLeft
  5 -> downRight
  where
    n' = n `rem` 6
    index = if n' >= 0 then n' else n' + 6

neighbors :: HexCubicP -> [HexCubicP]
neighbors h = map ((h .+^) . direction) [0..5]

distance :: HexCubicP -> HexCubicP -> Int
distance s t = maximum . fmap abs $ s .-. t


transformAxialToRect :: Double -> M22 Double
transformAxialToRect r =
  r *!!
  V2 (V2 (sqrt 3) (sqrt 3 / 2))
     (V2   0        (3/2)     )
transformRectToAxial :: Double -> M22 Double
transformRectToAxial r = inv22 $ transformAxialToRect r

center :: Double --the radius of the hex
       -> HexCubicP --The position of the hex in cubic coordinates
       -> Point V2 Double -- The center of the hex
center r hex = P $ transformAxialToRect r !* v
  where
    v = fmap fromIntegral (hex ^. _xy)


rectToHex :: Double -> --the radius of the hexes
             Point V2 Double -> --the position to translate
             HexCubicP
rectToHex r (P v) | dx >= dy && dx >= dz = P $ V3 (-ry - rz) ry rz
                  | dy >= dz             = P $ V3 rx (-rx - rz) rz
                  | otherwise            = P $ V3 rx ry (-rx - ry)
  where
--    v = p ^. _xy
    (V2 x y) = transformRectToAxial r !* v
    z =  -x - y
    (rx, ry, rz) = (round x, round y, round z)
    (dx, dy, dz) = (abs (x - fromIntegral rx)
                   ,abs (y - fromIntegral ry)
                   ,abs (z - fromIntegral rz))
