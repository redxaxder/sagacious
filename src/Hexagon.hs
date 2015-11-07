module Hexagon where

import Linear.Vector
import Linear.Affine
import Linear.V3
import Linear.V2
type HexCubic = V3 Int
type Position = Point V3 Int
type Direction = HexCubic
--       ( -1, 1, 0)  ( 0, 1,-1)
--  ( -1, 0, 1) ( 0, 0, 0) ( 1, 0,-1)
--       ( 0,-1, 1)  ( 1,-1, 0)
right :: HexCubic
right = V3 1 0 (-1)
left :: HexCubic
left = V3 (-1) 0 1
downLeft :: HexCubic
downLeft = V3 0 (-1) 1
upRight :: HexCubic
upRight = V3 0 1 (-1)
downRight :: HexCubic
downRight = V3 1 (-1) 0
upLeft :: HexCubic
upLeft = V3 (-1) 1 0


distance :: HexCubic -> HexCubic -> Int
distance s t = maximum . fmap abs $ s ^-^ t

center :: Double --the radius of the hex
       -> HexCubic --The position of the hex in cubic coordinates
       -> Point V2 Double -- The center of the hex
center r (V3 x y _) =
  P $ V2
  (w * (x' + y' / 2))
  (h * y' * (3/4) )
  where
    (x', y') = (fromIntegral x, fromIntegral y)
    h = 2 * r :: Double
    w = sqrt 3 * r :: Double


rectToHex :: Double -> --the radius of the hexes
             Point V2 Double -> --the position to translate
             HexCubic
rectToHex r (P (V2 x y)) | dx >= dy && dx >= dz = V3 (-ry - rz) ry rz
                     | dy >= dz             = V3 rx (-rx - rz) rz
                     | otherwise            = V3 rx ry (-rx - ry)
  where
    x' = (x * sqrt 3   - y ) / (3 * r)
    y' = y * 2 / (3 * r)
    z' =  -x' - y'
    (rx, ry, rz) = (round x', round y', round z')
    (dx, dy, dz) = (abs (x' - fromIntegral rx)
                   ,abs (y' - fromIntegral ry)
                   ,abs (z' - fromIntegral rz))
