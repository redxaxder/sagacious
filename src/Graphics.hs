module Graphics where

import qualified SDL
import SDL (($=))
import GameTypes
import qualified Data.Map.Strict as M
import Data.Map.Strict((!))
import Foreign.C.Types
import Linear
import Linear.Affine
import qualified Concurrency as C
import Control.Lens
import qualified Data.Vector.Storable as SV
import Hexagon

type TextureMap = M.Map String Texture
data Texture = Texture SDL.Texture (V2 CInt)

hexRadius :: Num a => a
hexRadius = 30

hexagon :: SDL.Renderer -> Int -> IO Texture
hexagon renderer radius = do
  let height = fromIntegral $ 2 * radius
      width = ceiling $ sqrt 3 * fromIntegral radius
      size = V2 (width+1) (height+1)
  t <- SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessTarget size
  SDL.rendererRenderTarget renderer $= Just t
  SDL.rendererDrawColor renderer $= V4 0 0 0 0
  SDL.clear renderer
  let vertices =  SV.fromList $ map P
        [ V2 (width `div` 2) 0
        , V2 width (height `div` 4)
        , V2 width (3 * height `div` 4)
        , V2 (width `div` 2) height
        , V2 0 (3 * height `div` 4)
        , V2 0 (height `div` 4)
        , V2 (width `div` 2) 0]
  SDL.rendererDrawColor renderer $= V4 maxBound 0 0 maxBound
  SDL.drawLines renderer vertices
  SDL.present renderer
  SDL.rendererRenderTarget renderer $= Nothing
  SDL.textureBlendMode t $= SDL.BlendAlphaBlend
  return (Texture t size)


render :: TextureMap -> SDL.Renderer -> GameState -> IO ()
render textureMap renderer gameState = do
  let (Texture t size) = textureMap ! "hex"
  let mousepos = cursor gameState
      mousex = view _x (cursor gameState)
      mousey = view _y (cursor gameState)
      drawHex coord = do
        let hexpos = fmap round $
              center hexRadius coord .-^ (0.5 *^ fmap fromIntegral size)
        SDL.copy renderer t Nothing (Just $ SDL.Rectangle hexpos size)
      hexcoord = rectToHex hexRadius (fromIntegral <$> mousepos)
  SDL.rendererDrawColor renderer $= V4 0 0 0 maxBound
  SDL.clear renderer
  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
  mapM_ drawHex $ neighbors hexcoord
  SDL.present renderer
