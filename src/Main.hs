{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Foreign.C.Types
import Linear
import Linear.Affine
import Data.Foldable (for_)
import qualified SDL
import SDL (($=))

import qualified Data.Vector.Storable as SV
import qualified Control.Monad.State.Strict as S
import Control.Concurrent.STM.TBChan (TBChan, newTBChanIO
  , readTBChan, tryWriteTBChan)
import Control.Concurrent.STM (TChan, newBroadcastTChanIO
  , atomically, dupTChan, writeTChan, tryReadTChan)
import Control.Concurrent (forkIO, killThread)
import Control.Lens(view)

import Hexagon

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  window <-
    SDL.createWindow
      "Sagacious"
      SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
  SDL.showWindow window
  renderer <-
    SDL.createRenderer
      window
      (-1)
      SDL.RendererConfig
         { SDL.rendererType = SDL.AcceleratedRenderer
         , SDL.rendererTargetTexture = False
         }

  sdlChan <- newBroadcastTChanIO

  gameStateChan <- newTBChanIO 3
  gameSdlEvents <- atomically $ dupTChan sdlChan
  gameThread <- forkIO $ gameLoop gameInit gameStateChan gameSdlEvents

  rendererSdlEvents <- atomically $ dupTChan sdlChan
  renderThread <- forkIO $ renderLoop renderer gameStateChan rendererSdlEvents

  eventPollLoop sdlChan

  killThread gameThread
  killThread renderThread
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

data Texture = Texture SDL.Texture (V2 CInt)

data GameState = GameState {cursor :: Point V2 CInt}
gameInit = GameState (P (V2 0 0))

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

gameLoop :: GameState -> TBChan GameState -> TChan SDL.Event -> IO ()
gameLoop initialState gameStateChan sdlChan =
  flip S.evalStateT initialState $ forever $ do
    sdlEvents <- S.liftIO $ flushTChan 1000 sdlChan
    mousePos <- S.liftIO SDL.getMouseLocation
    S.modify (\x -> x{cursor=mousePos})
    s <- S.get
    S.liftIO . atomically $ tryWriteTBChan gameStateChan s


eventPollLoop :: TChan SDL.Event -> IO ()
eventPollLoop chan = go
  where
  go = do
    e <- SDL.pollEvent
    case e of
      Nothing -> return ()
      Just event -> atomically $ writeTChan chan event
    let quit = Just SDL.QuitEvent == fmap SDL.eventPayload e
    unless quit go

hexRadius :: Num a => a
hexRadius = 30
renderLoop :: SDL.Renderer -> TBChan GameState -> TChan SDL.Event -> IO ()
renderLoop renderer gameStateChan sdlChan = do
  hex <- hexagon renderer hexRadius
  go hex
  where
  go (Texture t size) = forever $ do
    events <- flushTChan 1000 sdlChan
    gameState <- atomically $ readTBChan gameStateChan

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

    SDL.drawLine renderer (P $ V2 0 mousey) (P $ V2 screenWidth mousey)
    SDL.drawLine renderer (P $ V2 mousex 0) (P $ V2 mousex screenHeight)

    mapM_ drawHex $
      fmap (hexcoord .+^) [zero, left, right, upRight, upLeft, downRight, downLeft]
    SDL.present renderer


flushTChan :: Int -> TChan a -> IO [a]
flushTChan n c = go n
  where
  go n =
   if n <= 0 then return [] else do
     mx <- atomically $ tryReadTChan c
     case mx of
       Nothing -> return []
       Just x -> (x:) <$> go (n-1)
