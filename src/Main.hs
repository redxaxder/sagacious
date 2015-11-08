{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.State.Strict
import Foreign.C.Types
import Linear
import Linear.Affine
import Data.Foldable (for_)
import qualified SDL
import qualified SDL.Time
import SDL (($=))
import qualified Data.Map.Strict as M
-- import Data.IORef

import qualified Control.Monad.State.Strict as S
-- import Control.Concurrent.STM.TBChan (TBChan, newTBChanIO
--   , readTBChan, writeTBChan, tryWriteTBChan, tryReadTBChan)
-- import Control.Concurrent.STM (atomically)
-- import Control.Concurrent.MVar
-- import Control.Concurrent
import Control.Lens(view)

import qualified Concurrency as C
import Hexagon
import Time
import GameTypes
import Graphics

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

  sdlChan <- C.newTBChanIO 50
  gameStateVar <- C.newMVar gameInit
  textureMapVar <- C.newMVar (M.fromList [])

  gameLoopThread <- C.forkIO $ gameLoop gameStateVar sdlChan
  renderLoopThread <- C.forkIO $ renderLoop renderer textureMapVar gameStateVar
  eventPollLoop sdlChan

  C.killThread gameLoopThread
  C.killThread renderLoopThread
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit


gameLoop :: C.MVar GameState -> C.TBChan SDL.Event -> IO ()
gameLoop gameStateVar sdlChan =
  foreverThrottled 60 $ do
  sdlEvents <- liftIO $ C.flushTBChan 1000 sdlChan
  mousePos <- SDL.getMouseLocation
  C.modifyMVar_ gameStateVar (\x -> return x{cursor=mousePos})

untilM :: Monad m => m Bool -> m ()
untilM action = go
  where
  go = do
    b <- action
    unless b go

eventPollLoop :: C.TBChan SDL.Event -> IO ()
eventPollLoop chan = untilM $ do
  e <- SDL.pollEvent
  case e of
    Nothing -> return ()
    --If the event consumer is way behind we block here and can't
    --detect quits. Maybe we should do this a different way.
    Just event -> C.atomically $ C.writeTBChan chan event
  let quit = Just SDL.QuitEvent == fmap SDL.eventPayload e
  return quit

renderLoop :: SDL.Renderer -> C.MVar TextureMap -> C.MVar GameState -> IO ()
renderLoop renderer textureMapVar gameStateVar = do
  hex <- hexagon renderer hexRadius
  C.modifyMVar_ textureMapVar (return . M.insert "hex" hex)
  foreverThrottled 60 $ do
    gameState <- C.readMVar gameStateVar
    textureMap <- C.readMVar textureMapVar
    render textureMap renderer gameState
