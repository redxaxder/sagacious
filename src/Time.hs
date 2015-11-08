module Time (foreverThrottled) where
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import Control.Monad.State.Strict
import Control.Exception.Base(bracket)
import Control.Lens (zoom, _1, _2, (.=), use)
import qualified SDL.Time as T

foreverThrottled :: Int -> IO () -> IO ()
foreverThrottled fps action = bracket
  (newThrottle fps)
  (\(_,tid) -> killThread tid)
  (\(throttle,_) -> forever $ do
      takeMVar throttle
      action)

newThrottle :: Int -> IO (MVar (), ThreadId)
newThrottle n = do
  throttle <- newMVar ()
  tid <- forkIO $ forever $ do
    threadDelay (1000000 `div` n)
    putMVar throttle ()
  return (throttle, tid)


makeLoopS :: (Integral a) => a -> b -> StateT b IO () -> IO ThreadId
makeLoopS fps start action = do
  t0 <- T.ticks
  forkIO . forever . flip evalStateT (t0,start) $ do
    t_new <- T.ticks
    t_old <- use _1
    let delta = t_new - t_old
    _1 .= t_new
    when (delta < interval)
      (liftIO $ threadDelay $ fromIntegral $ interval - delta)
    zoom _2 action
  where
    interval = fromIntegral $ 1000000 `div` fps
makeLoop :: (Integral a) => a -> IO () -> IO ThreadId
makeLoop fps action = do
  t0 <- T.ticks
  forkIO . flip evalStateT t0 . forever $ do
    t_new <- T.ticks --Ticks is in milliseconds
    t_old <- get
    put t_new
    let delta = t_new - t_old
    when (delta < interval)
      --threadDelay expects microseconds
      (liftIO . threadDelay . fromIntegral $ 1000 * (interval - delta))
    liftIO action
  where
    interval = fromIntegral $ 1000 `div` fps
