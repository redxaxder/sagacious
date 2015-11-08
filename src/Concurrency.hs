module Concurrency
  ( module C
  , flushTBChan)
  where

import Control.Concurrent.STM.TBChan as C
import Control.Concurrent.MVar as C
import Control.Concurrent as C
import Control.Concurrent.STM as C

flushTBChan :: Int -> C.TBChan a -> IO [a]
flushTBChan n c = go n
  where
  go n =
   if n <= 0 then return [] else do
     mx <- C.atomically $ tryReadTBChan c
     case mx of
       Nothing -> return []
       Just x -> (x:) <$> go (n-1)
