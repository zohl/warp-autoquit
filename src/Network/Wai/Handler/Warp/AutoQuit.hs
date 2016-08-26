{-# LANGUAGE RecordWildCards #-}

module Network.Wai.Handler.Warp.AutoQuit (
    AutoQuitSettings(..)
  , AutoQuitException(..)
  , withAutoQuit
  , withHeartBeat
  ) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception (Exception, throw)
import Control.Monad (void, when)
import Data.Default (Default, def)
import Data.Maybe (fromMaybe)
import Data.Typeable
import Network.Wai (Application)
import System.Timeout


data AutoQuitSettings = AutoQuitSettings {
    aqsTimeout :: Int
  , aqsOnExit  :: IO ()
  }

instance Default AutoQuitSettings where
  def = AutoQuitSettings {
      aqsTimeout = 60 * 1000000 -- 1 minute
    , aqsOnExit  = return ()
    }

data AutoQuitException
     = HeartBeatEventsMismatched
     -- ^ Thrown when there are more disconnections than connections.

    deriving (Eq, Show, Typeable)

instance (Exception AutoQuitException)


data HeartBeat = Connect | Disconnect

withAutoQuit :: AutoQuitSettings -> (Chan HeartBeat -> IO a) -> IO ()
withAutoQuit set@(AutoQuitSettings {..}) f = do
  chan <- newChan
  threadId <- forkIO . void $ f chan
  wait set chan
  killThread threadId

withHeartBeat :: Chan HeartBeat -> Application -> Application
withHeartBeat chan app request f = do
  writeChan chan Connect
  app request $ \r -> do
    response <- f r
    writeChan chan Disconnect
    return response

wait :: AutoQuitSettings -> Chan HeartBeat -> IO ()
wait (AutoQuitSettings {..}) chan = wait' 0 where
  wait' :: Int -> IO ()
  wait' n = case n of
    0 -> timeout aqsTimeout (readChan chan) >>= fromMaybe aqsOnExit . (process n <$>)
    _ -> readChan chan                      >>= process n

  process n Connect = wait' (n+1)
  process n Disconnect = when (n == 0) (throw HeartBeatEventsMismatched) >> wait' (n-1)

