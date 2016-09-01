{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
  Module:      Network.Wai.Handler.Warp.AutoQuit
  Copyright:   (c) 2016 Al Zohali
  License:     BSD3
  Maintainer:  Al Zohali <zohl@fmap.me>
  Stability:   experimental


  = Description
  Automatically quit warp server when inactive.
-}


module Network.Wai.Handler.Warp.AutoQuit (
    AutoQuitSettings(..)
  , AutoQuitException(..)
  , withAutoQuit
  , withHeartBeat
  ) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow(..), catch, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (void, when)
import Data.Default (Default, def)
import Data.Time.Clock (NominalDiffTime)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Network.Wai (Application)
import System.Timeout (timeout)


-- | Options that determine quitting mechanism.
data AutoQuitSettings = AutoQuitSettings {
    aqsTimeout :: NominalDiffTime
    -- ^ Time interval within which the server should still alive.
  , aqsOnExit  :: IO ()
    -- ^ An action to perform right after the timeout.
  }

instance Default AutoQuitSettings where
  def = AutoQuitSettings {
      aqsTimeout = fromIntegral (5 * 60 :: Integer) -- 5 minutes
    , aqsOnExit  = return ()
    }


-- | The exception is thrown when something goes wrong with this package.
data AutoQuitException
     = HeartBeatEventsMismatched
     -- ^ Thrown when there are more disconnections than connections.

    deriving (Eq, Show, Typeable)

instance (Exception AutoQuitException)


data HeartBeat = Connect | Disconnect


-- | Wrapper that kills the server after the timeout. It is supposed
--   to be used in tandem with 'withHeartBeat'.
withAutoQuit :: AutoQuitSettings -> (Chan HeartBeat -> IO a) -> IO ()
withAutoQuit set f = catch (withAutoQuitM set f) $
  \(ex :: AutoQuitException) -> error $ show ex

-- | Wrapper that kills the server after the timeout. It is supposed
--   to be used in tandem with 'withHeartBeat'.
withAutoQuitM :: forall a m. (MonadIO m, MonadThrow m)
  => AutoQuitSettings
  -> (Chan HeartBeat -> IO a)
  -> m ()
withAutoQuitM set@(AutoQuitSettings {..}) f = do
  chan     <- liftIO $ newChan
  threadId <- liftIO $ forkIO . void $ f chan
  wait set chan
  liftIO $ killThread threadId

-- | Wrapper that sends signals when the application is active/inactive.
withHeartBeat :: Chan HeartBeat -> Application -> Application
withHeartBeat chan app request f = do
  writeChan chan Connect
  app request $ \r -> do
    response <- f r
    writeChan chan Disconnect
    return response

toMs :: NominalDiffTime -> Int
toMs = round . (* 1000000)

wait :: forall m. (MonadIO m, MonadThrow m) => AutoQuitSettings -> Chan HeartBeat -> m ()
wait (AutoQuitSettings {..}) chan = wait' 0 where

  wait' :: Int -> m ()
  wait' n = case n of
    0 -> (liftIO wait'')          >>= fromMaybe (liftIO $ aqsOnExit) . (process n <$>)
    _ -> (liftIO $ readChan chan) >>= process n

  wait'' :: IO (Maybe HeartBeat)
  wait'' = timeout (toMs aqsTimeout) (readChan chan)

  process :: Int -> HeartBeat -> m ()
  process n Connect = wait' (n+1)
  process n Disconnect = when (n == 0) (throwM HeartBeatEventsMismatched) >> wait' (n-1)

