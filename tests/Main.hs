{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow ((***))
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime, getCurrentTime)
import Network.HTTP (simpleHTTP, getRequest)
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Request(..), responseLBS, Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.Warp.AutoQuit
import Prelude hiding (log)
import Test.Hspec (Spec, hspec, describe, it, shouldBe)


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "log" $ do
    it "records time correctly (sanity check)" $ do
      output <- withLog $ \log -> do
        log "begin"
        threadDelay' 1
        log "after delay"
        threadDelay' 1
        log "end"
      output `shouldBe` [
          (0, "begin")
        , (1, "after delay")
        , (2, "end")
        ]

  describe "server" $ do
    let port = 3000

    it "automatically quits after aqsTimeout" $ do
      output <- withLog $ \log -> do
        log "begin"
        log "before server start"
        _ <- forkIO $ runServer port AutoQuitSettings {
              aqsTimeout = 1 * scale
            , aqsOnExit = log "server exit"
            }
        log "after server start"
        threadDelay' 2 >> log "end"

      output `shouldBe` [
           (0, "begin")
         , (0, "before server start")
         , (0, "after server start")
         , (1, "server exit")
         , (2, "end")
         ]

    it "prolongs it's lifetime after a request" $ do
      output <- withLog $ \log -> do
        log "server start"
        _ <- forkIO $ runServer port AutoQuitSettings {
              aqsTimeout = 2 * scale
            , aqsOnExit = log "server exit"
            }
        threadDelay' 1 >> ping port "fast" >> log "ping"
        threadDelay' 1 >> ping port "fast" >> log "ping"
        threadDelay' 3 >> log "end"

      output `shouldBe` [
           (0, "server start")
         , (1, "ping")
         , (2, "ping")
         , (4, "server exit")
         , (5, "end")
         ]

    it "does not die when serves requests" $ do
      output <- withLog $ \log -> do
        log "server start"
        _ <- forkIO $ runServer port AutoQuitSettings {
              aqsTimeout = 2 * scale
            , aqsOnExit = log "server exit"
            }
        _ <- forkIO $ ping port "slow" >> log "ping"
        threadDelay' 7 >> log "end"

      output `shouldBe` [
           (0, "server start")
         , (4, "ping")
         , (6, "server exit")
         , (7, "end")
         ]

type LogEntry = (UTCTime, String)
type NormalizedLogEntry = (Int, String)
type Log = IORef [LogEntry]

scale :: NominalDiffTime
scale = 0.1

threadDelay' :: Int -> IO ()
threadDelay' n = threadDelay . (* n) . round . (* 1000000) $ scale

normalize :: [LogEntry] -> [NormalizedLogEntry]
normalize [] = []
normalize xs = map ((round . (/ scale) . (flip diffUTCTime t)) *** id) xs where
  t = fst . head $ xs

withLog :: ((String -> IO ()) -> IO ()) -> IO [NormalizedLogEntry]
withLog f = do
  ref <- newIORef []
  f (addEntry ref)
  (normalize . reverse) <$> readIORef ref

addEntry :: Log -> String -> IO ()
addEntry ref message = do
  t <- getCurrentTime
  atomicModifyIORef' ref $ \xs -> ((t, message):xs, ())

ping :: Int -> String -> IO ()
ping port path = void $ simpleHTTP . getRequest $ concat ["http://localhost:", show port, "/", path]

runServer :: Int -> AutoQuitSettings -> IO ()
runServer port set = withAutoQuit set $ \chan -> run port (withHeartBeat chan app) where
  app :: Application
  app req f = dispatch >>= f where
    dispatch = case (rawPathInfo req) of
      "/fast" -> return respond200
      "/slow" -> threadDelay' 4 >> return respond200
      _       -> return respond404

    headers = [(hContentType, "text/plain")]

    respond200 = responseLBS status404 headers "Not Found"
    respond404 = responseLBS status200 headers "OK"
