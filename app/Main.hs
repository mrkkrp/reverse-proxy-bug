{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent.Async
import Control.Monad
import qualified Data.ByteString.Lazy as L
import qualified Data.Conduit.Network as Net
import Data.IORef
import qualified Network.HTTP.Types.Status as S
import qualified Network.HTTP.ReverseProxy as RP
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as W

data Target = Blue | Green

bluePort, greenPort :: Int
bluePort = 5555
greenPort = 5556

main :: IO ()
main = do
  void . async $ runSimpleServer bluePort "blue"
  void . async $ runSimpleServer greenPort "green"
  proxyTargetRef <- newIORef Blue
  Net.runTCPServer (Net.serverSettings 8089 "*") $
    RP.rawProxyTo $ \_ -> do
      targetState <- readIORef proxyTargetRef
      writeIORef proxyTargetRef $
        case targetState of
          Blue -> Green
          Green -> Blue
      return $
        Right
          ( RP.ProxyDest
              "localhost"
              ( case targetState of
                  Blue -> bluePort
                  Green -> greenPort
              )
          )

runSimpleServer ::
  W.Port ->
  L.ByteString ->
  IO ()
runSimpleServer port res = W.run port $ \_ respond ->
  respond (W.responseLBS S.ok200 [] res)
