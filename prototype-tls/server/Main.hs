module Main where

import Network.TLS
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString
import Control.Concurrent
import Control.Exception.Safe
import Control.Monad

main :: IO ()
main = do
    soc <- socket AF_INET Stream defaultProtocol
    addr <- inet_addr "0.0.0.0"
    bind soc (SockAddrInet 8080 addr)
    listen soc 5
    acceptLoop soc `finally` close soc

acceptLoop :: Socket -> IO ()
acceptLoop soc = forever $ do
    (conn, addr) <- accept soc
    forkIO $ echoLoop conn

echoLoop :: Socket -> IO ()
echoLoop conn = do
    sequence_ $ repeat $ do
      str <- recv conn 64
      send conn str
    `catch` (\(SomeException e) -> return ())
    `finally` close conn
