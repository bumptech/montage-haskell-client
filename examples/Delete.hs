{-# LANGUAGE OverloadedStrings, ScopedTypeVariables  #-}
import Data.Conduit.Pool (Pool, createPool)
import Network.Riak (defaultClient, connect, disconnect,
                    Client(port), Connection)

import Control.Exception (assert)
import Control.Monad (mapM)
import System.ZMQ as ZMQ
import System.IO
import qualified Data.ByteString.Lazy as BW
import Data.Maybe

import Network.Riak.MontageClient
import Utils (putDecimal, generateUI)

threadCount :: Int
threadCount = 1

main :: IO ()
main = do
  ctx <- ZMQ.init 1

  -- Montage ZeroMQ
  montageZpool <- createPool (do
      s <- ZMQ.socket ctx Req
      ZMQ.connect s "tcp://localhost:7078"
      return s
      ) ZMQ.close 1 5 threadCount

  let (bucket, key) = ("u-info", 1) :: (BW.ByteString, Int)
  let d = generateUI (key,1)

  mr <- montagePut montageZpool d
  assert ((data' $ fromMaybe (error "failed put") mr) == data' d) $ return ()

  -- test delete
  montageDelete montageZpool bucket (putDecimal key)
  mr <- montageGet montageZpool bucket (putDecimal key)
  assert (isNothing mr) $ return ()

  hPutStrLn stdout "\nsuccessfully deleted\n"
