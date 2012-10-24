{-# LANGUAGE OverloadedStrings, ScopedTypeVariables  #-}
import Data.Conduit.Pool (Pool, createPool)
import Network.Riak (defaultClient, connect, disconnect,
                    Client(port), Connection)

import Control.Exception (assert)
import Control.Monad (mapM)
import System.ZMQ as ZMQ
import System.IO
import qualified Data.ByteString.Lazy as BW

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
  let ds = map generateUI $ zip [key | x <- [0..2]] [1..3]

  -- test put
  mr <- mapM (montagePut montageZpool) ds
  assert (length mr == length ds) $ return ()

  -- test get and resolution
  mr <- montageGet montageZpool bucket (putDecimal key)
  case mr of
      Just mo -> assert (data' mo == data' (head $ reverse ds)) $ return ()
      Nothing -> error "no value found"

  hPutStrLn stdout "\nsuccessfully did last write wins resolution\n"
