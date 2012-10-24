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

import Network.Riak.MontageClient as MO
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
  let ds = map generateUI $ zip [x | x <- [1..3]] [1..3]

  -- test get many
  mr <- montagePutMany montageZpool ds
  assert (length (fromMaybe (error "failed put") mr) == length ds) $ return ()

  mr <- montageGetMany montageZpool $ zip [ bucket | _ <- [1..3]] $ map putDecimal [1..3]
  case mr of
      Just mo -> assert ((map comparable ds) == (map comparable mo)) $ return ()
        where comparable mo = (MO.bucket mo, MO.key mo, MO.data' mo)
      Nothing -> error "found nothing on got many"

  hPutStrLn stdout "\nsuccessfully matched put many with get many\n"
