{-# LANGUAGE OverloadedStrings, ScopedTypeVariables  #-}
import Data.Conduit.Pool (Pool, createPool)
import Network.Riak (defaultClient, connect, disconnect,
                    Client(port), Connection)

import qualified Data.Attoparsec.ByteString.Char8 as AttoC
import qualified Data.ByteString.Char8 as S

import Control.Monad
import Control.Exception (assert)
import System.ZMQ as ZMQ
import System.IO
import Data.Word (Word8)
import Data.Maybe

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Lazy as BW

import Text.ProtocolBuffers.Basic (toUtf8)
import Text.ProtocolBuffers.WireMessage (messageGet, messagePut, Wire)
import Text.ProtocolBuffers.Reflections (ReflectDescriptor)

import Network.Riak.MontageClient as MO
import Utils

import Proto.User.UserInfo as UI
import Proto.User.UserEvent as UE
import Proto.User.UserName as UN

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

  let ui = generateUI (1,2)
  _ <- montagePut montageZpool ui
  let ue = generateUE (2,3)
  _ <- montagePut montageZpool ue
  let un = generateUN (2, "montage")
  _ <- montagePut montageZpool un

  (subKey, valuesFound) <- montageGetBy' montageZpool "u-info" (putDecimal 1) ["u-event", "u-name"]
  assert ((data' $ fromMaybe (error "failed get subKey") subKey) == data' ui) $ return ()

  case valuesFound of
      Just v -> do
          assert (length v == 2) $ return ()
          let [ueFound, unFound] = v
          assert ((data' ueFound == data' ue) && (data' unFound == data' un)) $ return ()
      Nothing -> error "reference get found nothing"

  hPutStrLn stdout "\nsuccessfully did reference get\n"
