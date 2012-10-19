{-# LANGUAGE OverloadedStrings, ScopedTypeVariables  #-}
import Data.Conduit.Pool (Pool, createPool)
import Network.Riak (defaultClient, connect, disconnect,
                    Client(port), Connection)

import Control.Exception (try, SomeException(..), fromException, throw)
import System.ZMQ as ZMQ
import System.IO
import Data.Word (Word8)

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Lazy as BW

import Text.ProtocolBuffers.WireMessage (messageGet, messagePut, Wire)
import Text.ProtocolBuffers.Reflections (ReflectDescriptor)
import Network.Riak.MontageClient
import Network.Riak.Montage.Proto.Montage.MontageObject as MO

import Proto.User.UserInfo

-- utils

putDecimal' :: Int -> [Word8]
putDecimal' 0 = []
putDecimal' i = ((fromIntegral f) + 48) : putDecimal' (fromIntegral r)
  where
    (r, f) = i `divMod` 10
{-# INLINE putDecimal' #-}

putDecimal :: Int -> B.ByteString
putDecimal i | i < 0     = BW.pack $ (45 :: Word8) : (reverse $ putDecimal' $ abs i)
             | otherwise = BW.pack $ reverse $ putDecimal' i
{-# INLINE putDecimal #-}

-- test client

generateData :: (Int, Int) -> MontageObject
generateData (key, uid) = MontageObject Nothing "u-info" key' data' Nothing
  where
    key' = putDecimal $ fromIntegral key
    data' = messagePut $ UserInfo { uid = fromIntegral uid }

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
  let data' = map generateData $ zip [key | x <- [0..2]] [1..3]

  -- test put
  mr <- montagePutMany montageZpool data'
  hPutStrLn stdout "\nput: "
  case mr of
      Just res -> mapM_ (\r -> hPutStrLn stdout $ show r) res
      Nothing -> error "Nothing put in riak"

  -- test get and resolution
  mr <- montageGet montageZpool bucket (putDecimal key)
  case mr of
      Just mo -> hPutStrLn stdout $ "\ngot: \n" ++ (show mo)
      Nothing -> error $ "no value at bucket: " ++ (show bucket) ++ ", key: " ++ (show key)

  -- test delete
  montageDelete montageZpool bucket (putDecimal key)
  mr <- montageGet montageZpool bucket (putDecimal key)
  case mr of
      Just mo -> error $ "\ngot: \n" ++ (show mo) ++ " when should have found Nothing"
      Nothing -> hPutStrLn stdout "\nsuccessfully deleted"

  -- test get many
  let data' = map generateData $ zip [x | x <- [1..3]] [1..3]
  _ <- montagePutMany montageZpool data'
  mr <- montageGetMany montageZpool $ zip [ bucket | _ <- [1..3]] $ map putDecimal [1..3]
  case mr of
      Just mo -> case (map comparable data') == (map comparable mo) of
          True -> hPutStrLn stdout "\nsuccessfully got many"
          False -> error $ "\nput many: " ++ (show $ map comparable data') ++ "\ngot many: " ++ (show $ map comparable mo)
        where comparable mo = (MO.bucket mo, MO.key mo, MO.data' mo)
      Nothing -> error "Found nothing on got many"

