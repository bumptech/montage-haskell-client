{-# LANGUAGE OverloadedStrings, ScopedTypeVariables  #-}
module Utils where

import Data.Word (Word8)

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Lazy as BW

import Text.ProtocolBuffers.WireMessage (messageGet, messagePut, Wire)
import Text.ProtocolBuffers.Reflections (ReflectDescriptor)
import Network.Riak.MontageClient (MontageObject(..))

import Proto.User.UserInfo
import Proto.User.UserEvent
import Proto.User.UserName

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

generateUI :: (Int, Int) -> MontageObject
generateUI (key, uid) = MontageObject Nothing "u-info" key' data' Nothing
  where
    key' = putDecimal $ fromIntegral key
    data' = messagePut $ UserInfo { uid = fromIntegral uid }

generateUE :: (Int, Int) -> MontageObject
generateUE (key, eid) = MontageObject Nothing "u-event" key' data' Nothing
  where
    key' = putDecimal $ fromIntegral key
    data' = messagePut $ UserEvent { eid = fromIntegral eid }

generateUN :: (Int, BW.ByteString) -> MontageObject
generateUN (key, nameStr) = MontageObject Nothing "u-name" key' data' Nothing
  where
    key' = putDecimal $ fromIntegral key
    data' = messagePut $ UserName { name = nameStr }

