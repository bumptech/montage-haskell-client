{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Network.Riak.MontageClient.MontageClient
-- Maintainer:  Erin Dahlgren <edahlgren@bu.mp>
-- Stability:   experimental
-- Portability: portable
--
-- Functions for making requests to the Montage riak resolution proxy.

module Network.Riak.MontageClient.MontageClient
       (
       -- * MontageObjects
       -- $montageObjects
         Bucket
       , ReferenceBucket
       , Key
       -- * Get requests
       , montageGet
       , montageGetMany
       , montageGetBy
       , montageGetBy'
       -- * Delete requests
       , montageDelete
       -- * Put requests
       , montagePut
       , montagePutMany
       )
       where

import System.UUID.V4 (uuid)
import System.Timeout (timeout)
import System.ZMQ as ZMQ
import System.IO
import System.IO.Error (IOError(..), ioeGetErrorString)
import Control.Monad.Error (throwError, strMsg, Error, MonadError)
import Control.Exception (try, SomeException(..), fromException, throw)
import Control.Monad

import Data.Conduit.Pool (Pool, withResource)
import Data.Text.Format
import Data.Text.Format.Params
import qualified Data.Text.Lazy as T
import qualified Data.ListLike as LL
import qualified Data.Sequence as Seq

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as B

import Text.ProtocolBuffers.Basic (toUtf8, uToString, Utf8)
import Text.ProtocolBuffers.WireMessage (messageGet, messagePut, Wire)
import Text.ProtocolBuffers.Reflections (ReflectDescriptor)

import Network.Riak.MontageClient.Proto.MontageClient.MontageWireMessages
import Network.Riak.MontageClient.Proto.MontageClient.MontageObject as MO
import Network.Riak.MontageClient.Proto.MontageClient.MontageEnvelope as ME
import Network.Riak.MontageClient.Proto.MontageClient.MontageGetStatus
import Network.Riak.MontageClient.Proto.MontageClient.MontageGet
import Network.Riak.MontageClient.Proto.MontageClient.MontagePut
import Network.Riak.MontageClient.Proto.MontageClient.MontageGetMany
import Network.Riak.MontageClient.Proto.MontageClient.MontageGetReference
import Network.Riak.MontageClient.Proto.MontageClient.MontageGetResponse as MGR
import Network.Riak.MontageClient.Proto.MontageClient.MontagePutResponse as MPR
import Network.Riak.MontageClient.Proto.MontageClient.MontagePutManyResponse as MPMR
import Network.Riak.MontageClient.Proto.MontageClient.MontagePutMany
import Network.Riak.MontageClient.Proto.MontageClient.MontageDelete

import qualified Network.Riak.MontageClient.Proto.MontageClient.MontageError as MErr

type Bucket = L.ByteString

type ReferenceBucket = L.ByteString

type Key = L.ByteString

type MontagePool = Pool (ZMQ.Socket Req)

-- utils

sTl :: S.ByteString -> B.ByteString
sTl s = B.fromChunks [s]

formatThrow :: (Params ps, Error e, MonadError e m, Monad m) => Format -> ps -> m ()
formatThrow f p = throwError (strMsg $ T.unpack $ format f p)

assertM :: (Params ps, Error e, MonadError e m, Monad m) => Bool -> Format -> ps -> m ()
assertM bool f p = when (not bool) $ formatThrow f p

messageGetError :: (ReflectDescriptor a, Wire a) => B.ByteString -> B.ByteString -> a
messageGetError cls v =
    case messageGet v of
        Right (msg, x) | B.length x == 0 ->
            msg
        Right (_, x) | B.length x /= 0 ->
            error ("error decoding " ++ B.unpack cls ++ ": did not consume all of input")
        Left e ->
            error ("error decoding " ++ B.unpack cls ++ ": " ++ e)

--- client

sixtySecs = 60 * 1000 * 1000

montageRpc :: MontagePool -> L.ByteString -> IO MontageEnvelope
montageRpc pool req = do
    mr <- timeout sixtySecs rpc
    case mr of
        Just r -> return $ (messageGetError "MontageEnvelope" . sTl) r
        Nothing -> error "timeout on montageRpc!"
  where
    rpc =   withResource pool (\socket -> do
                ZMQ.send' socket req []
                ZMQ.receive socket []
                )

montageRequest :: MontagePool -> MontageWireMessages -> L.ByteString -> IO MontageEnvelope
montageRequest pool wireMsg request = do
    uid <- fmap (B.pack . show) uuid
    let req = messagePut $ MontageEnvelope wireMsg request (Just uid)
    res <- montageRpc pool req
    assertM (ME.msgid res == Just uid) "mismatched req/response!" ()
    return res

-- $montageObjects
--
-- Requests to and responses from the Montage riak resolution proxy are wrapped in 'MontageObject's.  A 'MontageObject' contains information about the target\/source ('Bucket' and 'Key'), the serialized data to store\/find, and a riak vclock (which is originally Nothing).
--
-- You must have resolutions defined in your running proxy (see montage\/examples\/basic_proxy, and montage-haskell-client\/examples) for each of the datatypes you are serializing and placing inside a 'MontageObject'.
--
-- For @montageGetBy@ to complete its intermediate lookup, you must have @referenceKey@ defined for each of your datatypes in your running proxy.

-- | Retrieve a 'MontageObject' from a 'Bucket' and 'Key', if data exists at that source.  Montage itself resolves any siblings.
montageGet :: MontagePool -> Bucket -> Key -> IO (Maybe MontageObject)
montageGet pool buck key = do
    res <- montageRequest pool MONTAGE_GET (messagePut $ MontageGet buck key Nothing)
    case ME.mtype res of
        MONTAGE_GET_RESPONSE -> return $ MGR.master $ messageGetError "montageGet: MontageGetResponse" $ ME.msg res
        MONTAGE_ERROR        -> formatThrow "Incorrect response to MontageGet: error={}, bucket={}, key={}" (Shown $ MErr.error msg, Shown buck, Shown key) >> return Nothing
          where msg = messageGetError "montageGet: MontageGetResponse" $ ME.msg res
        _                  -> formatThrow "Unknown response to MontageGet" () >> return Nothing

zipGets :: [MontageGetStatus] -> [MontageObject] -> [Maybe MontageObject]
zipGets ss objs = reverse $ snd $ foldl honest (objs, []) ss
  where
    honest (os, accum) status = case status of
          EXISTS -> honest' os accum
          MISSING -> (os, Nothing:accum)

    honest' [] _ = error "Status EXISTS when no more responses"
    honest' (o:os) accum = (os, (Just o:accum))

-- | Retrieve many 'MontageObject's from a list of 'Bucket's and 'Key's, if data exists at those sources.  Responses are ordered the same as the 'Bucket's and 'Key's.
montageGetMany :: MontagePool -> [(Bucket, Key)] -> IO ([Maybe MontageObject])
montageGetMany pool gets = do
    let gets' = Seq.fromList [ MontageGet buck key Nothing | (buck, key) <- gets ]
    res <- montageRequest pool MONTAGE_GET_MANY (messagePut $ MontageGetMany gets')
    case ME.mtype res of
        MONTAGE_GET_RESPONSE ->
            return $ zipGets (LL.toList . MGR.status $ mgr) (LL.toList . MGR.subs $ mgr)
          where mgr = messageGetError "montageGetMany: MontageGetResponse" $ ME.msg res
        MONTAGE_ERROR        -> formatThrow "Incorrect response to MontageGetMany: error={}" (Only $ Shown $ MErr.error msg) >> return []
          where msg = messageGetError "montageGetMany: MontageGetResponse" $ ME.msg res
        _                  -> formatThrow "Unknown response to MontageGetMany" () >> return []

-- | Retrieve zero or more 'MontageObject's through an intermediate lookup.
--
-- The first lookup will use the 'ReferenceBucket' and 'Key' to retreive a 'MontageObject. An implementation of @referenceKey@ (in the instances you define for the Montage proxy) extracts from this 'MontageObject' a sensible 'Key' that can be used for further lookups. @montageGetBy@ uses the intermediate 'Key' and each target 'Bucket' to retrieve 'MontageObject's. If a sensible intermediate 'Key' cannot be made, then @montageGetBy@ reacts the same as @montageGetMany@ when nothing can be found.
montageGetBy :: MontagePool -> ReferenceBucket -> Key -> [Bucket] -> IO ([Maybe MontageObject])
montageGetBy pool buck key target = fmap snd (montageGetBy' pool buck key target)

-- | Retrieve zero or more 'MontageObjects's through an intermediate lookup, but also give the intermediate 'Key' that was made if possible.
montageGetBy' :: MontagePool -> Bucket -> Key -> [Bucket] -> IO (Maybe MontageObject, [Maybe MontageObject])
montageGetBy' pool buck key target = do
    res <- montageRequest pool MONTAGE_GET_REFERENCE (messagePut $ MontageGetReference buck key (Seq.fromList target))
    case ME.mtype res of
        MONTAGE_GET_RESPONSE -> return (MGR.master mo, values)
          where
            values = zipGets (LL.toList . MGR.status $ mo) (LL.toList . MGR.subs $ mo)
            mo = messageGetError "montageGetReference: MontageGetResponse" $ ME.msg res
        MONTAGE_ERROR        -> formatThrow "Incorrect response to MontageGetReference: error={}, bucket={}, key={}" (Shown $ MErr.error msg, Shown buck, Shown key) >> return (Nothing, [])
          where msg = messageGetError "montageGet: MontageGetResponse" $ ME.msg res
        _                  -> formatThrow "Unknown response to MontageGetReference" () >> return (Nothing, [])

-- | Delete whatever is stored at 'Bucket' and 'Key'.
montageDelete :: MontagePool -> Bucket -> Key -> IO ()
montageDelete pool buck key = do
    res <- montageRequest pool MONTAGE_DELETE (messagePut $ MontageDelete buck key)
    case ME.mtype res of
        MONTAGE_DELETE_RESPONSE -> return ()
        MONTAGE_ERROR        -> formatThrow "Incorrect response to MontageDelete: error={}, bucket={}, key={}" (Shown $ MErr.error msg, Shown buck, Shown key) >> return ()
          where msg = messageGetError "montageDelete: MontageDeleteResponse" $ ME.msg res
        _                  -> formatThrow "Unknown response to MontageDelete" () >> return ()

zipPuts :: [MontageObject] -> [MontageObject] -> [Maybe MontageObject]
zipPuts reqs objs = reverse $ snd $ foldl honest (objs, []) reqs
  where
    honest ([], accum) req = ([], Nothing:accum)
    honest (o:os, accum) req = case ((MO.bucket req, MO.key req) == (MO.bucket o, MO.key o)) of
        True -> (os, (Just o:accum))
        False -> (os, Nothing:accum)

-- | Put data, wrapped in a 'MontageObject', at the 'Bucket' and 'Key' specified in that 'MontageObject'.
montagePut :: MontagePool -> MontageObject -> IO (Maybe MontageObject)
montagePut pool os = do
    res <- montageRequest pool MONTAGE_PUT (messagePut $ MontagePut os)
    case ME.mtype res of
        MONTAGE_PUT_RESPONSE -> return $ Just $ MPR.object $ messageGetError "montagePut: put" $ ME.msg res
        MONTAGE_ERROR        -> formatThrow "Incorrect response to MontagePutMany: error={}" (Only $ uToString $ MErr.error msg) >> return Nothing
          where msg = messageGetError "montagePut: MontagePutResponse" $ ME.msg res
        _ -> formatThrow "Unknown response to MontagePut" () >> return Nothing

-- | Put data, wrapped in 'MontageObject's, at the 'Bucket's and 'Key's specified in the 'MontageObject's.
montagePutMany :: MontagePool -> [MontageObject] -> IO ([Maybe MontageObject])
montagePutMany pool os = do
    res <- montageRequest pool MONTAGE_PUT_MANY (messagePut $ MontagePutMany $ Seq.fromList os)
    case ME.mtype res of
        MONTAGE_PUT_MANY_RESPONSE -> return $ zipPuts os objsPut
          where objsPut = LL.map MPR.object $ MPMR.objects $ messageGetError "montagePutMany: put" $ ME.msg res
        MONTAGE_ERROR        -> formatThrow "Incorrect response to MontagePutMany: error={}" (Only $ uToString $ MErr.error msg) >> return []
          where msg = messageGetError "montagePutMany: MontageGetResponse" $ ME.msg res
        _ -> formatThrow "Unknown response to MontagePutMany" () >> return []
