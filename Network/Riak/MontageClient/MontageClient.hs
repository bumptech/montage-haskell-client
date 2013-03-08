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
       -- * Commands
       , montageCommand
       )
       where

import System.UUID.V4 (uuid)
import System.Timeout (timeout)
import System.ZMQ as ZMQ
import Control.Monad.Error (throwError, strMsg, Error, MonadError)
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

import Text.ProtocolBuffers.Basic (uToString, Utf8)
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
import Network.Riak.MontageClient.Proto.MontageClient.MontageCommand
import Network.Riak.MontageClient.Proto.MontageClient.MontageCommandResponse

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
        Right (mes, x) -> case B.length x == 0 of
            True -> mes
            False -> error ("error decoding " ++ B.unpack cls ++ ": did not consume all of input")
        Left e ->
            error ("error decoding " ++ B.unpack cls ++ ": " ++ e)

--- client

sixtySecs :: Int
sixtySecs = 60 * 1000 * 1000

montageRpc :: MontagePool -> L.ByteString -> IO MontageEnvelope
montageRpc pool req = (return . messageGetError "MontageEnvelop" . sTl) =<< rpc
  where
    rpc =   withResource pool (\sock -> do
                ZMQ.send' sock req []
                ZMQ.receive sock []
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
-- Requests to and responses from the Montage riak resolution proxy are wrapped in 'MontageObject's.  A 'MontageObject' contains information about the target\/source ('Bucket' and 'Key'), the serialized data to store\/find, and a riak vector clock (which is originally 'Nothing').
--
-- You must have resolutions defined in your running proxy (see <https://github.com/bumptech/montage/blob/master/examples>) for each of the datatypes you are serializing and placing inside a 'MontageObject'.
--
-- For @montageGetBy@ to complete its intermediate lookup, you must have @referenceKey@ defined for each of your datatypes in your running proxy.

-- | Retrieve a 'MontageObject' from a 'Bucket' and 'Key', if data exists at that source.  Montage itself resolves any siblings.
montageGet :: MontagePool -> Bucket -> Key -> IO (Maybe MontageObject)
montageGet pool buck key' = do
    res <- montageRequest pool MONTAGE_GET (messagePut $ MontageGet buck key' Nothing)
    case ME.mtype res of
        MONTAGE_GET_RESPONSE -> return $ MGR.master $ messageGetError "montageGet: MontageGetResponse" $ ME.msg res
        MONTAGE_ERROR        -> formatThrow "Incorrect response to MontageGet: error={}, bucket={}, key={}" (Shown $ MErr.error mes, Shown buck, Shown key') >> return Nothing
          where mes = messageGetError "montageGet: MontageGetResponse" $ ME.msg res
        _                  -> formatThrow "Unknown response to MontageGet" () >> return Nothing

zipGets :: [MontageGetStatus] -> [MontageObject] -> [Maybe MontageObject]
zipGets ss objs = reverse $ snd $ foldl honest (objs, []) ss
  where
    honest (os, accum) status' = case status' of
          EXISTS -> honest' os accum
          MISSING -> (os, Nothing:accum)
          ERROR -> error "Status ERROR in get response"

    honest' [] _ = error "Status EXISTS when no more responses"
    honest' (o:os) accum = (os, (Just o:accum))

-- | Retrieve many 'MontageObject's from a list of 'Bucket's and 'Key's, if data exists at those sources.  Responses are ordered the same as the 'Bucket's and 'Key's.
montageGetMany :: MontagePool -> [(Bucket, Key)] -> IO ([Maybe MontageObject])
montageGetMany pool gets' = do
    let gets'' = Seq.fromList [ MontageGet buck key' Nothing | (buck, key') <- gets' ]
    res <- montageRequest pool MONTAGE_GET_MANY (messagePut $ MontageGetMany gets'')
    case ME.mtype res of
        MONTAGE_GET_RESPONSE ->
            return $ zipGets (LL.toList . MGR.status $ mgr) (LL.toList . MGR.subs $ mgr)
          where mgr = messageGetError "montageGetMany: MontageGetResponse" $ ME.msg res
        MONTAGE_ERROR        -> formatThrow "Incorrect response to MontageGetMany: error={}" (Only $ Shown $ MErr.error mes) >> return []
          where mes = messageGetError "montageGetMany: MontageGetResponse" $ ME.msg res
        _                  -> formatThrow "Unknown response to MontageGetMany" () >> return []

-- | Retrieve zero or more 'MontageObject's through an intermediate lookup.
--
-- The first lookup will use the 'ReferenceBucket' and 'Key' to retreive a 'MontageObject. An implementation of @referenceKey@ (in the instances you define for the Montage proxy) extracts from this 'MontageObject' a sensible 'Key' that can be used for further lookups. @montageGetBy@ uses the intermediate 'Key' and each target 'Bucket' to retrieve 'MontageObject's. If a sensible intermediate 'Key' cannot be made, then @montageGetBy@ reacts the same as @montageGetMany@ when nothing can be found.
montageGetBy :: MontagePool -> ReferenceBucket -> Key -> [Bucket] -> IO ([Maybe MontageObject])
montageGetBy pool buck key' target = fmap snd (montageGetBy' pool buck key' target)

-- | Retrieve zero or more 'MontageObjects's through an intermediate lookup, but also give the intermediate 'Key' that was made if possible.
montageGetBy' :: MontagePool -> ReferenceBucket -> Key -> [Bucket] -> IO (Maybe MontageObject, [Maybe MontageObject])
montageGetBy' pool buck key' target = do
    res <- montageRequest pool MONTAGE_GET_REFERENCE (messagePut $ MontageGetReference buck key' (Seq.fromList target))
    case ME.mtype res of
        MONTAGE_GET_RESPONSE -> return (MGR.master mo, values)
          where
            values = zipGets (LL.toList . MGR.status $ mo) (LL.toList . MGR.subs $ mo)
            mo = messageGetError "montageGetReference: MontageGetResponse" $ ME.msg res
        MONTAGE_ERROR        -> formatThrow "Incorrect response to MontageGetReference: error={}, bucket={}, key={}" (Shown $ MErr.error mes, Shown buck, Shown key') >> return (Nothing, [])
          where mes = messageGetError "montageGet: MontageGetResponse" $ ME.msg res
        _                  -> formatThrow "Unknown response to MontageGetReference" () >> return (Nothing, [])

-- | Delete whatever is stored at 'Bucket' and 'Key'.
montageDelete :: MontagePool -> Bucket -> Key -> IO ()
montageDelete pool buck key' = do
    res <- montageRequest pool MONTAGE_DELETE (messagePut $ MontageDelete buck key')
    case ME.mtype res of
        MONTAGE_DELETE_RESPONSE -> return ()
        MONTAGE_ERROR        -> formatThrow "Incorrect response to MontageDelete: error={}, bucket={}, key={}" (Shown $ MErr.error mes, Shown buck, Shown key') >> return ()
          where mes = messageGetError "montageDelete: MontageDeleteResponse" $ ME.msg res
        _                  -> formatThrow "Unknown response to MontageDelete" () >> return ()

zipPuts :: [MontageObject] -> [MontageObject] -> [Maybe MontageObject]
zipPuts reqs objs = reverse $ snd $ foldl honest (objs, []) reqs
  where
    honest ([], accum) _ = ([], Nothing:accum)
    honest (o:os, accum) req = case ((MO.bucket req, MO.key req) == (MO.bucket o, MO.key o)) of
        True -> (os, (Just o:accum))
        False -> (os, Nothing:accum)

-- | Put data, wrapped in a 'MontageObject', at the 'Bucket' and 'Key' specified in that 'MontageObject'.
montagePut :: MontagePool -> MontageObject -> IO (Maybe MontageObject)
montagePut pool os = do
    res <- montageRequest pool MONTAGE_PUT (messagePut $ MontagePut os)
    case ME.mtype res of
        MONTAGE_PUT_RESPONSE -> return $ Just $ MPR.object $ messageGetError "montagePut: put" $ ME.msg res
        MONTAGE_ERROR        -> formatThrow "Incorrect response to MontagePutMany: error={}" (Only $ uToString $ MErr.error mes) >> return Nothing
          where mes = messageGetError "montagePut: MontagePutResponse" $ ME.msg res
        _ -> formatThrow "Unknown response to MontagePut" () >> return Nothing

-- | Put data, wrapped in 'MontageObject's, at the 'Bucket's and 'Key's specified in the 'MontageObject's.
montagePutMany :: MontagePool -> [MontageObject] -> IO ([Maybe MontageObject])
montagePutMany pool os = do
    res <- montageRequest pool MONTAGE_PUT_MANY (messagePut $ MontagePutMany $ Seq.fromList os)
    case ME.mtype res of
        MONTAGE_PUT_MANY_RESPONSE -> return $ zipPuts os objsPut
          where objsPut = LL.map MPR.object $ MPMR.objects $ messageGetError "montagePutMany: put" $ ME.msg res
        MONTAGE_ERROR        -> formatThrow "Incorrect response to MontagePutMany: error={}" (Only $ uToString $ MErr.error mes) >> return []
          where mes = messageGetError "montagePutMany: MontageGetResponse" $ ME.msg res
        _ -> formatThrow "Unknown response to MontagePutMany" () >> return []

montageCommand :: MontagePool -> Utf8 -> Maybe L.ByteString -> IO (Maybe MontageCommandResponse)
montageCommand pool cmd args = do
    res <- montageRequest pool MONTAGE_COMMAND (messagePut $ MontageCommand cmd args)
    case ME.mtype res of
        MONTAGE_COMMAND_RESPONSE -> return $ Just $ messageGetError "MontageCommandResponse" $ ME.msg res
        MONTAGE_ERROR            -> do
            formatThrow "Incorrect response to MontageCommand: error={}" $ Only $ uToString $ MErr.error $ messageGetError "MontageError: MontageCommand" $ ME.msg res
            return Nothing
        _ -> formatThrow "Unknown response to MontageCommand" () >> return Nothing
