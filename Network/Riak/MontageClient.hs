{-# LANGUAGE OverloadedStrings #-}
module Network.Riak.MontageClient where

import System.UUID.V4 (uuid)
import System.Timeout (timeout)
import System.ZMQ as ZMQ
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
import qualified Data.ByteString.Lazy.Char8 as B

import Text.ProtocolBuffers.Basic (toUtf8, uToString, Utf8)
import Text.ProtocolBuffers.WireMessage (messageGet, messagePut, Wire)
import Text.ProtocolBuffers.Reflections (ReflectDescriptor)

import Network.Riak.Montage.Util
import Network.Riak.Montage.Proto.Montage.MontageWireMessages
import Network.Riak.Montage.Proto.Montage.MontageObject
import Network.Riak.Montage.Proto.Montage.MontageEnvelope as ME
import Network.Riak.Montage.Proto.Montage.MontageGet
import Network.Riak.Montage.Proto.Montage.MontagePut
import Network.Riak.Montage.Proto.Montage.MontageGetMany
import Network.Riak.Montage.Proto.Montage.MontageGetReference
import Network.Riak.Montage.Proto.Montage.MontageGetResponse as MGR
import Network.Riak.Montage.Proto.Montage.MontagePutResponse as MPR
import Network.Riak.Montage.Proto.Montage.MontagePutManyResponse as MPMR
import Network.Riak.Montage.Proto.Montage.MontagePutMany
import Network.Riak.Montage.Proto.Montage.MontageDelete

import Network.Riak.Montage.Proto.Montage.MontageCommandResponse as MCR
import qualified Network.Riak.Montage.Proto.Montage.MontageError as MErr

type MontagePool = Pool (ZMQ.Socket Req)

-- utils

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

montageGet :: MontagePool -> L.ByteString -> L.ByteString -> IO (Maybe MontageObject)
montageGet pool buck key = do
    res <- montageRequest pool MONTAGE_GET (messagePut $ MontageGet buck key Nothing)
    case ME.mtype res of
        MONTAGE_GET_RESPONSE -> return $ MGR.master $ messageGetError "montageGet: MontageGetResponse" $ ME.msg res
        MONTAGE_ERROR        -> formatThrow "Incorrect response to MontageGet: error={}, bucket={}, key={}" (Shown $ MErr.error msg, Shown buck, Shown key) >> return Nothing
          where msg = messageGetError "montageGet: MontageGetResponse" $ ME.msg res
        _                  -> formatThrow "Unknown response to MontageGet" () >> return Nothing

montageGetMany :: MontagePool -> [(L.ByteString, L.ByteString)] -> IO (Maybe [MontageObject])
montageGetMany pool gets = do
    let gets' = Seq.fromList [ MontageGet buck key Nothing | (buck, key) <- gets ]
    res <- montageRequest pool MONTAGE_GET_MANY (messagePut $ MontageGetMany gets')
    case ME.mtype res of
        MONTAGE_GET_RESPONSE -> return $ Just $ LL.toList $ MGR.subs $ messageGetError "montageGetMany: MontageGetResponse" $ ME.msg res
        MONTAGE_ERROR        -> formatThrow "Incorrect response to MontageGetMany: error={}" (Only $ Shown $ MErr.error msg) >> return Nothing
          where msg = messageGetError "montageGetMany: MontageGetResponse" $ ME.msg res
        _                  -> formatThrow "Unknown response to MontageGetMany" () >> return Nothing

montageGetBy :: MontagePool -> L.ByteString -> L.ByteString -> [L.ByteString] -> IO (Maybe [MontageObject])
montageGetBy pool buck key target = fmap snd (montageGetBy' pool buck key target)

montageGetBy' :: MontagePool -> L.ByteString -> L.ByteString -> [L.ByteString] -> IO (Maybe MontageObject, Maybe [MontageObject])
montageGetBy' pool buck key target = do
    res <- montageRequest pool MONTAGE_GET_REFERENCE (messagePut $ MontageGetReference buck key (Seq.fromList target))
    case ME.mtype res of
        MONTAGE_GET_RESPONSE -> return (MGR.master mo, Just $ LL.toList $ MGR.subs mo)
          where mo = messageGetError "montageGetReference: MontageGetResponse" $ ME.msg res
        MONTAGE_ERROR        -> formatThrow "Incorrect response to MontageGetReference: error={}, bucket={}, key={}" (Shown $ MErr.error msg, Shown buck, Shown key) >> return (Nothing, Nothing)
          where msg = messageGetError "montageGet: MontageGetResponse" $ ME.msg res
        _                  -> formatThrow "Unknown response to MontageGetReference" () >> return (Nothing, Nothing)

montageDelete :: MontagePool -> L.ByteString -> L.ByteString -> IO ()
montageDelete pool buck key = do
    res <- montageRequest pool MONTAGE_DELETE (messagePut $ MontageDelete buck key)
    case ME.mtype res of
        MONTAGE_DELETE_RESPONSE -> return ()
        MONTAGE_ERROR        -> formatThrow "Incorrect response to MontageDelete: error={}, bucket={}, key={}" (Shown $ MErr.error msg, Shown buck, Shown key) >> return ()
          where msg = messageGetError "montageDelete: MontageDeleteResponse" $ ME.msg res
        _                  -> formatThrow "Unknown response to MontageDelete" () >> return ()

montagePut :: MontagePool -> MontageObject -> IO (Maybe MontageObject)
montagePut pool os = do
    res <- montageRequest pool MONTAGE_PUT (messagePut $ MontagePut os)
    case ME.mtype res of
        MONTAGE_PUT_RESPONSE -> return $ Just $ MPR.object $ messageGetError "montagePut: put" $ ME.msg res
        MONTAGE_ERROR        -> formatThrow "Incorrect response to MontagePutMany: error={}" (Only $ uToString $ MErr.error msg) >> return Nothing
          where msg = messageGetError "montagePut: MontagePutResponse" $ ME.msg res
        _ -> formatThrow "Unknown response to MontagePut" () >> return Nothing

montagePutMany :: MontagePool -> [MontageObject] -> IO (Maybe [MontageObject])
montagePutMany pool os = do
    res <- montageRequest pool MONTAGE_PUT_MANY (messagePut $ MontagePutMany $ Seq.fromList os)
    case ME.mtype res of
        MONTAGE_PUT_MANY_RESPONSE -> return $ Just $ LL.map MPR.object $ MPMR.objects $ messageGetError "montagePutMany: put" $ ME.msg res
        MONTAGE_ERROR        -> formatThrow "Incorrect response to MontagePutMany: error={}" (Only $ uToString $ MErr.error msg) >> return Nothing
          where msg = messageGetError "montagePutMany: MontageGetResponse" $ ME.msg res
        _ -> formatThrow "Unknown response to MontagePutMany" () >> return Nothing
