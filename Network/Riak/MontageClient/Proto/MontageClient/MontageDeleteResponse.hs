{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Network.Riak.MontageClient.Proto.MontageClient.MontageDeleteResponse (MontageDeleteResponse(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data MontageDeleteResponse = MontageDeleteResponse{}
                           deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable MontageDeleteResponse where
  mergeAppend MontageDeleteResponse MontageDeleteResponse = MontageDeleteResponse
 
instance P'.Default MontageDeleteResponse where
  defaultValue = MontageDeleteResponse
 
instance P'.Wire MontageDeleteResponse where
  wireSize ft' self'@(MontageDeleteResponse)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = 0
  wirePut ft' self'@(MontageDeleteResponse)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             Prelude'.return ()
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> MontageDeleteResponse) MontageDeleteResponse where
  getVal m' f' = f' m'
 
instance P'.GPB MontageDeleteResponse
 
instance P'.ReflectDescriptor MontageDeleteResponse where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".MontageClient.MontageDeleteResponse\", haskellPrefix = [MName \"Network\",MName \"Riak\",MName \"MontageClient\",MName \"Proto\"], parentModule = [MName \"MontageClient\"], baseName = MName \"MontageDeleteResponse\"}, descFilePath = [\"Network\",\"Riak\",\"MontageClient\",\"Proto\",\"MontageClient\",\"MontageDeleteResponse.hs\"], isGroup = False, fields = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"