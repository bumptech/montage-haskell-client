{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Network.Riak.MontageClient.Proto.MontageClient.MontagePut (MontagePut(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Network.Riak.MontageClient.Proto.MontageClient.MontageObject as MontageClient (MontageObject)
 
data MontagePut = MontagePut{object :: !MontageClient.MontageObject}
                deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable MontagePut where
  mergeAppend (MontagePut x'1) (MontagePut y'1) = MontagePut (P'.mergeAppend x'1 y'1)
 
instance P'.Default MontagePut where
  defaultValue = MontagePut P'.defaultValue
 
instance P'.Wire MontagePut where
  wireSize ft' self'@(MontagePut x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 11 x'1)
  wirePut ft' self'@(MontagePut x'1)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 10 11 x'1
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{object = P'.mergeAppend (object old'Self) (new'Field)}) (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> MontagePut) MontagePut where
  getVal m' f' = f' m'
 
instance P'.GPB MontagePut
 
instance P'.ReflectDescriptor MontagePut where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [10]) (P'.fromDistinctAscList [10])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".MontageClient.MontagePut\", haskellPrefix = [MName \"Network\",MName \"Riak\",MName \"MontageClient\",MName \"Proto\"], parentModule = [MName \"MontageClient\"], baseName = MName \"MontagePut\"}, descFilePath = [\"Network\",\"Riak\",\"MontageClient\",\"Proto\",\"MontageClient\",\"MontagePut.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".MontageClient.MontagePut.object\", haskellPrefix' = [MName \"Network\",MName \"Riak\",MName \"MontageClient\",MName \"Proto\"], parentModule' = [MName \"MontageClient\",MName \"MontagePut\"], baseName' = FName \"object\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".MontageClient.MontageObject\", haskellPrefix = [MName \"Network\",MName \"Riak\",MName \"MontageClient\",MName \"Proto\"], parentModule = [MName \"MontageClient\"], baseName = MName \"MontageObject\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"