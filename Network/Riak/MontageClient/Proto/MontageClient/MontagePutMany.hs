{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Network.Riak.MontageClient.Proto.MontageClient.MontagePutMany (MontagePutMany(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Network.Riak.MontageClient.Proto.MontageClient.MontageObject as MontageClient (MontageObject)
 
data MontagePutMany = MontagePutMany{objects :: !(P'.Seq MontageClient.MontageObject)}
                    deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable MontagePutMany where
  mergeAppend (MontagePutMany x'1) (MontagePutMany y'1) = MontagePutMany (P'.mergeAppend x'1 y'1)
 
instance P'.Default MontagePutMany where
  defaultValue = MontagePutMany P'.defaultValue
 
instance P'.Wire MontagePutMany where
  wireSize ft' self'@(MontagePutMany x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeRep 1 11 x'1)
  wirePut ft' self'@(MontagePutMany x'1)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutRep 10 11 x'1
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{objects = P'.append (objects old'Self) new'Field}) (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> MontagePutMany) MontagePutMany where
  getVal m' f' = f' m'
 
instance P'.GPB MontagePutMany
 
instance P'.ReflectDescriptor MontagePutMany where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".MontageClient.MontagePutMany\", haskellPrefix = [MName \"Network\",MName \"Riak\",MName \"MontageClient\",MName \"Proto\"], parentModule = [MName \"MontageClient\"], baseName = MName \"MontagePutMany\"}, descFilePath = [\"Network\",\"Riak\",\"MontageClient\",\"Proto\",\"MontageClient\",\"MontagePutMany.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".MontageClient.MontagePutMany.objects\", haskellPrefix' = [MName \"Network\",MName \"Riak\",MName \"MontageClient\",MName \"Proto\"], parentModule' = [MName \"MontageClient\",MName \"MontagePutMany\"], baseName' = FName \"objects\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".MontageClient.MontageObject\", haskellPrefix = [MName \"Network\",MName \"Riak\",MName \"MontageClient\",MName \"Proto\"], parentModule = [MName \"MontageClient\"], baseName = MName \"MontageObject\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"