
module Blockchain.DB.StorageDB (
  HasStorageDB(..),
  putStorageKeyVal',
  deleteStorageKey',
  getStorageKeyVal',
  getAllStorageKeyVals'
  ) where

import Control.Monad.State
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Resource
import qualified Data.ByteString as B
import Data.Default
import qualified Data.Map as M
import qualified Database.LevelDB as DB

import qualified Data.NibbleString as N
import Blockchain.Data.Address
import Blockchain.Data.AddressStateDB
import Blockchain.Data.RLP
import Blockchain.DB.MemAddressStateDB
import Blockchain.DB.HashDB
import Blockchain.DB.StateDB
import qualified Blockchain.Database.MerklePatricia as MP
import qualified Blockchain.Database.MerklePatricia.Internal as MP
import Blockchain.ExtWord
  
class MonadResource m=>
      HasStorageDB m where
  getStorageDB::Monad m=>m (DB.DB, M.Map (Address, Word512) Word512)

putStorageKeyVal'::(HasMemAddressStateDB m, HasStorageDB m, HasStateDB m, HasHashDB m)=>
                  Address->Word256->Word256->m ()
putStorageKeyVal' owner key val = putStorageKeyValMC owner key val

deleteStorageKey'::(HasMemAddressStateDB m, HasStorageDB m, HasStateDB m, HasHashDB m)=>
                   Address->Word256->m ()
deleteStorageKey' owner key = deleteStorageKeyMC owner key

getStorageKeyVal'::(HasMemAddressStateDB m, HasStorageDB m, HasStateDB m, HasHashDB m)=>
                   Address->Word256->m Word256
getStorageKeyVal' owner key = getStorageKeyValMC owner key

getAllStorageKeyVals'::(HasMemAddressStateDB m, HasStorageDB m, HasStateDB m, HasHashDB m)=>
                       Address->m [(MP.Key, Word256)]
getAllStorageKeyVals' owner = getAllStorageKeyValsMC owner






--The following are the memory cache versions of the functions

putStorageKeyValMC::(HasMemAddressStateDB m, HasStorageDB m, HasStateDB m, HasHashDB m)=>
                  Address->Word256->Word256->m ()
putStorageKeyValMC owner key val = putStorageKeyValDB owner key val

deleteStorageKeyMC::(HasMemAddressStateDB m, HasStorageDB m, HasStateDB m, HasHashDB m)=>
                   Address->Word256->m ()
deleteStorageKeyMC owner key = deleteStorageKeyDB owner key

getStorageKeyValMC::(HasMemAddressStateDB m, HasStorageDB m, HasStateDB m, HasHashDB m)=>
                   Address->Word256->m Word256
getStorageKeyValMC owner key = getStorageKeyValDB owner key

getAllStorageKeyValsMC::(HasMemAddressStateDB m, HasStorageDB m, HasStateDB m, HasHashDB m)=>
                       Address->m [(MP.Key, Word256)]
getAllStorageKeyValsMC owner = getAllStorageKeyValsDB owner











--The following are the DB versions of the functions


putStorageKeyValDB::(HasMemAddressStateDB m, HasStorageDB m, HasStateDB m, HasHashDB m)=>
                  Address->Word256->Word256->m ()
putStorageKeyValDB owner key val = do
  hashDBPut storageKeyNibbles
  addressState <- getAddressState owner
  db <- fmap fst getStorageDB
  let mpdb = MP.MPDB{MP.ldb=db, MP.stateRoot=addressStateContractRoot addressState}
  newContractRoot <- fmap MP.stateRoot $ MP.putKeyVal mpdb storageKeyNibbles (rlpEncode $ rlpSerialize $ rlpEncode $ toInteger val)
  putAddressState owner addressState{addressStateContractRoot=newContractRoot}
  where storageKeyNibbles = N.pack $ (N.byte2Nibbles =<<) $ word256ToBytes key

deleteStorageKeyDB::(HasMemAddressStateDB m, HasStorageDB m, HasStateDB m, HasHashDB m)=>
                   Address->Word256->m ()
deleteStorageKeyDB owner key = do
  addressState <- getAddressState owner
  db <- fmap fst getStorageDB
  let mpdb = MP.MPDB{MP.ldb=db, MP.stateRoot=addressStateContractRoot addressState}
  newContractRoot <- fmap MP.stateRoot $ MP.deleteKey mpdb (N.pack $ (N.byte2Nibbles =<<) $ word256ToBytes key)
  putAddressState owner addressState{addressStateContractRoot=newContractRoot}

getStorageKeyValDB::(HasMemAddressStateDB m, HasStorageDB m, HasStateDB m, HasHashDB m)=>
                   Address->Word256->m Word256
getStorageKeyValDB owner key = do
  addressState <- getAddressState owner
  db <- fmap fst getStorageDB
  let mpdb = MP.MPDB{MP.ldb=db, MP.stateRoot=addressStateContractRoot addressState}
  maybeVal <- MP.getKeyVal mpdb (N.pack $ (N.byte2Nibbles =<<) $ word256ToBytes key)
  case maybeVal of
    Nothing -> return 0
    Just x -> return $ fromInteger $ rlpDecode $ rlpDeserialize $ rlpDecode x

getAllStorageKeyValsDB::(HasMemAddressStateDB m, HasStorageDB m, HasStateDB m, HasHashDB m)=>
                       Address->m [(MP.Key, Word256)]
getAllStorageKeyValsDB owner = do
  addressState <- getAddressState owner
  db <- fmap fst getStorageDB
  let mpdb = MP.MPDB{MP.ldb=db, MP.stateRoot=addressStateContractRoot addressState}
  kvs <- MP.unsafeGetAllKeyVals mpdb
  return $ map (fmap $ fromInteger . rlpDecode . rlpDeserialize . rlpDecode) kvs
