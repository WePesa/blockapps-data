
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
import qualified Database.LevelDB as DB

import qualified Data.NibbleString as N
import Blockchain.Data.Address
import Blockchain.Data.AddressStateDB
import Blockchain.Data.RLP
import Blockchain.DB.AddressStateDB
import Blockchain.DB.HashDB
import Blockchain.DB.StateDB
import qualified Blockchain.Database.MerklePatricia as MP
import qualified Blockchain.Database.MerklePatricia.Internal as MP
import Blockchain.ExtWord

class MonadResource m=>
      HasStorageDB m where
  getStorageDB::Monad m=>m DB.DB

putStorageKeyVal'::(HasStorageDB m, HasStateDB m, HasHashDB m)=>
                  Address->Word256->Word256->m ()
putStorageKeyVal' owner key val = do
  hashDBPut storageKeyNibbles
  addressState <- getAddressState owner
  db <- getStorageDB
  let mpdb = MP.MPDB{MP.ldb=db, MP.stateRoot=addressStateContractRoot addressState}
  newContractRoot <- fmap MP.stateRoot $ MP.putKeyVal mpdb storageKeyNibbles (rlpEncode $ rlpSerialize $ rlpEncode $ toInteger val)
  putAddressState owner addressState{addressStateContractRoot=newContractRoot}
  where storageKeyNibbles = N.pack $ (N.byte2Nibbles =<<) $ word256ToBytes key

deleteStorageKey'::(HasStorageDB m, HasStateDB m, HasHashDB m)=>
                   Address->Word256->m ()
deleteStorageKey' owner key = do
  addressState <- getAddressState owner
  db <- getStorageDB
  let mpdb = MP.MPDB{MP.ldb=db, MP.stateRoot=addressStateContractRoot addressState}
  newContractRoot <- fmap MP.stateRoot $ MP.deleteKey mpdb (N.pack $ (N.byte2Nibbles =<<) $ word256ToBytes key)
  putAddressState owner addressState{addressStateContractRoot=newContractRoot}

getStorageKeyVal'::(HasStorageDB m, HasStateDB m, HasHashDB m)=>
                   Address->Word256->m Word256
getStorageKeyVal' owner key = do
  addressState <- getAddressState owner
  db <- getStorageDB
  let mpdb = MP.MPDB{MP.ldb=db, MP.stateRoot=addressStateContractRoot addressState}
  maybeVal <- MP.getKeyVal mpdb (N.pack $ (N.byte2Nibbles =<<) $ word256ToBytes key)
  case maybeVal of
    Nothing -> return 0
    Just x -> return $ fromInteger $ rlpDecode $ rlpDeserialize $ rlpDecode x

getAllStorageKeyVals'::(HasStorageDB m, HasStateDB m, HasHashDB m)=>
                       Address->m [(MP.Key, Word256)]
getAllStorageKeyVals' owner = do
  addressState <- getAddressState owner
  db <- getStorageDB
  let mpdb = MP.MPDB{MP.ldb=db, MP.stateRoot=addressStateContractRoot addressState}
  kvs <- MP.unsafeGetAllKeyVals mpdb
  return $ map (fmap $ fromInteger . rlpDecode . rlpDeserialize . rlpDecode) kvs
