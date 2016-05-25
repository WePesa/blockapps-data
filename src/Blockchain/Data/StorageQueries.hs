{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Blockchain.Data.StorageQueries where

import qualified Database.Esqueleto as E 

import qualified Data.Text as T 

import Control.Monad.Trans.Resource

import Blockchain.DB.SQLDB
import Blockchain.Data.DataDefs
import Blockchain.Data.StorageFilters

import GHC.Int

type StorageIndex = Int64
type StorageFetchLimit = Int64

genStorageQuery :: (HasSQLDB m, MonadResource m, MonadBaseControl IO m) 
                => [(T.Text,T.Text)]
--                -> StorageIndex
                -> StorageFetchLimit 
                -> m [E.Entity Storage]
genStorageQuery params {- index' -} limit = do
    sqlQuery $ E.select . E.distinct $
        E.from $ \(storage `E.InnerJoin` addrStRef) -> do
            E.on ( storage E.^. StorageAddressStateRefId E.==. addrStRef E.^. AddressStateRefId )
            
            E.where_ ((foldl1 (E.&&.) $ map (getStorageFilter (storage,addrStRef)) $ params ))

            E.limit $ limit

            E.orderBy [E.asc (storage E.^. StorageKey)]

            return storage
