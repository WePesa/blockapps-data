{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Blockchain.Data.AccountQueries where

import qualified Database.Esqueleto as E 

import qualified Data.Text as T 

import Control.Monad.Trans.Resource

import Blockchain.DB.SQLDB
import Blockchain.Data.DataDefs
import Blockchain.Data.AccountFilters

import GHC.Int

type AccountIndex = Int64
type AccountFetchLimit = Int64

genAccountQuery :: (HasSQLDB m, MonadResource m, MonadBaseControl IO m) 
                => [(T.Text,T.Text)]
                -> AccountIndex
                -> AccountFetchLimit 
                -> m [E.Entity AddressStateRef]
genAccountQuery params index' limit = do
    sqlQuery $ E.select . E.distinct $
        E.from $ \(accStateRef) -> do
            let criteria = map (getAccFilter (accStateRef)) $ params
            let allCriteria = ((accStateRef E.^. AddressStateRefId) E.>=. E.val (E.toSqlKey index')) : criteria

            E.where_ (foldl1 (E.&&.) allCriteria)

            E.limit $ limit

            E.orderBy [E.asc (accStateRef E.^. AddressStateRefId)]
     
            return accStateRef
