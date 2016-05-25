{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Blockchain.Data.TransactionQueries where

import qualified Database.Esqueleto as E 

import qualified Data.Text as T 

import Control.Monad.Trans.Resource

import Blockchain.DB.SQLDB
import Blockchain.Data.DataDefs
import Blockchain.Data.TransactionFilters
import Blockchain.Data.Params

import GHC.Int

type TransactionIndex = Int
type TransactionFetchLimit = Int64
type TransactionSortParam = Maybe T.Text

genTransactionQuery :: (HasSQLDB m, MonadResource m, MonadBaseControl IO m) 
                    => [(T.Text,T.Text)]
                    -> TransactionIndex
                    -> TransactionFetchLimit 
                    -> TransactionSortParam
                    -> m [E.Entity RawTransaction]
genTransactionQuery params index' limit sortParam = do
     sqlQuery $ E.select $ 
                    E.from $ \(rawTx) -> do
                        E.where_ ((foldl1 (E.&&.) $ map (getTransFilter (rawTx)) $ params ))

                        let criteria = map (getTransFilter rawTx) $ params
                            allCriteria = ((rawTx E.^. RawTransactionBlockNumber) E.>=. E.val index') : criteria

                        -- FIXME: if more than `limit` transactions per block, we will need to have a tuple as index                                                                                                   
                        E.where_ (foldl1 (E.&&.) allCriteria)

                        E.limit $ (limit)
                        E.orderBy $ [(sortToOrderBy sortParam) $ (rawTx E.^. RawTransactionBlockNumber),
                                     (sortToOrderBy sortParam) $ (rawTx E.^. RawTransactionNonce)]

                        return rawTx
