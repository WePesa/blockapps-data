{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Blockchain.Data.BlockQueries where

import qualified Database.Esqueleto as E 

import qualified Data.Text as T 

import Control.Monad.Trans.Resource

import Blockchain.DB.SQLDB
import Blockchain.Data.DataDefs
import Blockchain.Data.BlockFilters

import GHC.Int

type BlockIndex = Integer
type BlockFetchLimit = Int64

genBlockQuery :: (HasSQLDB m, MonadResource m, MonadBaseControl IO m) 
              => [(T.Text,T.Text)]
              -> BlockIndex
              -> BlockFetchLimit 
              -> m [E.Entity Block]
genBlockQuery params index' limit = do
                   sqlQuery $ E.select $
                                E.from $ \(bdRef `E.InnerJoin` blk `E.LeftOuterJoin` btx `E.FullOuterJoin` rawTX `E.LeftOuterJoin` accStateRef) -> do

                                E.on ( accStateRef E.^. AddressStateRefAddress E.==. rawTX E.^. RawTransactionFromAddress )
                                E.on ( rawTX E.^. RawTransactionId E.==. btx E.^. BlockTransactionTransaction )
                                E.on ( btx E.^. BlockTransactionBlockId E.==. blk E.^. BlockId )
                                E.on ( blk E.^. BlockId E.==. bdRef E.^. BlockDataRefBlockId )

                                let criteria = map (getBlkFilter (bdRef, accStateRef, rawTX, blk)) $ params
                                    allCriteria = ((bdRef E.^. BlockDataRefNumber) E.>=. E.val index') : criteria

                                E.where_ (foldl1 (E.&&.) allCriteria)

                                E.limit $ limit
                                E.orderBy [E.asc (bdRef E.^. BlockDataRefNumber)]

                                return blk


