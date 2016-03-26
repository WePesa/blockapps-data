
module Blockchain.Data.BlockOffset (
  putBlockOffsets,
  getBlockOffsetsForNumber
  ) where

import Control.Monad
import Control.Monad.Trans.Resource
import qualified Database.Persist.Postgresql as SQL
import Blockchain.Data.DataDefs

import Blockchain.DB.SQLDB

putBlockOffsets::HasSQLDB m=>[BlockOffset]->m ()
putBlockOffsets blockOffsets = do
  db <- getSQLDB
  runResourceT $
    flip SQL.runSqlPool db $
    forM blockOffsets $ SQL.insert
  return ()

getBlockOffsetsForNumber::HasSQLDB m=>Integer->m [BlockOffset]
getBlockOffsetsForNumber blockOffset = do
  db <- getSQLDB
  ret <-
    runResourceT $
    flip SQL.runSqlPool db $
    SQL.selectList [BlockOffsetOffset SQL.==. blockOffset] []

  return $ map SQL.entityVal ret
