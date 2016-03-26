
module Blockchain.Data.BlockOffset (
  putBlockOffsets
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
