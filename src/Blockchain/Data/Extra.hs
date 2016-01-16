
module Blockchain.Data.Extra (
     getBestProcessedStateRoot,
     getBestBlockId,
     putBestBlockId,
    ) where

import qualified Database.Persist.Sql as SQL

import Blockchain.Data.DataDefs
import qualified Blockchain.Database.MerklePatricia as MP
import Blockchain.DB.SQLDB

getBestBlockId::HasSQLDB m=>
                m (BlockId, Integer)
getBestBlockId = do
  s <- sqlQuery $ SQL.getJust (ExtraKey "bestBlock")
  return $ read $ extraValue s

getBestProcessedStateRoot::HasSQLDB m =>
                           m (MP.SHAPtr, Integer)
getBestProcessedStateRoot = do
  (bid, n) <- getBestBlockId
  b <- sqlQuery $ SQL.getJust bid
  return (blockDataStateRoot $ blockBlockData b, n)

putBestBlockId::HasSQLDB m=>
                BlockId ->Integer->m ()
putBestBlockId bid bestNumber = do
  _ <- sqlQuery $ SQL.upsert (Extra "bestBlock" $ show (bid, bestNumber)) []
  return ()
